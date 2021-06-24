module Math.Optimization.Accelerate.BinarySpec where

import qualified Data.Array.Accelerate         as A
import qualified Data.Array.Accelerate.Interpreter
                                               as AI
import           Math.Optimization.Accelerate.Binary
                                                ( reversedBitsToFrac
                                                , reversedBitsToInt
                                                )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldBe
                                                )
import           Test.QuickCheck                ( property )

spec :: Spec
spec =
  describe "Math"
    $ describe "Optimization"
    $ describe "Accelerate"
    $ describe "Binary"
    $ do
        describe "reversedBitsToFrac" $ do
          let ub' lb x = lb + abs x
          it "returns lower bound for empty" $ property $ \lb x ->
            reversedBitsToFracL lb (ub' lb x) [] == lb
          it "returns lower bound when all False" $ property $ \lb x ->
            let ub = ub' lb x
            in  reversedBitsToFracL lb ub [False]
                  ~= lb
                  && reversedBitsToFracL lb ub [False, False]
                  ~= lb
                  && reversedBitsToFracL lb ub [False, False, False]
                  ~= lb
          it "returns upper bound when all True" $ property $ \lb x ->
            let ub = ub' lb x
            in  reversedBitsToFracL lb ub [True]
                  ~= ub
                  && reversedBitsToFracL lb ub [True, True]
                  ~= ub
                  && reversedBitsToFracL lb ub [True, True, True]
                  ~= ub
          it "returns a number between lower and upper bound when some True"
            $ property
            $ \lb x ->
                let ub       = ub' lb x + 0.001
                    between' = between lb ub
                in  between' (reversedBitsToFracL lb ub [False, True])
                      && between' (reversedBitsToFracL lb ub [True, False])
                      && between'
                           (reversedBitsToFracL lb ub [True, False, True])
          it "monotonically increases as binary increases"
            $ property
            $ \lb x ->
                let ub = ub' lb x + 0.001
                in  reversedBitsToFracL lb ub [True]
                      >  reversedBitsToFracL lb ub [False]
                      && reversedBitsToFracL lb ub [True, False]
                      >  reversedBitsToFracL lb ub [False, False]
                      && reversedBitsToFracL lb ub [False, True]
                      >  reversedBitsToFracL lb ub [True, False]
                      && reversedBitsToFracL lb ub [True, True]
                      >  reversedBitsToFracL lb ub [False, True]
                      && reversedBitsToFracL lb ub [True, False, False]
                      >  reversedBitsToFracL lb ub [False, False, False]
                      && reversedBitsToFracL lb ub [False, True, False]
                      >  reversedBitsToFracL lb ub [True, False, False]
                      && reversedBitsToFracL lb ub [True, True, False]
                      >  reversedBitsToFracL lb ub [False, True, False]
                      && reversedBitsToFracL lb ub [False, False, True]
                      >  reversedBitsToFracL lb ub [True, True, False]
                      && reversedBitsToFracL lb ub [True, False, True]
                      >  reversedBitsToFracL lb ub [False, False, True]
                      && reversedBitsToFracL lb ub [False, True, True]
                      >  reversedBitsToFracL lb ub [True, False, True]
                      && reversedBitsToFracL lb ub [True, True, True]
                      >  reversedBitsToFracL lb ub [False, True, True]
          it "converts each row in a matrix to a number" $ do
            AI.run
                ( reversedBitsToFrac (A.constant 0) (A.constant 3)
                $ A.use
                $ A.fromList (A.Z A.:. 3 A.:. 2)
                             [False, False, True, False, False, True]
                )
              `shouldBe` A.fromList (A.Z A.:. (3 :: Int)) [0 :: Double, 1, 2]

        describe "reversedBitsToInt" $ do
          it "returns 0 when empty" $ do
            reversedBitsToIntL [] `shouldBe` 0
          it "returns the base 10 integer represented by binary bits" $ do
            reversedBitsToIntL [False] `shouldBe` 0
            reversedBitsToIntL [False, False] `shouldBe` 0
            reversedBitsToIntL [False, False, False] `shouldBe` 0
            reversedBitsToIntL [True] `shouldBe` 1
            reversedBitsToIntL [True, True] `shouldBe` 3
            reversedBitsToIntL [True, True, True] `shouldBe` 7
          it "treats leftmost as least significant" $ do
            reversedBitsToIntL [False, True] `shouldBe` 2
            reversedBitsToIntL [False, False, True] `shouldBe` 4
          it "converts each row in a matrix to a number" $ do
            AI.run
                (reversedBitsToInt $ A.use $ A.fromList
                  (A.Z A.:. 3 A.:. 2)
                  [False, False, True, False, False, True]
                )
              `shouldBe` A.fromList (A.Z A.:. (3 :: Int)) [0 :: Int, 1, 2]

reversedBitsToFracL :: Double -> Double -> [Bool] -> Double
reversedBitsToFracL lb ub xs = head $ A.toList $ AI.run $ reversedBitsToFrac
  (A.constant lb)
  (A.constant ub)
  (A.use $ A.fromList (A.Z A.:. length xs) xs)

reversedBitsToIntL :: [Bool] -> Int
reversedBitsToIntL xs = head $ A.toList $ AI.run $ reversedBitsToInt
  (A.use $ A.fromList (A.Z A.:. length xs) xs)

(~=) :: (Floating a, Ord a) => a -> a -> Bool
(~=) = aboutEquals 0.001

aboutEquals :: (Num a, Ord a) => a -> a -> a -> Bool
aboutEquals tol x y = abs (x - y) < tol

between :: Ord a => a -> a -> a -> Bool
between lb ub x = x > lb && x < ub
