module Math.Optimization.Accelerate.BinarySpec where

import qualified Data.Array.Accelerate         as A
import qualified Data.Array.Accelerate.Interpreter
                                               as AI
import           Math.Optimization.Accelerate.Binary
                                                ( reverseBitsToFrac
                                                , reverseBitsToInt
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
        describe "reverseBitsToFrac" $ do
          let ub' lb x = lb + abs x
          it "returns lower bound for empty" $ property $ \lb x ->
            reverseBitsToFracL lb (ub' lb x) [] == lb
          it "returns lower bound when all False" $ property $ \lb x ->
            let ub = ub' lb x
            in  reverseBitsToFracL lb ub [False]
                  ~= lb
                  && reverseBitsToFracL lb ub [False, False]
                  ~= lb
                  && reverseBitsToFracL lb ub [False, False, False]
                  ~= lb
          it "returns upper bound when all True" $ property $ \lb x ->
            let ub = ub' lb x
            in  reverseBitsToFracL lb ub [True]
                  ~= ub
                  && reverseBitsToFracL lb ub [True, True]
                  ~= ub
                  && reverseBitsToFracL lb ub [True, True, True]
                  ~= ub
          it "returns a number between lower and upper bound when some True"
            $ property
            $ \lb x ->
                let ub       = ub' lb x + 0.001
                    between' = between lb ub
                in  between' (reverseBitsToFracL lb ub [False, True])
                      && between' (reverseBitsToFracL lb ub [True, False])
                      && between' (reverseBitsToFracL lb ub [True, False, True])
          it "monotonically increases as binary increases"
            $ property
            $ \lb x ->
                let ub = ub' lb x + 0.001
                in  reverseBitsToFracL lb ub [True]
                      >  reverseBitsToFracL lb ub [False]
                      && reverseBitsToFracL lb ub [True, False]
                      >  reverseBitsToFracL lb ub [False, False]
                      && reverseBitsToFracL lb ub [False, True]
                      >  reverseBitsToFracL lb ub [True, False]
                      && reverseBitsToFracL lb ub [True, True]
                      >  reverseBitsToFracL lb ub [False, True]
                      && reverseBitsToFracL lb ub [True, False, False]
                      >  reverseBitsToFracL lb ub [False, False, False]
                      && reverseBitsToFracL lb ub [False, True, False]
                      >  reverseBitsToFracL lb ub [True, False, False]
                      && reverseBitsToFracL lb ub [True, True, False]
                      >  reverseBitsToFracL lb ub [False, True, False]
                      && reverseBitsToFracL lb ub [False, False, True]
                      >  reverseBitsToFracL lb ub [True, True, False]
                      && reverseBitsToFracL lb ub [True, False, True]
                      >  reverseBitsToFracL lb ub [False, False, True]
                      && reverseBitsToFracL lb ub [False, True, True]
                      >  reverseBitsToFracL lb ub [True, False, True]
                      && reverseBitsToFracL lb ub [True, True, True]
                      >  reverseBitsToFracL lb ub [False, True, True]
          it "converts each row in a matrix to a number" $ do
            AI.run
                ( reverseBitsToFrac (A.constant 0) (A.constant 3)
                $ A.use
                $ A.fromList (A.Z A.:. 3 A.:. 2)
                             [False, False, True, False, False, True]
                )
              `shouldBe` A.fromList (A.Z A.:. (3 :: Int)) [0 :: Double, 1, 2]

        describe "reverseBitsToInt" $ do
          it "returns 0 when empty" $ do
            reverseBitsToIntL [] `shouldBe` 0
          it "returns the base 10 integer represented by binary bits" $ do
            reverseBitsToIntL [False] `shouldBe` 0
            reverseBitsToIntL [False, False] `shouldBe` 0
            reverseBitsToIntL [False, False, False] `shouldBe` 0
            reverseBitsToIntL [True] `shouldBe` 1
            reverseBitsToIntL [True, True] `shouldBe` 3
            reverseBitsToIntL [True, True, True] `shouldBe` 7
          it "treats leftmost as least significant" $ do
            reverseBitsToIntL [False, True] `shouldBe` 2
            reverseBitsToIntL [False, False, True] `shouldBe` 4
          it "converts each row in a matrix to a number" $ do
            AI.run
                (reverseBitsToInt $ A.use $ A.fromList
                  (A.Z A.:. 3 A.:. 2)
                  [False, False, True, False, False, True]
                )
              `shouldBe` A.fromList (A.Z A.:. (3 :: Int)) [0 :: Int, 1, 2]

reverseBitsToFracL :: Double -> Double -> [Bool] -> Double
reverseBitsToFracL lb ub xs = head $ A.toList $ AI.run $ reverseBitsToFrac
  (A.constant lb)
  (A.constant ub)
  (A.use $ A.fromList (A.Z A.:. length xs) xs)

reverseBitsToIntL :: [Bool] -> Int
reverseBitsToIntL xs = head $ A.toList $ AI.run $ reverseBitsToInt
  (A.use $ A.fromList (A.Z A.:. length xs) xs)

(~=) :: (Floating a, Ord a) => a -> a -> Bool
(~=) = aboutEquals 0.001

aboutEquals :: (Num a, Ord a) => a -> a -> a -> Bool
aboutEquals tol x y = abs (x - y) < tol

between :: Ord a => a -> a -> a -> Bool
between lb ub x = x > lb && x < ub
