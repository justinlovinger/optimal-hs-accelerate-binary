module Math.Optimization.Accelerate.BinarySpec where

import qualified Data.Array.Accelerate         as A
import qualified Data.Array.Accelerate.Interpreter
                                               as AI
import           Math.Optimization.Accelerate.Binary
                                                ( fromBools
                                                , fromBools'
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
        describe "fromBools" $ do
          let ub' lb x = lb + abs x
          it "returns lower bound for empty" $ property $ \lb x ->
            fromBoolsL lb (ub' lb x) [] == lb
          it "returns lower bound when all False" $ property $ \lb x ->
            let ub = ub' lb x
            in  fromBoolsL lb ub [False]
                  ~= lb
                  && fromBoolsL lb ub [False, False]
                  ~= lb
                  && fromBoolsL lb ub [False, False, False]
                  ~= lb
          it "returns upper bound when all True" $ property $ \lb x ->
            let ub = ub' lb x
            in  fromBoolsL lb ub [True]
                  ~= ub
                  && fromBoolsL lb ub [True, True]
                  ~= ub
                  && fromBoolsL lb ub [True, True, True]
                  ~= ub
          it
              "returns a number between lower and upper bound when some True"
            $ property
            $ \lb x ->
                let ub       = ub' lb x + 0.001
                    between' = between lb ub
                in  between' (fromBoolsL lb ub [False, True])
                      && between' (fromBoolsL lb ub [True, False])
                      && between' (fromBoolsL lb ub [True, False, True])
          it "monotonically increases as binary increases"
            $ property
            $ \lb x ->
                let ub = ub' lb x + 0.001
                in  fromBoolsL lb ub [True]
                      >  fromBoolsL lb ub [False]
                      && fromBoolsL lb ub [True, False]
                      >  fromBoolsL lb ub [False, False]
                      && fromBoolsL lb ub [False, True]
                      >  fromBoolsL lb ub [True, False]
                      && fromBoolsL lb ub [True, True]
                      >  fromBoolsL lb ub [False, True]
                      && fromBoolsL lb ub [True, False, False]
                      >  fromBoolsL lb ub [False, False, False]
                      && fromBoolsL lb ub [False, True, False]
                      >  fromBoolsL lb ub [True, False, False]
                      && fromBoolsL lb ub [True, True, False]
                      >  fromBoolsL lb ub [False, True, False]
                      && fromBoolsL lb ub [False, False, True]
                      >  fromBoolsL lb ub [True, True, False]
                      && fromBoolsL lb ub [True, False, True]
                      >  fromBoolsL lb ub [False, False, True]
                      && fromBoolsL lb ub [False, True, True]
                      >  fromBoolsL lb ub [True, False, True]
                      && fromBoolsL lb ub [True, True, True]
                      >  fromBoolsL lb ub [False, True, True]
          it "converts each row in a matrix to a number" $ do
            AI.run
                (fromBools (A.constant 0) (A.constant 3) $ A.use $ A.fromList
                  (A.Z A.:. 3 A.:. 2)
                  [False, False, True, False, False, True]
                )
              `shouldBe` A.fromList (A.Z A.:. (3 :: Int)) [0 :: Double, 1, 2]

        describe "fromBools'" $ do
          it "returns 0 when empty" $ do
            fromBoolsL' [] `shouldBe` 0
          it "returns the base 10 integer represented by binary bits" $ do
            fromBoolsL' [False] `shouldBe` 0
            fromBoolsL' [False, False] `shouldBe` 0
            fromBoolsL' [False, False, False] `shouldBe` 0
            fromBoolsL' [True] `shouldBe` 1
            fromBoolsL' [True, True] `shouldBe` 3
            fromBoolsL' [True, True, True] `shouldBe` 7
          it "treats leftmost as least significant" $ do
            fromBoolsL' [False, True] `shouldBe` 2
            fromBoolsL' [False, False, True] `shouldBe` 4
          it "converts each row in a matrix to a number" $ do
            AI.run
                (fromBools' $ A.use $ A.fromList
                  (A.Z A.:. 3 A.:. 2)
                  [False, False, True, False, False, True]
                )
              `shouldBe` A.fromList (A.Z A.:. (3 :: Int)) [0 :: Int, 1, 2]

fromBoolsL :: Double -> Double -> [Bool] -> Double
fromBoolsL lb ub xs = head $ A.toList $ AI.run $ fromBools
  (A.constant lb)
  (A.constant ub)
  (A.use $ A.fromList (A.Z A.:. length xs) xs)

fromBoolsL' :: [Bool] -> Int
fromBoolsL' xs = head $ A.toList $ AI.run $ fromBools'
  (A.use $ A.fromList (A.Z A.:. length xs) xs)

(~=) :: (Floating a, Ord a) => a -> a -> Bool
(~=) = aboutEquals 0.001

aboutEquals :: (Num a, Ord a) => a -> a -> a -> Bool
aboutEquals tol x y = abs (x - y) < tol

between :: Ord a => a -> a -> a -> Bool
between lb ub x = x > lb && x < ub
