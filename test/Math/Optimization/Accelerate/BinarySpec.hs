module Math.Optimization.Accelerate.BinarySpec where

import qualified Data.Array.Accelerate         as A
import qualified Data.Array.Accelerate.Interpreter
                                               as AI
import           Math.Optimization.Accelerate.Binary
                                                ( fromBits
                                                , fromBits'
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
        describe "fromBits" $ do
          let ub' lb x = lb + abs x
          it "returns lower bound when no bits" $ property $ \lb x ->
            fromBitsL lb (ub' lb x) [] == lb
          it "returns lower bound when all bits are 0" $ property $ \lb x ->
            let ub = ub' lb x
            in  fromBitsL lb ub [False]
                  ~= lb
                  && fromBitsL lb ub [False, False]
                  ~= lb
                  && fromBitsL lb ub [False, False, False]
                  ~= lb
          it "returns upper bound when all bits are 1" $ property $ \lb x ->
            let ub = ub' lb x
            in  fromBitsL lb ub [True]
                  ~= ub
                  && fromBitsL lb ub [True, True]
                  ~= ub
                  && fromBitsL lb ub [True, True, True]
                  ~= ub
          it
              "returns a number between lower and upper bound when some bits are 1"
            $ property
            $ \lb x ->
                let ub       = ub' lb x + 0.001
                    between' = between lb ub
                in  between' (fromBitsL lb ub [False, True])
                      && between' (fromBitsL lb ub [True, False])
                      && between' (fromBitsL lb ub [True, False, True])
          it "monotonically increases as binary increases"
            $ property
            $ \lb x ->
                let ub = ub' lb x + 0.001
                in  fromBitsL lb ub [True]
                      >  fromBitsL lb ub [False]
                      && fromBitsL lb ub [True, False]
                      >  fromBitsL lb ub [False, False]
                      && fromBitsL lb ub [False, True]
                      >  fromBitsL lb ub [True, False]
                      && fromBitsL lb ub [True, True]
                      >  fromBitsL lb ub [False, True]
                      && fromBitsL lb ub [True, False, False]
                      >  fromBitsL lb ub [False, False, False]
                      && fromBitsL lb ub [False, True, False]
                      >  fromBitsL lb ub [True, False, False]
                      && fromBitsL lb ub [True, True, False]
                      >  fromBitsL lb ub [False, True, False]
                      && fromBitsL lb ub [False, False, True]
                      >  fromBitsL lb ub [True, True, False]
                      && fromBitsL lb ub [True, False, True]
                      >  fromBitsL lb ub [False, False, True]
                      && fromBitsL lb ub [False, True, True]
                      >  fromBitsL lb ub [True, False, True]
                      && fromBitsL lb ub [True, True, True]
                      >  fromBitsL lb ub [False, True, True]
          it "converts each row in a matrix to a number" $ do
            AI.run
                (fromBits (A.constant 0) (A.constant 3) $ A.use $ A.fromList
                  (A.Z A.:. 3 A.:. 2)
                  [False, False, True, False, False, True]
                )
              `shouldBe` A.fromList (A.Z A.:. (3 :: Int)) [0 :: Double, 1, 2]

        describe "fromBits'" $ do
          it "returns 0 when no bits" $ do
            fromBitsL' [] `shouldBe` 0
          it "returns the base 10 integer represented by binary bits" $ do
            fromBitsL' [False] `shouldBe` 0
            fromBitsL' [False, False] `shouldBe` 0
            fromBitsL' [False, False, False] `shouldBe` 0
            fromBitsL' [True] `shouldBe` 1
            fromBitsL' [True, True] `shouldBe` 3
            fromBitsL' [True, True, True] `shouldBe` 7
          it "treats the leftmost bit as least significant" $ do
            fromBitsL' [False, True] `shouldBe` 2
            fromBitsL' [False, False, True] `shouldBe` 4
          it "converts each row in a matrix to a number" $ do
            AI.run
                (fromBits' $ A.use $ A.fromList
                  (A.Z A.:. 3 A.:. 2)
                  [False, False, True, False, False, True]
                )
              `shouldBe` A.fromList (A.Z A.:. (3 :: Int)) [0 :: Int, 1, 2]

fromBitsL :: Double -> Double -> [Bool] -> Double
fromBitsL lb ub xs = head $ A.toList $ AI.run $ fromBits
  (A.constant lb)
  (A.constant ub)
  (A.use $ A.fromList (A.Z A.:. length xs) xs)

fromBitsL' :: [Bool] -> Int
fromBitsL' xs = head $ A.toList $ AI.run $ fromBits'
  (A.use $ A.fromList (A.Z A.:. length xs) xs)

(~=) :: (Floating a, Ord a) => a -> a -> Bool
(~=) = aboutEquals 0.001

aboutEquals :: (Num a, Ord a) => a -> a -> a -> Bool
aboutEquals tol x y = abs (x - y) < tol

between :: Ord a => a -> a -> a -> Bool
between lb ub x = x > lb && x < ub
