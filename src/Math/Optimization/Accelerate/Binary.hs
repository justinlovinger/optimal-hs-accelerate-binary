{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Math.Optimization.Accelerate.Binary
  ( reverseBitsToFrac
  , reverseBitsToInt
  ) where

import qualified Data.Array.Accelerate         as A

-- | Reduce innermost dimension
-- to numbers within lower and upper bound.
-- Leftmost is least significant.
reverseBitsToFrac
  :: (A.Shape sh, A.Fractional a, A.FromIntegral Int a)
  => A.Exp a -- ^ Lower bound
  -> A.Exp a -- ^ Upper bound
  -> A.Acc (A.Array (sh A.:. Int) Bool)
  -> A.Acc (A.Array sh a)
-- Guard on empty
-- to avoid division by 0.
reverseBitsToFrac lb ub bs = A.acond
  (nb A.== 0)
  (A.fill (A.indexTail sh) lb)
  ((a *! A.map A.fromIntegral (reverseBitsToInt bs)) !+ lb)
 where
  a  = (ub - lb) / (2 A.^ nb - 1) -- range / max int
  nb = A.indexHead sh
  sh = A.shape bs

-- | Reduce innermost dimension
-- to base 10 integer representations of bits.
-- Leftmost is least significant.
reverseBitsToInt
  :: (A.Shape sh)
  => A.Acc (A.Array (sh A.:. Int) Bool)
  -> A.Acc (A.Array sh Int)
reverseBitsToInt = A.sum . A.imap (\i x -> A.boolToInt x * 2 A.^ A.indexHead i)

-- | Add scalar to each element of an array.
(!+)
  :: (A.Shape sh, A.Num a)
  => A.Acc (A.Array sh a)
  -> A.Exp a
  -> A.Acc (A.Array sh a)
(!+) a x = A.map (+ x) a

-- | Multiply scalar by each element of an array.
(*!)
  :: (A.Shape sh, A.Num a)
  => A.Exp a
  -> A.Acc (A.Array sh a)
  -> A.Acc (A.Array sh a)
(*!) x = A.map (x *)
