{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Math.Optimization.Accelerate.Binary
  ( reversedBitsToFrac
  , reversedBitsToInt
  , fromBool
  ) where

import qualified Data.Array.Accelerate         as A
import qualified Data.Array.Accelerate.Smart   as AS

-- | Reduce innermost dimension
-- to numbers within lower and upper bound.
-- Leftmost is least significant.
reversedBitsToFrac
  :: (A.Shape sh, A.Fractional a, A.FromIntegral A.Word8 a)
  => A.Exp a -- ^ Lower bound
  -> A.Exp a -- ^ Upper bound
  -> A.Acc (A.Array (sh A.:. Int) Bool)
  -> A.Acc (A.Array sh a)
-- Guard on empty
-- to avoid division by 0.
reversedBitsToFrac lb ub bs = A.acond (nb A.== 0)
                                      (A.fill (A.indexTail sh) lb)
                                      ((a *! reversedBitsToInt bs) !+ lb)
 where
  a  = (ub - lb) / (2 A.^ nb - 1) -- range / max int
  nb = A.indexHead sh
  sh = A.shape bs

-- | Reduce innermost dimension
-- to base 10 integer representations of bits.
-- Leftmost is least significant.
reversedBitsToInt
  :: (A.Shape sh, A.Num a, A.FromIntegral A.Word8 a)
  => A.Acc (A.Array (sh A.:. Int) Bool)
  -> A.Acc (A.Array sh a)
reversedBitsToInt = A.sum . A.imap (\i x -> fromBool x * 2 A.^ A.indexHead i)

-- | Return '1' for 'True'
-- and '0' for 'False'.
fromBool :: (A.FromIntegral A.Word8 a) => A.Exp Bool -> A.Exp a
fromBool = A.fromIntegral . AS.mkCoerce @_ @A.Word8

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
