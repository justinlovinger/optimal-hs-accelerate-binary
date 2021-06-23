{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Math.Optimization.Accelerate.Binary
  ( fromBools
  , fromBools'
  , fromBool
  ) where

import qualified Data.Array.Accelerate         as A

-- | Reduce innermost dimension
-- to numbers within lower and upper bound.
fromBools
  :: (A.Shape sh, A.Fractional a)
  => A.Exp a -- ^ Lower bound
  -> A.Exp a -- ^ Upper bound
  -> A.Acc (A.Array (sh A.:. Int) Bool)
  -> A.Acc (A.Array sh a)
-- Guard on empty
-- to avoid division by 0.
fromBools lb ub bs = A.acond (nb A.== 0)
                            (A.fill (A.indexTail sh) lb)
                            ((a *! fromBools' bs) !+ lb)
 where
  a  = (ub - lb) / (2 A.^ nb - 1) -- range / max int
  nb = A.indexHead sh
  sh = A.shape bs

-- | Reduce innermost dimension
-- to base 10 integer representations of bits.
-- Leftmost is least significant.
fromBools'
  :: (A.Shape sh, A.Num a)
  => A.Acc (A.Array (sh A.:. Int) Bool)
  -> A.Acc (A.Array sh a)
fromBools' = A.sum . A.imap (\i x -> fromBool x * 2 A.^ A.indexHead i)

fromBool :: (A.Num a) => A.Exp Bool -> A.Exp a
fromBool x = A.cond x 1 0

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
