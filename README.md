# optimal-hs-accelerate-binary

Convert binary vectors
backed by [Accelerate](https://github.com/AccelerateHS/accelerate).

Convert numeric objective functions
to binary objective functions,
for use with binary optimizers,
like:
```haskell
import qualified Data.Array.Accelerate         as A
import           Math.Optimization.Accelerate.Binary
                                                ( reverseBitsToFrac )

f' = A.Acc (A.Vector Bool) -> A.Acc (A.Scalar Double)
f' = f . reverseBitsToFrac lb ub . A.reshape (A.constant $ A.Z A.:. n A.:. b)
```
where `f` is an objective function
with a type like `A.Acc (A.Vector Double) -> A.Acc (A.Scalar Double)`,
`n` is the length of the vector passed to `f`,
and `b` is the number of bits per value in the vector passed to `f`.
