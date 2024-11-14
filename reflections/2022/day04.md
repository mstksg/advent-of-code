A bit of fun number crunching :)  Here is a chance to leverage an interval
library, like haskell's
*[data-interval](https://hackage.haskell.org/package/data-interval)*:

```haskell
import           Data.IntegerInterval (IntegerInterval)
import qualified Data.IntegerInterval as I

part1Criterion :: IntegerInterval -> IntegerInterval -> Bool
part1Criterion xs ys = xs `I.isSubsetOf` ys || ys `I.isSubsetOf` xs

part2Criterion :: IntegerInterval -> IntegerInterval -> Bool
part2Criterion = (I.==?)
```

From there on it's just a matter of running the criteria on each pair of
intervals in the list and counting which ones are valid!
