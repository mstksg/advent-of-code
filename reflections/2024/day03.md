You can think of the whole thing is essentially a state machine / finite
automat. For part 1 it's straightforward: chump as many `mul(x,y)` as
possible, summing the muls:

```haskell
import qualified Control.Monad.Combinators as P
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P
import qualified Text.Megaparsec.Char.Lexer as PL

parseMul :: P.Parsec v String Int
parseMul = product <$> P.between "mul(" ")" (PL.decimal `P.sepBy` ",")

part1 :: Parsec v Int
part1 = sum <$> many (dropUntil parseMul)

-- | A utility parser combinator I have that skips until the first match
dropUntil :: P.Parsec e s end -> P.Parsec e s end
dropUntil x = P.try (P.skipManyTill P.anySingle (P.try x))
```

For part 2 the state machine has a "on or off" state: on the "off" state,
search for the next `don't`. On the "on" state, search for the next `mul` and
continue on, or the next `don't` and continue off.

```haskell
part2 :: P.Parsec v String Int
part2 = sum <$> goDisabled
  where
    goDisabled = P.option [] . dropUntil $ "do()" *> goEnabled
    goEnabled = P.option [] . dropUntil $
      P.choice
        [ "don't()" *> goDisabled n
        , (:) <$> parseMul <*> goEnabled
        ]
```
