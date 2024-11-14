Advent of Code
==============

It's the most wonderful time of the year!

This is my mega-repo of [Advent of Code][] solutions here, along with an
automated fetching, testing, running environment (powered by the
*[advent-of-code-api][]* library). So far only Haskell solutions are here, but
maybe more will come later?

[Advent of Code]: https://adventofcode.com/
[advent-of-code-api]: https://hackage.haskell.org/package/advent-of-code-api

Reflections and detailed solutions are available on the wiki:

*   [2020 Reflections](https://github.com/mstksg/advent-of-code/wiki/Reflections-2020)
*   [2021 Reflections](https://github.com/mstksg/advent-of-code/wiki/Reflections-2021)
*   [2022 Reflections](https://github.com/mstksg/advent-of-code/wiki/Reflections-2022)
*   [2023 Reflections](https://github.com/mstksg/advent-of-code/wiki/Reflections-2023)

## `:~>` type

If you're looking at my actual github solutions (and not the ones cleaned up
for the reflections), you'll notice that I'm implementing my solutions in terms
of a `:~>` record type:

```haskell
data a :~> b = MkSol
    { sParse :: String -> Maybe a    -- ^ parse input into an `a`
    , sSolve :: a      -> Maybe b    -- ^ solve an `a` input to a `b` solution
    , sShow  :: b      -> String     -- ^ print out the `b` solution for submission
    }
```

An `a :~> b` is a solution to a challenge expecting input of type `a` and
producing answers of type `b`.  It also packs in functions to parse a `String`
into an `a`, and functions to show a `b` as a `String` to submit as an answer.

This helps me mentally separate out parsing, solving, and showing, allowing for
some cleaner code and an easier time planning my solution.

Such a challenge can be "run" on string inputs by feeding the string into
`sParse`, then `sSolve`, then `sShow`:

```haskell
-- | Run a ':~>' on some input, retuning 'Maybe'
runSolution :: Challenge -> String -> Maybe String
runSolution MkSol{..} s = do
    x <- sParse s
    y <- sSolve x
    pure (sShow y)
```

In the actual library, I have `runSolution` return an `Either` so I can debug
which stage the error happened in.

You might also notice the function `dyno_`, used like `dyno_ "limit" 10000`.  This
is how I implement parameters in problems that vary between test data and
actual input.  For example, Day 6 involved finding points that had a total
distance of less than 10000, but for the test input, we found the points that
had a total distance of less than 32.  So, I have a system that lets me write
`dyno_ "limit" 10000` in my code instead of hard-coding in `10000`.  This
`10000` would be replaced by `32` when running with test data (which is parsed
from [this file][7btest])

[7btest]: https://github.com/mstksg/advent-of-code/blob/master/test-data/2018/06b.txt

Interactive
-----------

The *AOC.Run.Interactive* module has code (powered by *[advent-of-code-api][]*)
for testing your solutions and submitting within GHCI, so you don't have to
re-compile. If you edit your solution programs, they are automatically updated
when you hit `:r` in ghci.

```haskell
ghci> execSolution_   $ solSpec 'day02a   -- get answer for challenge based on solution
ghci> testSolution_   $ solSpec 'day02a   -- run solution against test suite
ghci> viewPrompt_     $ solSpec 'day02a   -- view the prompt for a part
ghci> waitForPrompt_  $ solSpec 'day02a   -- count down to the prompt for a part
ghci> submitSolution_ $ solSpec 'day02a   -- submit a solution, and retry after cooldown automatically
```

These are loaded with session key stored in the configuration file (see next
section).

Executable
----------

Comes with test examples given in problems.

You can install using `cabal` or `nix`:

```bash
$ git clone https://github.com/mstksg/advent-of-code
$ cd advent-of-code
$ nix build .#aoc2023
```

The executable `aoc2023` includes a testing and benchmark suite, as well as a
way to view prompts within the command line:

```
$ ./results/bin/aoc2023 --help
aoc2023 - Advent of Code 2023 challenge runner

Usage: aoc2023 [-c|--config PATH] COMMAND
  Run challenges from Advent of Code 2023. Available days: 1, 2, 3 (..)

Available options:
  -c,--config PATH         Path to configuration file (default: aoc-conf.yaml)
  -h,--help                Show this help text

Available commands:
  run                      Run, test, and benchmark challenges
  view                     View a prompt for a given challenge
  submit                   Test and submit answers for challenges
  test                     Alias for run --test
  bench                    Alias for run --bench
  countdown                Alias for view --countdown

$ ./results/bin/aoc2023 run 3 b
>> Day 03b
>> [✓] 243
```

You can supply input via stdin with `--stdin`:

```
$ ./results/bin/aoc2023 run 1 --stdin
>> Day 01a
+1
+2
+1
-3
<Ctrl+D>
[?] 1
>> Day 01b
[?] 1
```

Benchmarking is implemented using *criterion*

```
$ ./results/bin/aoc2023 bench 2
>> Day 02a
benchmarking...
time                 1.317 ms   (1.271 ms .. 1.392 ms)
                     0.982 R²   (0.966 R² .. 0.999 R²)
mean                 1.324 ms   (1.298 ms .. 1.373 ms)
std dev              115.5 μs   (77.34 μs .. 189.0 μs)
variance introduced by outliers: 65% (severely inflated)

>> Day 02b
benchmarking...
time                 69.61 ms   (68.29 ms .. 72.09 ms)
                     0.998 R²   (0.996 R² .. 1.000 R²)
mean                 69.08 ms   (68.47 ms .. 69.99 ms)
std dev              1.327 ms   (840.8 μs .. 1.835 ms)
```

Test suites run the example problems given in the puzzle description, and
outputs are colorized in ANSI terminals.

```
$ ./results/bin/aoc2021 test 1
>> Day 01a
[✓] (3)
[✓] (3)
[✓] (0)
[✓] (-6)
[✓] Passed 4 out of 4 test(s)
[✓] 416
>> Day 01b
[✓] (2)
[✓] (0)
[✓] (10)
[✓] (5)
[✓] (14)
[✓] Passed 5 out of 5 test(s)
[✓] 56752
```

This should only work if you're running `aoc2023` in the project directory.

**To run on actual inputs**, the executable expects inputs to be found in the
folder `data/<year>/XX.txt` in the directory you are running in.  That is, the input
for 2023 Day 7 will be expected at `data/2023/07.txt`.

*aoc2023 will download missing input files*, but requires a session token.
This can be provided in `aoc-conf.yaml`:

```yaml
session:  [[ session token goes here ]]
```

Session keys are also required to download "Part 2" prompts for each challenge.

You can "lock in" your current answers (telling the executable that those are
the correct answers) by passing in `--lock`.  This will lock in any final
puzzle solutions encountered as the verified official answers.  Later, if you
edit or modify your solutions, they will be checked on the locked-in answers.

These are stored in `data/<year>/ans/XXpart.txt`.  That is, the target output
for 2023 Day 7 (Part 2, `b`) will be expected at `data/2023/ans/07b.txt`.  You can
also manually edit these files.

You can view prompts: (use `--countdown` to count down until a prompt is
released, and display immediately)

```
$ ./result/bin/aoc2023 view 3 b
>> Day 03b
--- Part Two ---
----------------

Amidst the chaos, you notice that exactly one claim doesn't overlap by
even a single square inch of fabric with any other claim. If you can
somehow draw attention to it, maybe the Elves will be able to make
Santa's suit after all!

For example, in the claims above, only claim `3` is intact after all
claims are made.

*What is the ID of the only claim that doesn't overlap?*
```

You can also submit answers:

```
$ ./result/bin/aoc2023 submit 1 a
```

Submissions will automatically run the test suite.  If any tests fail, you will
be asked to confirm submission or else abort.  The submit command will output
the result of your submission: The message from the AoC website, and whether or
not your answer was correct (or invalid or ignored).  Answers that are
confirmed correct will be locked in and saved for future testing against, in
case you change your solution.

All networking features are powered by *[advent-of-code-api][]*.

Old Repos
---------

* [2016](https://github.com/mstksg/advent-of-code-2016)
* [2017](https://github.com/mstksg/advent-of-code-2017)
* [2018](https://github.com/mstksg/advent-of-code-2018)
* [2019](https://github.com/mstksg/advent-of-code-2019)
* [2020](https://github.com/mstksg/advent-of-code-2020) (Fully migrated)
* [2021](https://github.com/mstksg/advent-of-code-2021) (Fully migrated)
* [2022](https://github.com/mstksg/advent-of-code-2022) (Fully migrated)
* [2023](https://github.com/mstksg/advent-of-code-2023) (Fully migrated)
