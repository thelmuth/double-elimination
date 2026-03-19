# Double Elimination Tournament

Run a double elimination tournament from a CSV file of seeded players, with
interactive match-by-match input via the command line.

## Requirements

- [Clojure CLI](https://clojure.org/guides/install_clojure)

## CSV Format

Your players CSV must have a header row. Rows are listed in seeded order (first
row = seed 1, second row = seed 2, etc.). Any column names are supported.

Example:

```
artist,title,year
The Beatles,Hey Jude,1968
David Bowie,Heroes,1977
Led Zeppelin,Stairway to Heaven,1971
The Rolling Stones,Paint It Black,1966
```

## Running a Tournament

```
clj -M:run path/to/players.csv
```

This will:
1. Build a double elimination bracket seeded from your CSV
2. Present each match interactively, showing the bracket, round, match number,
   and both players with their seeds
3. Prompt you to enter `A` or `B` to pick the winner of each match
4. Continue until the Grand Finals is decided, then announce the winner

### Example match prompt

```
----------------------------------------------
    Winner Bracket  |  Round 1  |  Match 0
----------------------------------------------

A (seed 1): The Beatles - Hey Jude - 1968

B (seed 4): The Rolling Stones - Paint It Black - 1966

Winner (A or B):
```

## Running Tests

```
clj -X:test
```
