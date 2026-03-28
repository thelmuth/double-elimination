# Double Elimination Tournament

Run a double elimination tournament from a CSV file of seeded players, with
interactive match-by-match input via the command line. Progress is auto-saved
after each match so you can resume at any time.

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

## Starting a Tournament

```
clj -M:start path/to/players.csv
```

This will:
1. Build a double elimination bracket seeded from your CSV
2. Create a save file at `path/to/players.edn` (exits with an error if it already exists)
3. Present each match interactively, showing bracket, round, match number, and both players
4. Prompt you to enter `A` or `B` to pick the winner of each match
5. Auto-save progress after every match
6. Continue until the Grand Finals is decided, then announce the winner

## Resuming a Tournament

```
clj -M:load path/to/players.edn
```

Loads the saved tournament state and continues from where you left off,
auto-saving after each match as before.

## Notes on Save Files

- The save file is derived automatically from the CSV path (`.csv` → `.edn`, same directory)
- Starting a new tournament with a CSV whose save file already exists is an error —
  move or delete the `.edn` file first if you want to start fresh

### Example match prompt

```
┌──────────────────────────────────────────────────────────────────────────────────────────────────────────┐
│                              Winner Bracket  ·  Round 1  ·  Match 1                                      │
├────────────────┬─────────────────────────────────────────────────────┬───────────────────────────────────┤
│                │                        A                            │                  B                │
├────────────────┼─────────────────────────────────────────────────────┼───────────────────────────────────┤
│ Artist         │ The Beatles                                         │ The Rolling Stones                │
│ Title          │ Hey Jude                                            │ Paint It Black                    │
│ Year           │ 1968                                                │ 1966                              │
├────────────────┴─────────────────────────────────────────────────────┴───────────────────────────────────┤
│ ┌─ Commands ───────────────────────────────────┐                                                         │
│ │  a                    Player A wins          │                                                         │
│ │  b                    Player B wins          │                                                         │
│ │  undo <WB|LB|GF> <n>  Edit a past result     │                                                         │
│ │  svg                  Save bracket diagram   │                                                         │
│ └──────────────────────────────────────────────┘                                                         │
└──────────────────────────────────────────────────────────────────────────────────────────────────────────┘
  >
```

### SVG bracket diagram

At any match prompt, type `svg` to save an SVG bracket diagram alongside your save
file (same path with `.svg` extension). The diagram shows:

- **Winner's Bracket** on top, **Loser's Bracket** below, both progressing left to right
- **Grand Finals** connecting both brackets on the right
- Each match box shows seed + one player CSV field (defaults to the first column)
- Color-coded rows: green = winner, red = loser, gray = TBD/BYE
- Scales to tournaments of any size

## Running Tests

```
clj -X:test
```
