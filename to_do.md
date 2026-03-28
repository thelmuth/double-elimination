To Do
=====

## Done

- [x] Winner's bracket construction with standard seeding
- [x] Loser's bracket construction with drop-order anti-rematch logic
- [x] Grand finals match
- [x] `make-double-elimination` combining all three brackets
- [x] Full tests for all bracket sizes (4, 8, 13, 16, 32, 64 players)
- [x] CSV player loading (`load-players`) with keyword headers
- [x] `default-player->str` display function
- [x] `make-tournament` — combines `load-players` + `make-double-elimination`
- [x] `record-result` — sets `:winner`/`:loser` and advances players to next matches
- [x] `play-match` — drives a match with a pluggable `winner-fn`
- [x] `cli-winner-fn` — prompts user for input via stdin
- [x] `higher-seed-wins` — deterministic winner function for testing/simulation
- [x] `:BYE` handling in `play-match` (winner fns only receive integer seeds)
- [x] **Play order** — `ready-to-play?` predicate and `ready-matches` with `:wb-first`/`:interleaved` and `:in-order`/`:random` options
- [x] **Play through full tournament** — top-level loop that repeatedly picks the next
  match per the chosen play order and plays it until the GF is complete
- [x] **a main function for calling from command line** - adding functionality so I can call from command line using `clj`
- [x] Better formatting for CLI information about the players. Formatted in columns, with player A on left and player B on right. Could even format as a table with consistent width of columns (wrapping to next line if necessary) and row labels based on the keys of the player map.
- [x] **Save/load** — serialize tournament map to EDN file; auto-save after each match;
  load from file to resume
- [x] **Edit a result** - make it so that the user can edit a previously decided match.
  I only expect to use this in cases where I made a recent mistake, and not when the
  players in the match will have already played future matches.

## Next up

- [ ] **ASCII art bracket display** — print a readable text representation of the
  bracket showing match results as they are filled in

- [ ] **SVG bracket display** — generate an SVG file for better visualization
