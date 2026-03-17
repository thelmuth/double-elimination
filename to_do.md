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

## Next up

- [ ] **`make-tournament`** — combine `load-players` + `make-double-elimination` into a
  single tournament map `{:players [...] :wb [...] :lb [...] :gf [...]}`

- [ ] **Play a match** — function that takes a tournament and a match, prompts the user
  to pick a winner (by showing player strings), and returns an updated tournament
  with `:winner` and `:loser` set on that match. Consider accepting a
  winner-picking function as a parameter so deterministic versions can be used
  in tests.

- [ ] **Advance players** — after a match is played, move the winner and loser to their
  respective next matches (using `:next-winner` / `:next-loser` refs)

- [ ] **Play order** — function(s) to determine which matches are ready to play (both
  players known, not yet played). Support configurable orderings:
  - All WB then all LB
  - Interleaved: WB round N, then LB rounds 2N-1 and 2N
  - In-order within a round vs. random within a round

- [ ] **Play through full tournament** — top-level loop that repeatedly picks the next
  match(es) per the chosen play order and plays them until the GF is complete

- [ ] **Save/load** — serialize tournament map to EDN file; auto-save after each match;
  load from file to resume

## Later

- [ ] **ASCII art bracket display** — print a readable text representation of the
  bracket showing match results as they are filled in

- [ ] **SVG bracket display** — generate an SVG file for better visualization
