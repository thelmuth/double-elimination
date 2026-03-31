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
- [x] **SVG bracket display** — generate an SVG file showing WB on top, LB below,
  both left-to-right, GF connecting both; includes seed + one player CSV key per
  match box; scales to large brackets (800–1000+ players)
- [x] **Ranking of players after tournament** - Make a list of the order of players (some will be tied) of the "place" they took. Winner of GF is rank 1, loser of GF is rank 2, loser of last game in LB is rank 3, etc. I think it would be best to just create a text file of this at the end of the tournament.

## Next up

- [ ] **Make playlists for iTunes** (if possible) - I usually have to manually search for each song that will be in a match. Would it be possible to make an iTunes playlist of the upcoming matches for a given bracket/round, so that I can just make that playlist and listen to it straight through to decide matches? Note: I'm running the tournament on my MacOS laptop, but my iTunes is on Windows. I am making the original CSVs for the tournament by exporting a playlist from iTunes, if that helps - the seeds are in iTunes under the "Grouping" field. I do make a few manual changes to the iTunes exported CSV, things like renaming some columns and removing others, before sending to the tournament. I can do some of the work manually, if that helps -- making a new playlist, adding the songs, etc. The most important thing is sorting the songs in the order of the tournament, so that I don't have to do that myself.


## Maybe in the far future

- [ ] **Add to Clojars** - Add this project to Clojars, including specific useful functions in the README, so that others could use it as a dependency. Will require considering what are the core entry points and documenting those well.

- [ ] **Interactive Website** - Make a website with the same layout as the SVG, except interactive, so that you can click a match and tell which player won.