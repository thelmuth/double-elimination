(ns tournament.play
  (:require [tournament.double-elim :as de]
            [tournament.players :as players]))

;; ------------------------
;; Tournament creation
;; ------------------------

(defn make-tournament
  "Create a full tournament from a CSV file of players in seeded order.
   Returns a map with :players (1-indexed vector with nil at index 0), :WB, :LB, :GF.

   Args:
     players-file - path to a CSV file whose rows are players in seeded order"
  [players-file]
  (let [players      (players/load-players players-file)
        player-count (count players)]
    (assoc (de/make-double-elimination player-count)
           :players (into [nil] players))))

;; ------------------------
;; Match lookup helpers
;; ------------------------

(defn get-match
  "Get a match map by bracket keyword and match number.

   Args:
     tournament - the full tournament map
     bracket    - keyword :WB, :LB, or :GF
     number     - integer match number within that bracket"
  [tournament bracket number]
  (get-in tournament [bracket number]))

(defn- set-match-player
  "Place player seed into slot (0=left, 1=right) of the match at bracket/number.

   Args:
     tournament - the full tournament map
     bracket    - keyword :WB, :LB, or :GF
     number     - integer match number within that bracket
     slot       - 0 for left player, 1 for right player
     seed       - integer seed of the player to place"
  [tournament bracket number slot seed]
  (assoc-in tournament [bracket number :players slot] seed))

(defn- set-match-result
  "Record winner and loser on the match at bracket/number.

   Args:
     tournament   - the full tournament map
     bracket      - keyword :WB, :LB, or :GF
     number       - integer match number within that bracket
     winner-seed  - integer seed of the winning player
     loser-seed   - integer seed of the losing player"
  [tournament bracket number winner-seed loser-seed]
  (-> tournament
      (assoc-in [bracket number :winner] winner-seed)
      (assoc-in [bracket number :loser] loser-seed)))

;; ------------------------
;; Recording results and advancing players
;; ------------------------

(defn- advance-player
  "Place a player into the correct slot of the next match after a result is recorded.
   Determines left (slot 0) vs. right (slot 1) by matching the destination match's
   :prev-left/:prev-right against from-bracket/from-number/result-type.
   Returns tournament unchanged if next-ref is nil.

   Args:
     tournament   - the full tournament map
     next-ref     - map {:bracket :WB/:LB/:GF :number N} pointing to the destination match,
                    or nil if there is no next match (e.g. the tournament winner or a loser
                    in the LB)
     seed         - integer seed of the player being advanced
     from-bracket - bracket keyword of the match just played (:WB, :LB, or :GF)
     from-number  - match number of the match just played
     result-type  - :winner or :loser, indicating which result is being advanced"
  [tournament next-ref seed from-bracket from-number result-type]
  (if (nil? next-ref)
    tournament
    (let [next-match  (get-match tournament (:bracket next-ref) (:number next-ref))
          from-ref    {:bracket from-bracket :number from-number :result result-type}
          player-slot (cond
                        (= from-ref (:prev-left next-match))  0
                        (= from-ref (:prev-right next-match)) 1
                        :else (throw (ex-info "Could not determine slot for advancing player"
                                              {:next-ref next-ref :from-ref from-ref})))]
      (set-match-player tournament (:bracket next-ref) (:number next-ref) player-slot seed))))

(defn record-result
  "Record the result of a match, returning an updated tournament with :winner
   and :loser set on the match and both players advanced to their next matches.

   Args:
     tournament   - the full tournament map
     bracket      - keyword :WB, :LB, or :GF
     number       - integer match number within that bracket
     winner-seed  - integer seed of the winning player"
  [tournament bracket number winner-seed]
  (let [match (get-match tournament bracket number)
        [left-seed right-seed] (:players match)]
    (when (= :TBD left-seed)
      (throw (ex-info "Left player is TBD; match is not ready to play"
                      {:bracket bracket :number number})))
    (when (= :TBD right-seed)
      (throw (ex-info "Right player is TBD; match is not ready to play"
                      {:bracket bracket :number number})))
    (when-not (or (= winner-seed left-seed) (= winner-seed right-seed))
      (throw (ex-info "Winner is not one of the players in this match"
                      {:bracket bracket :number number :winner-seed winner-seed :players [left-seed right-seed]})))
    (let [loser-seed (if (= winner-seed left-seed) right-seed left-seed)]
      (-> tournament
        (set-match-result bracket number winner-seed loser-seed)
        (advance-player (:next-winner match) winner-seed bracket number :winner)
        (advance-player (:next-loser match)  loser-seed  bracket number :loser)))))

;; ------------------------
;; Playing matches
;; ------------------------

(defn play-match
  "Play a single match using winner-fn to determine the winner, returning
   an updated tournament.

   Args:
     tournament - the full tournament map
     bracket    - keyword :WB, :LB, or :GF
     number     - integer match number within that bracket
     winner-fn  - function of [seed1 seed2 players] that returns the winning seed;
                  seed1/seed2 are integer seeds, players is the tournament's
                  1-indexed player vector; not called when either player is :BYE"
  [tournament bracket number winner-fn]
  (let [match       (get-match tournament bracket number)
        [left-seed right-seed] (:players match)
        winner-seed (cond
                      (= left-seed :BYE)  right-seed
                      (= right-seed :BYE) left-seed
                      :else (winner-fn left-seed right-seed (:players tournament)))]
    (record-result tournament bracket number winner-seed)))
