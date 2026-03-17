(ns tournament.play
  (:require [clojure.string :as str]
            [tournament.double-elim :as de]
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

(defn- set-match
  "Return tournament with the match at bracket/number replaced by match.

   Args:
     tournament - the full tournament map
     bracket    - keyword :WB, :LB, or :GF
     number     - integer match number within that bracket
     match      - the new match map to store at that position"
  [tournament bracket number match]
  (assoc-in tournament [bracket number] match))

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
                    or nil if there is no next match (e.g. the tournament winner)
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
                                              {:next-ref next-ref :from-ref from-ref})))
          updated-match (assoc next-match :players (assoc (:players next-match) player-slot seed))]
      (set-match tournament (:bracket next-ref) (:number next-ref) updated-match))))

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
        [left-seed right-seed] (:players match)
        loser-seed (if (= winner-seed left-seed) right-seed left-seed)]
    (-> tournament
        (set-match bracket number (assoc match :winner winner-seed :loser loser-seed))
        (advance-player (:next-winner match) winner-seed bracket number :winner)
        (advance-player (:next-loser match)  loser-seed  bracket number :loser))))

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
                  1-indexed player vector"
  [tournament bracket number winner-fn]
  (let [match       (get-match tournament bracket number)
        [left-seed right-seed] (:players match)
        winner-seed (winner-fn left-seed right-seed (:players tournament))]
    (record-result tournament bracket number winner-seed)))

(defn cli-winner-fn
  "Returns a winner-picking function compatible with play-match. Displays both
   players, prompts the user to enter 1 or 2, and returns the winning seed.

   Args:
     player->str - function of [player-map] that returns a display string for a player"
  [player->str]
  (fn [seed1 seed2 players]
    (println (str "1: " (player->str (nth players seed1))))
    (println (str "2: " (player->str (nth players seed2))))
    (print "Winner (1 or 2): ")
    (flush)
    (loop []
      (let [input (str/trim (read-line))]
        (case input
          "1" seed1
          "2" seed2
          (do (println "Please enter 1 or 2.")
              (recur)))))))

(comment

  (make-tournament "data/very_short_example_seeding.csv")

  (make-tournament "test/resources/very_very_short.csv")

  )
