(ns tournament.play
  (:require [clojure.string :as str]
            [tournament.double-elim :as de]
            [tournament.players :as players]))

;; ------------------------
;; Tournament creation
;; ------------------------

(defn make-tournament
  "Create a full tournament from a CSV file of players in seeded order.
   Returns a map with :players (1-indexed vector with nil at index 0), :WB, :LB, :GF."
  [players-file]
  (let [players (players/load-players players-file)
        n (count players)]
    (assoc (de/make-double-elimination n)
           :players (into [nil] players))))

;; ------------------------
;; Match lookup helpers
;; ------------------------

(defn get-match
  "Get a match map by bracket keyword (:WB/:LB/:GF) and match number."
  [tournament bracket number]
  (get-in tournament [bracket number]))

(defn- set-match
  "Return tournament with the match at bracket/number replaced by match."
  [tournament bracket number match]
  (assoc-in tournament [bracket number] match))

;; ------------------------
;; Recording results and advancing players
;; ------------------------

(defn- advance-player
  "Place seed into the correct slot of the match pointed to by next-ref.
   Determines left (slot 0) vs. right (slot 1) by matching the next match's
   :prev-left/:prev-right against from-bracket/from-number/result-type.
   Returns tournament unchanged if next-ref is nil."
  [tournament next-ref seed from-bracket from-number result-type]
  (if (nil? next-ref)
    tournament
    (let [next-match (get-match tournament (:bracket next-ref) (:number next-ref))
          from-ref   {:bracket from-bracket :number from-number :result result-type}
          slot       (cond
                       (= from-ref (:prev-left next-match))  0
                       (= from-ref (:prev-right next-match)) 1
                       :else (throw (ex-info "Could not determine slot for advancing player"
                                             {:next-ref next-ref :from-ref from-ref})))
          updated    (assoc next-match :players (assoc (:players next-match) slot seed))]
      (set-match tournament (:bracket next-ref) (:number next-ref) updated))))

(defn record-result
  "Record the result of a match. winner-seed is the seed of the winning player.
   Returns updated tournament with :winner/:loser set on that match and both
   players advanced to their next matches."
  [tournament bracket number winner-seed]
  (let [match      (get-match tournament bracket number)
        [s1 s2]    (:players match)
        loser-seed (if (= winner-seed s1) s2 s1)
        t1         (set-match tournament bracket number
                               (assoc match :winner winner-seed :loser loser-seed))
        t2         (advance-player t1 (:next-winner match) winner-seed bracket number :winner)
        t3         (advance-player t2 (:next-loser match)  loser-seed  bracket number :loser)]
    t3))

;; ------------------------
;; Playing matches
;; ------------------------

(defn play-match
  "Play a single match. winner-fn takes [seed1 seed2 players] and returns
   the winning seed. Returns updated tournament."
  [tournament bracket number winner-fn]
  (let [match      (get-match tournament bracket number)
        [s1 s2]    (:players match)
        winner-seed (winner-fn s1 s2 (:players tournament))]
    (record-result tournament bracket number winner-seed)))

(defn cli-winner-fn
  "Returns a winner-picking function that displays both players using player->str,
   prompts the user to enter 1 or 2, and returns the winning seed."
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