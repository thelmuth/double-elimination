(ns tournament.winner-fns
  (:require [clojure.string :as str]))

(defn higher-seed-wins
  "Winner function that always returns the higher-seeded (lower seed number) player.

   Args:
     left-seed  - integer seed of the left player
     right-seed - integer seed of the right player
     players    - the tournament's 1-indexed player vector (unused)
     match      - the full match map (unused)"
  [left-seed right-seed _players _match]
  (min left-seed right-seed))

(defn cli-winner-fn
  "Returns a winner-picking function compatible with play-match. Displays match
   info and both players, prompts the user to pick a winner, and returns the
   winning seed.

   Args:
     player->str - function of [player-map] that returns a display string for a player"
  [player->str]
  (fn [left-seed right-seed players match]
    (println "\n----------------------------------------------")
    (println (format "    %s  |  Round %d  |  Match %d"
                     (case (:bracket match)
                       :WB "Winner Bracket"
                       :LB "Loser Bracket"
                       :GF "Grand Finals")
                     (:round match)
                     (:number match)))
    (println "----------------------------------------------")
    (println)
    (println (format "A (seed %d): %s" left-seed (player->str (nth players left-seed))))
    (println)
    (println (format "B (seed %d): %s" right-seed (player->str (nth players right-seed))))
    (println)
    (print "Winner (A or B): ")
    (flush)
    (loop []
      (let [input (str/lower-case (str/trim (read-line)))]
        (case input
          "a" left-seed
          "b" right-seed
          (do (println "Please enter A or B.")
              (recur)))))))
