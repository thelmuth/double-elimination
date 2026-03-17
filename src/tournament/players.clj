(ns tournament.players
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.string :as str]))

;; ------------------------
;; Player loading from CSV
;; ------------------------

(defn load-players
  "Load players from a CSV file. The first row is treated as column headers,
   which become keyword keys in each player map. The rows are in seeded order,
   so the first data row is seed 1, second is seed 2, etc.

   Returns a 0-indexed vector where the first element is the first player.

   Example CSV:
     artist,title,year
     The Beatles,Hey Jude,1968
     David Bowie,Heroes,1977

   Returns:
     [{:artist \"The Beatles\" :title \"Hey Jude\" :year \"1968\"}
      {:artist \"David Bowie\" :title \"Heroes\" :year \"1977\"}]"
  [filename]
  (with-open [reader (io/reader filename)]
    (let [rows    (doall (csv/read-csv reader))
          headers (map (comp keyword #(str/replace % " " "-") str/lower-case str/trim) (first rows))
          players (mapv (fn [row] (zipmap headers (map str/trim row))) (rest rows))]
      players)))

;; ------------------------
;; Player display
;; ------------------------

(defn default-player->str
  "Default player display string: joins all map values with ' - '.
   Provide your own function for custom formatting."
  [player]
  (str/join " - " (vals player)))
