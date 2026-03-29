(ns tournament.storage
  (:require [clojure.data.csv :as csv]
            [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [tournament.play :as play]))

(defn save-path
  "Derive the EDN save file path from a CSV path by replacing the .csv extension."
  [csv-path]
  (str/replace csv-path #"\.csv$" ".edn"))

(defn svg-path
  "Derive the SVG output path from an EDN save path by replacing the .edn extension."
  [edn-path]
  (str/replace edn-path #"\.edn$" ".svg"))

(defn save-tournament
  "Serialize tournament to an EDN file at path."
  [tournament path]
  (spit path (with-out-str (pprint/pprint tournament))))

(defn load-tournament
  "Deserialize a tournament from an EDN file at path."
  [path]
  (edn/read-string (slurp path)))

(defn rankings-path
  "Derive the rankings CSV path from an EDN save path."
  [edn-path]
  (str/replace edn-path #"\.edn$" "-rankings.csv"))

(defn save-rankings
  "Write final tournament rankings to a CSV file at path.
   The CSV has the same columns as the original players CSV, with a new
   'rank' column prepended.  Players eliminated in the same LB round share
   a rank.

   Args:
     tournament - completed tournament map
     path       - output file path (e.g. \"tournament-rankings.csv\")"
  [tournament path]
  (let [players      (:players tournament)
        first-player (first (filter some? (rest players)))
        col-keys     (keys first-player)
        header-row   (into ["rank"] (map name col-keys))
        rankings     (play/compute-rankings tournament)
        data-rows    (map (fn [{:keys [rank seed]}]
                            (into [rank] (map #(get (nth players seed) %) col-keys)))
                          rankings)]
    (with-open [writer (io/writer path)]
      (csv/write-csv writer (cons header-row data-rows)))))
