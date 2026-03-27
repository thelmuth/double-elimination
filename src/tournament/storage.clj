(ns tournament.storage
  (:require [clojure.edn :as edn]
            [clojure.string :as str]))

(defn save-path
  "Derive the EDN save file path from a CSV path by replacing the .csv extension."
  [csv-path]
  (str/replace csv-path #"\.csv$" ".edn"))

(defn save-tournament
  "Serialize tournament to an EDN file at path."
  [tournament path]
  (spit path (pr-str tournament)))

(defn load-tournament
  "Deserialize a tournament from an EDN file at path."
  [path]
  (edn/read-string (slurp path)))
