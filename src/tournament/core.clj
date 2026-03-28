(ns tournament.core
  (:require [clojure.java.io :as io]
            [tournament.play :as play]
            [tournament.storage :as storage]
            [tournament.winner-fns :as wfn]))

(defn- run-tournament [tournament save-path]
  (let [completed      (play/play-tournament tournament
                                             (wfn/cli-winner-fn)
                                             {:after-match #(storage/save-tournament % save-path)})
        gf-match       (play/get-match completed :GF 0)
        winner         (nth (:players completed) (:winner gf-match))
        runner-up      (nth (:players completed) (:loser gf-match))]
    (println)
    (println (wfn/format-final-result winner runner-up))
    (println)))

(defn -main
  [mode & args]
  (when (empty? args)
    (println "Usage: clj -M:start <players.csv>")
    (println "       clj -M:load <save.edn>")
    (System/exit 1))
  (case mode
    "start"
    (let [csv-path (first args)
          edn-path (storage/save-path csv-path)]
      (when (.exists (io/file edn-path))
        (println (str "Error: save file already exists: " edn-path))
        (println "Move or delete it before starting a new tournament with this CSV.")
        (System/exit 1))
      (println (str "Starting tournament from " csv-path))
      (println (str "Saving progress to " edn-path))
      (run-tournament (play/make-tournament csv-path) edn-path))

    "load"
    (let [edn-path (first args)]
      (when-not (.exists (io/file edn-path))
        (println (str "Error: save file not found: " edn-path))
        (System/exit 1))
      (println (str "Resuming tournament from " edn-path))
      (run-tournament (storage/load-tournament edn-path) edn-path))

    (do
      (println (str "Unknown command: " mode))
      (println "Usage: clj -M:start <players.csv>")
      (println "       clj -M:load <save.edn>")
      (System/exit 1))))
