(ns tournament.core
  (:require [clojure.java.io :as io]
            [tournament.play :as play]
            [tournament.storage :as storage]
            [tournament.svg :as svg]
            [tournament.winner-fns :as wfn]))

(defn- run-tournament [tournament save-path]
  (let [svg-save-path      (storage/svg-path save-path)
        rankings-save-path (storage/rankings-path save-path)
        ;; Agent serializes saves onto a background thread so the match prompt
        ;; appears immediately rather than blocking on EDN/SVG I/O.
        save-agent         (agent nil :error-handler (fn [_ ex] (println "\nSave error:" ex)))
        completed          (play/play-tournament tournament
                                                 (wfn/cli-winner-fn nil)
                                                 {:after-match (fn [t]
                                                                 (svg/save-svg t svg-save-path)
                                                                 (send save-agent
                                                                       (fn [_]
                                                                         (storage/save-tournament t save-path))))})
        ;; Block until the last background save finishes before writing rankings.
        _                  (await save-agent)
        gf-match           (play/get-match completed :GF 0)
        winner             (nth (:players completed) (:winner gf-match))
        runner-up          (nth (:players completed) (:loser gf-match))]
    (storage/save-rankings completed rankings-save-path)
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
