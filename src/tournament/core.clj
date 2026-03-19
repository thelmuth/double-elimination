(ns tournament.core
  (:require [tournament.play :as play]
            [tournament.players :as players]
            [tournament.winner-fns :as wfn]))

(defn -main
  [& args]
  (when (empty? args)
    (println "Usage: clj -M:run <players-csv>")
    (System/exit 1))
  (let [players-file (first args)
        tournament   (play/make-tournament players-file)
        completed    (play/play-tournament tournament
                                           (wfn/cli-winner-fn players/default-player->str)
                                           {})
        winner-seed  (:winner (first (:GF completed)))
        winner       (nth (:players completed) winner-seed)]
    (println "\n==============================================\n")
    (println "Tournament complete!")
    (println "Winner:" (players/default-player->str winner))))
