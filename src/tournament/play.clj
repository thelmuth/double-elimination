(ns tournament.play
  (:require [clojure.string :as str]
            [tournament.double-elim :as td]))

(defn all-matches-played?
  "True if all matches have a :winner."
  [bracket]
  (every? :winner bracket))

(defn find-match
  "Find match by ID in vector of matches."
  [bracket id]
  (first (filter #(= (:id %) id) bracket)))

(defn update-match
  "Replace match with same :id in bracket with new value."
  [bracket match]
  (mapv #(if (= (:id %) (:id match)) 
           match
           %) 
        bracket))

(defn advance-winner
  "Place the winner into the appropriate slot of the next match's :players."
  [bracket match winner]
  (if-let [next-id (:next-winner match)]
    (let [next-match (find-match bracket next-id)
          ;; replace :TBD with winner
          players (:players next-match)
          idx (.indexOf players :TBD)
          updated-next (assoc-in next-match [:players idx] winner)]
      (update-match bracket updated-next))
    bracket))

(defn play-match
  "Play a single match interactively. Returns updated bracket."
  [bracket match]
  (let [[p1 p2] (:players match)]
    (println "\nRound" (:round match) "- Match" (:id match))
    (println "a)" p1)
    (println "b)" p2)
    (cond
      (= p1 :TBD) bracket
      (= p2 :TBD) bracket

      (= p1 :BYE)
      (let [winner p2
            loser p1
            updated (assoc match :winner winner :loser loser)
            bracket' (update-match bracket updated)]
        (println "Auto-advancing" winner "(bye)")
        (advance-winner bracket' updated winner))

      (= p2 :BYE)
      (let [winner p1
            loser p2
            updated (assoc match :winner winner :loser loser)
            bracket' (update-match bracket updated)]
        (println "Auto-advancing" winner "(bye)")
        (advance-winner bracket' updated winner))

      :else
      (let [winner (loop []
                     (print "Winner? (a/b): ") (flush)
                     (let [input (read-line)]
                       (println input)
                       (case (str/lower-case input)
                         "a" p1
                         "b" p2
                         (do (println "Invalid input, please enter a or b.")
                             (recur)))))
            loser (if (= winner p1) p2 p1)
            updated (assoc match :winner winner :loser loser)
            bracket' (update-match bracket updated)]
        (advance-winner bracket' updated winner)))))

(defn play-bracket
  "Play through the entire Winner's Bracket interactively."
  [bracket]
  (loop [bracket bracket]
    (if (all-matches-played? bracket)
      (do
        (println "\n🏆 Winner's bracket complete!")
        bracket)
      (let [next-match (first (filter #(nil? (:winner %)) bracket))]
        (recur (play-match bracket next-match))))))




(comment

  (def wb (td/make-wb 6))

  wb

  (tournament.play/play-bracket wb)

  )
