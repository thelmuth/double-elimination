(ns tournament.play
  (:require [tournament.double-elim :as de]
            [tournament.play-order :as order]
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

(defn- set-match-player
  "Place player seed into slot (0=left, 1=right) of the match at bracket/number.

   Args:
     tournament - the full tournament map
     bracket    - keyword :WB, :LB, or :GF
     number     - integer match number within that bracket
     slot       - 0 for left player, 1 for right player
     seed       - integer seed of the player to place"
  [tournament bracket number slot seed]
  (assoc-in tournament [bracket number :players slot] seed))

(defn- set-match-result
  "Record winner and loser on the match at bracket/number.

   Args:
     tournament   - the full tournament map
     bracket      - keyword :WB, :LB, or :GF
     number       - integer match number within that bracket
     winner-seed  - integer seed of the winning player
     loser-seed   - integer seed of the losing player"
  [tournament bracket number winner-seed loser-seed]
  (-> tournament
      (assoc-in [bracket number :winner] winner-seed)
      (assoc-in [bracket number :loser] loser-seed)))

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
                    or nil if there is no next match (e.g. the tournament winner or a loser
                    in the LB)
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
                                              {:next-ref next-ref :from-ref from-ref})))]
      (set-match-player tournament (:bracket next-ref) (:number next-ref) player-slot seed))))

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
        [left-seed right-seed] (:players match)]
    (when (= :TBD left-seed)
      (throw (ex-info "Left player is TBD; match is not ready to play"
                      {:bracket bracket :number number})))
    (when (= :TBD right-seed)
      (throw (ex-info "Right player is TBD; match is not ready to play"
                      {:bracket bracket :number number})))
    (when-not (or (= winner-seed left-seed) (= winner-seed right-seed))
      (throw (ex-info "Winner is not one of the players in this match"
                      {:bracket bracket :number number :winner-seed winner-seed :players [left-seed right-seed]})))
    (let [loser-seed (if (= winner-seed left-seed) right-seed left-seed)]
      (-> tournament
        (set-match-result bracket number winner-seed loser-seed)
        (advance-player (:next-winner match) winner-seed bracket number :winner)
        (advance-player (:next-loser match)  loser-seed  bracket number :loser)))))

;; ------------------------
;; Undoing match results
;; ------------------------

(defn- retract-player
  "Reset a player's destination slot back to :TBD, undoing an advance.
   Returns tournament unchanged if next-ref is nil.

   Args:
     tournament   - the full tournament map
     next-ref     - map {:bracket :WB/:LB/:GF :number N} pointing to the destination match,
                    or nil
     from-bracket - bracket keyword of the match being undone
     from-number  - match number of the match being undone
     result-type  - :winner or :loser"
  [tournament next-ref from-bracket from-number result-type]
  (if (nil? next-ref)
    tournament
    (let [next-match  (get-match tournament (:bracket next-ref) (:number next-ref))
          from-ref    {:bracket from-bracket :number from-number :result result-type}
          player-slot (cond
                        (= from-ref (:prev-left next-match))  0
                        (= from-ref (:prev-right next-match)) 1
                        :else (throw (ex-info "Could not determine slot for retracting player"
                                              {:next-ref next-ref :from-ref from-ref})))]
      (set-match-player tournament (:bracket next-ref) (:number next-ref) player-slot :TBD))))

(defn undo-result
  "Undo the result of a completed match, clearing its winner/loser and resetting
   both players' destination slots to :TBD. Returns {:ok updated-tournament} on
   success, or {:error message} if the match hasn't been played or a downstream
   match has already been played.

   Args:
     tournament - the full tournament map
     bracket    - keyword :WB, :LB, or :GF
     number     - integer match number within that bracket"
  [tournament bracket number]
  (let [match       (get-match tournament bracket number)
        next-winner (:next-winner match)
        next-loser  (:next-loser match)]
    (cond
      (nil? (:winner match))
      {:error (format "%s %d has not been played yet." (name bracket) number)}

      (and next-winner
           (some? (:winner (get-match tournament (:bracket next-winner) (:number next-winner)))))
      {:error (format "Cannot undo: %s %d has already been played (uses the winner of %s %d)."
                      (name (:bracket next-winner)) (:number next-winner)
                      (name bracket) number)}

      (and next-loser
           (some? (:winner (get-match tournament (:bracket next-loser) (:number next-loser)))))
      {:error (format "Cannot undo: %s %d has already been played (uses the loser of %s %d)."
                      (name (:bracket next-loser)) (:number next-loser)
                      (name bracket) number)}

      :else
      {:ok (-> tournament
               (assoc-in [bracket number :winner] nil)
               (assoc-in [bracket number :loser] nil)
               (retract-player next-winner bracket number :winner)
               (retract-player next-loser  bracket number :loser))})))

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
     winner-fn  - function of [left-seed right-seed players match tournament] that
                  returns the winning seed; not called when either player is :BYE"
  [tournament bracket number winner-fn]
  (let [match       (get-match tournament bracket number)
        [left-seed right-seed] (:players match)
        winner-seed (cond
                      (= left-seed :BYE)  right-seed
                      (= right-seed :BYE) left-seed
                      :else (winner-fn left-seed right-seed (:players tournament) match tournament))]
    (record-result tournament bracket number winner-seed)))

;; ------------------------
;; Playing full tournament
;; ------------------------

(defn tournament-complete?
  "Returns true when the GF match has a winner."
  [tournament]
  (some? (:winner (get-match tournament :GF 0))))

(defn play-tournament
  "Play through the entire tournament using winner-fn to decide each match.
   Returns the completed tournament.

   Args:
     tournament - the full tournament map
     winner-fn  - function of [left-seed right-seed players match tournament] that
                  returns either the winning seed (integer) or an undo command map
                  {:command :undo :bracket bracket :number number}
     opts       - map with optional keys:
                    :bracket-order  - passed to ready-matches
                    :within-round   - passed to ready-matches
                    :after-match    - fn of [tournament] called after each match or undo"
  [tournament winner-fn opts]
  (let [after-match (get opts :after-match (fn [_] nil))]
    (loop [tournament tournament]
      (if (tournament-complete? tournament)
        tournament
        (let [next-match-ref (first (order/ready-matches tournament opts))]
          (when (nil? next-match-ref)
            (throw (ex-info "No ready match found but tournament is not complete"
                            {:tournament tournament})))
          (let [{:keys [bracket number]} next-match-ref
                match       (get-match tournament bracket number)
                [left right] (:players match)
                result      (cond
                              (= left :BYE)  right
                              (= right :BYE) left
                              :else (winner-fn left right (:players tournament) match tournament))]
            (if (and (map? result) (= (:command result) :undo))
              (let [outcome (undo-result tournament (:bracket result) (:number result))]
                (when (:error outcome)
                  (throw (ex-info "Undo failed unexpectedly after validation" outcome)))
                (let [updated (:ok outcome)]
                  (after-match updated)
                  (recur updated)))
              (let [updated (record-result tournament bracket number result)]
                (after-match updated)
                (recur updated)))))))))

;; ------------------------
;; Rankings
;; ------------------------

(defn compute-rankings
  "Returns a sequence of {:rank n :seed s} maps for a completed tournament,
   ordered from rank 1 (winner) outward.  Players eliminated in the same LB
   round share a rank; the next rank increments past all of them.

   Ranking order:
     1  — GF winner
     2  — GF loser
     3+ — LB losers, latest round first (highest LB round = eliminated last)

   BYE entries are excluded."
  [tournament]
  (let [gf-match    (get-match tournament :GF 0)
        lb-by-round (group-by :round (:LB tournament))
        lb-rounds   (sort > (keys lb-by-round))]
    (loop [rounds lb-rounds
           rank   3
           result [{:rank 1 :seed (:winner gf-match)}
                   {:rank 2 :seed (:loser  gf-match)}]]
      (if (empty? rounds)
        result
        (let [losers (filter integer? (map :loser (get lb-by-round (first rounds))))]
          (recur (rest rounds)
                 (+ rank (count losers))
                 (into result (map #(hash-map :rank rank :seed %) losers))))))))
