(ns tournament.play-order)

;; ------------------------
;; Ready-to-play predicate
;; ------------------------

(defn ready-to-play?
  "Returns true if both players are known (not :TBD) and the match hasn't been played yet."
  [match]
  (and (nil? (:winner match))
       (not= :TBD (first (:players match)))
       (not= :TBD (second (:players match)))))

;; ------------------------
;; Sort key functions
;; ------------------------

(defn- wb-first-sort-key
  "Sort key for :wb-first ordering: all WB rounds, then all LB rounds, then GF.
   Returns [bracket-order round number]."
  [match]
  [(case (:bracket match) :WB 0 :LB 1 :GF 2)
   (:round match)
   (:number match)])

(defn- interleaved-sort-key
  "Sort key for :interleaved ordering: WB round N is grouped with LB rounds 2N-1 and 2N.
   Returns [wave sub-order number], where sub-order is 0 for WB, 1 for odd LB, 2 for even LB."
  [match]
  (let [round (:round match)]
    (case (:bracket match)
      :WB [round 0 (:number match)]
      :LB [(quot (inc round) 2) (if (odd? round) 1 2) (:number match)]
      :GF [Integer/MAX_VALUE 3 (:number match)])))

;; ------------------------
;; Ready matches
;; ------------------------

(defn ready-matches
  "Returns a sequence of ready-to-play matches from the tournament, ordered by opts.

   A match is ready when both players are known (not :TBD) and it hasn't been played yet.

   opts:
     :bracket-order - :wb-first (all WB rounds, then all LB rounds, then GF) or
                      :interleaved (WB round N grouped with LB rounds 2N-1 and 2N).
                      Default: :interleaved
     :within-round  - :in-order (matches in their natural order within each round) or
                      :random (matches shuffled within each round).
                      Default: :in-order"
  [tournament {:keys [bracket-order within-round]
               :or   {bracket-order :interleaved
                      within-round  :in-order}}]
  (let [all-matches  (concat (:WB tournament) (:LB tournament) (:GF tournament))
        ready        (filter ready-to-play? all-matches)
        sort-key-fn  (case bracket-order
                       :wb-first    wb-first-sort-key
                       :interleaved interleaved-sort-key)
        ;; Group by [wave sub-order] (first two elements of the sort key) so that
        ;; :within-round ordering is applied independently within each round/wave.
        sorted-groups (sort-by first (group-by #(vec (take 2 (sort-key-fn %))) ready))]
    (mapcat (fn [[_ matches]]
              (if (= within-round :random)
                (shuffle matches)
                (sort-by sort-key-fn matches)))
            sorted-groups)))
