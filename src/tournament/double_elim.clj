(ns tournament.double-elim)

;; Notes for future:
;; - when adapting for music tourney, make it so that players are provided by a CSV
;;   that is automatically converted to a map with a key for each column, and also
;;   provide a function that takes a player map and returns a string representation
;;   That way, could be used for different types of tournaments


;; ------------------------
;; Utilities
;; ------------------------

(defn next-power-of-two
  "Returns smallest power of 2 >= n"
  [n]
  (loop [p 1]
    (if (>= p n) p (recur (* 2 p)))))

(defn log2
  "Return floor(log2 x). Assumes x is positive."
  [x]
  (loop [value 1
         log -1]
    (if (> value x)
      log
      (recur (* value 2)
             (inc log)))))

;; ------------------------
;; Make seeding list including byes
;; ------------------------

(defn seeding-order
  "Return the *standard* seeding order for a bracket of size `n` (n must be a power of two).
   Example:
     (seeding-order 2)  => [1 2]
     (seeding-order 4)  => [1 4 2 3]
     (seeding-order 8)  => [1 8 4 5 2 7 3 6]
     (seeding-order 16) => [1 16 8 9 4 13 5 12 2 15 7 10 3 14 6 11]

   This uses the standard expansion:
     start with [1 2], then repeatedly replace each d with [d, (len+1 - d)]
   (see algorithm referenced on StackOverflow)."
  [n]
  (loop [pls  [1 2]]
    (if (>= (count pls) n)
      (vec pls)
      (let [len (inc (* 2 (count pls)))   ; length == (2*count(pls) + 1)
            out (mapcat (fn [d]
                          [d (- len d)])
                        pls)]
        (recur out)))))

(defn seed-list
  "Return a vector of length `slots` (next power of two >= n) containing seeds 1..n
   placed into standard bracket positions; remaining slots are :bye.
   Example: (seed-list 13) -> 16-length vector with seeds and :bye placed."
  [n]
  (map #(if (> % n)
          :BYE
          %)
       (seeding-order n)))

(defn initial-wb-pairs
  "Return the initial WB matchups as a vector of [p1 p2] pairs in order."
  [n]
  (partition 2 (seed-list n)))

;; ------------------------
;; Match creation
;; ------------------------

(defn make-match
  "Makes a map to store a single match. Explanation of keys:
   :id - string id of this match. Should be WB-Mn for the nth match in the winner's bracket, LB-Mn for the nth match in the loser's bracket, and GF for grand finals
   :round - integer round of this bracket
   :bracket - string WB, LB, or GF
   :players - tuple of 2 players of this match
   :winner - winner of the match. Set to nil until played
   :loser - loser of the match. Set to nil until played
   :next-winner - The ID of the match to which the winner of this match should advance.
   :next-loser - The ID of the match to which the loser of this match should advance.
   :prev-left - The ID of the match from which the first player of the game came, concatenated with winner or loser. Examples: WB-G4-winner, WB-G2-loser, LB-G3-winner
   :prev-right - Same as above, except for the second player
   "
  [bracket round number players]
  {:number number
   :round round
   :bracket bracket
   :players players
   :winner nil
   :loser nil
   :next-winner nil
   :next-loser nil
   :prev-left nil
   :prev-right nil})

;; ------------------------
;; Winner’s Bracket
;; ------------------------

(defn make-wb
  "Construct the winner's bracket using standard seeding.
   Returns a vector of match maps in round order."
  [n]
  (let [slots (next-power-of-two n)
        rounds (int (log2 slots))
        pairs (initial-wb-pairs n)
        ;; build round-1 matches and give them IDs "WB-M1" .. "WB-Mk"
        round1 (mapv (fn [i p] 
                       (make-match :WB 1 i (vec p)))
                     (range)
                     pairs)
        ;; starting counter for the next new match id
        start-counter (count round1)]
    ;; Iterate rounds 2..rounds, building matches and wiring :prev-* and :next-winner.
    (loop [r 2
           rounds-vec [round1]       ;; vector of round-vectors; index 0 == round1
           num-prior-matches 0
           counter start-counter]
      (if (> r rounds)
        ;; finished: flatten rounds in round order and return vector of maps
        (vec (apply concat rounds-vec))
        (let [prev (peek rounds-vec)          ;; previous round matches (vector)
              prev-count (count prev)
              matches-in-round (quot prev-count 2)
              init {:updated-prev prev
                    :new-round []
                    :counter counter}

              ;; reduce over the pairs of previous-round matches to:
              ;;  - create the new matches for this round
              ;;  - update the previous-round matches with :next-winner => new-id
              {:keys [updated-prev new-round]}
              (reduce
               (fn [acc j]
                 (let [uprev (:updated-prev acc)
                       left-idx (* 2 j)
                       right-idx (inc (* 2 j))
                       left (nth uprev left-idx)
                       right (nth uprev right-idx)
                       number (:counter acc)
                       prev-left {:bracket :WB :number (+ num-prior-matches left-idx) :result :winner}
                       prev-right {:bracket :WB :number (+ num-prior-matches right-idx) :result :winner}
                       new-match (-> (make-match :WB r number [:TBD :TBD])
                                     (assoc :prev-left prev-left :prev-right prev-right))
                       next-winner {:bracket :WB :number number}
                       uprev' (-> uprev
                                  (assoc left-idx (assoc left :next-winner next-winner))
                                  (assoc right-idx (assoc right :next-winner next-winner)))]
                   {:updated-prev uprev'
                    :new-round (conj (:new-round acc) new-match)
                    :counter (inc (:counter acc))}))
               init
               (range matches-in-round))

              ;; replace the last rounds-vec element with updated-prev (so previous round now has next-winner fields)
              rounds-vec' (assoc rounds-vec (dec (count rounds-vec)) updated-prev)
              ;; append the newly-built round
              rounds-vec'' (conj rounds-vec' new-round)]
          (recur (inc r)
                 rounds-vec'' 
                 counter
                 (+ counter matches-in-round)))))))



(comment

  (map #(vector % (log2 %)) (range 1 20))

  (Math/log 15)

  (initial-wb-pairs 5)


  (make-wb 5)

  (make-wb 13)


  (make-wb 3)

  )

;; ------------------------
;; Drop-order helpers
;; ------------------------

(def drop-orders
  "Cycle of drop orders used for successive winner rounds."
  [:reverse :half-reverse :half-swap :standard])

(defn drop-order-for-wb-round
  "Return the drop-order for a given winners-round index (1-based)."
  [wb-round-num]
  (nth drop-orders
       (mod (dec wb-round-num)
            (count drop-orders))))

(defn apply-drop-order
  "Reorder a vector `v` according to `order` keyword."
  [order v]
  (case order
    :standard v
    :reverse (vec (reverse v))
    :half-reverse (let [half (quot (count v) 2)
                        a (subvec (vec v) 0 half)
                        b (subvec (vec v) half)]
                    (vec (concat (reverse a) (reverse b))))
    :half-swap (let [half (quot (count v) 2)]
                 (vec (concat (subvec (vec v) half) (subvec (vec v) 0 half))))
    v))

;; ------------------------
;; Losers Bracket
;; ------------------------

(defn make-lb-round-1
  "Create round 1 of Losers Bracket based on round 1 of Winners Bracket"
  [wb-round-1]
  (map-indexed (fn [idx [left right]]
                 (assoc (make-match :LB 1 idx [:TBD :TBD])
                        :prev-left {:bracket :WB :number (:number left) :result :loser}
                        :prev-right {:bracket :WB :number (:number right) :result :loser}
                        :next-winner {:bracket :LB :number (+ (quot (count wb-round-1) 2)
                                                              idx)})) ;; don't divide by 2 since next round is against drop downs from winner's
               (partition 2 wb-round-1)))

(defn make-even-round-lb
  "Create an even round of the losers bracket, where each match is a pairing of
   a player dropped down from the winners bracket (who lost) [left player]
   and a player who just won a game in the losers bracket [right player]"
  [last-round-lb wb-by-round lb-round-num]
  (let [wb-drop-down-round-num (inc (quot lb-round-num 2))
        wb-round (get wb-by-round wb-drop-down-round-num)
        wb-round-drop-ordered (apply-drop-order (drop-order-for-wb-round wb-drop-down-round-num)
                                                wb-round)
        pairs (partition 2 (interleave wb-round-drop-ordered last-round-lb))
        start-match-number (inc (:number (last last-round-lb)))]
    (map-indexed (fn [idx [left right]]
                   (assoc (make-match :LB lb-round-num (+ start-match-number idx) [:TBD :TBD])
                          :prev-left {:bracket :WB :number (:number left) :result :loser}
                          :prev-right {:bracket :LB :number (:number right) :result :winner}
                          :next-winner {:bracket :LB :number (+ start-match-number
                                                                (count pairs)
                                                                (quot idx 2))}))
                 pairs)))

(defn make-odd-round-lb
  "Create an odd round of the losers bracket, where each match is a pairing of
   a two players that just won in the previous round of the losers bracket."
  [last-round-lb lb-round-num]
  (let [pairs (partition 2 last-round-lb)
        start-match-number (inc (:number (last last-round-lb)))]
    (map-indexed (fn [idx [left right]]
                   (assoc (make-match :LB lb-round-num (+ start-match-number idx) [:TBD :TBD])
                          :prev-left {:bracket :LB :number (:number left) :result :winner}
                          :prev-right {:bracket :LB :number (:number right) :result :winner}
                          :next-winner {:bracket :LB :number (+ start-match-number
                                                                (count pairs)
                                                                (quot idx 2))}))
                 pairs)))

;; TO DO: 
;; - update winner bracket places going
;; - test full brackets and winners brackets on larger tournaments


(defn make-lb
  "Build a full, precomputed Losers Bracket for `n` players given `wb` (vector of WB matches).
   Returns {:wb updated-wb :lb lb-matches} with LB matches in round order and WB matches updated
   with :next-loser where appropriate."
  [wb]
  (let [wb-by-round (group-by :round wb)]
    (loop [round 1
           lb '()
           last-round-lb nil]
      (let [this-round-lb
            (cond
              (= 1 round) (make-lb-round-1 (get wb-by-round 1))
              (odd? round) (make-odd-round-lb last-round-lb round)
              (even? round) (make-even-round-lb last-round-lb wb-by-round round))]
        (if (and (even? round)
                 (= 1 (count this-round-lb)))
          {:wb wb
           :lb (concat lb
                       (assoc-in (vec this-round-lb)
                                 [(dec (count this-round-lb)) :next-winner]
                                 {:bracket :GF :number 0}))}
          (recur (inc round)
                 (concat lb this-round-lb)
                 this-round-lb))))))


(comment

  (let [players 9]
    (make-lb (make-wb players)))

  (group-by :round
            (make-wb 8))

  )




;;;;; new Losers bracket

;; ------------------------
;; Drop-order helpers
;; ------------------------

;; (def drop-orders
;;   "Cycle of drop orders used for successive winner rounds."
;;   [:standard :reverse :half-reverse :half-swap])

;; (defn apply-drop-order
;;   "Reorder a vector `v` according to `order` keyword."
;;   [order v]
;;   (case order
;;     :standard v
;;     :reverse (vec (reverse v))
;;     :half-reverse
;;       (let [half (quot (count v) 2)
;;             a (subvec (vec v) 0 half)
;;             b (subvec (vec v) half)]
;;         (vec (concat (reverse a) (reverse b))))
;;     :half-swap
;;       (let [half (quot (count v) 2)]
;;         (vec (concat (subvec (vec v) half) (subvec (vec v) 0 half))))
;;     v))

;; (defn drop-order-for-wb-round
;;   "Return the drop-order for a given winners-round index (1-based)."
;;   [wb-round]
;;   (nth drop-orders (mod (dec wb-round) (count drop-orders))))

;; ------------------------
;; Utility: group WB matches by round in order
;; ------------------------

;; (defn wb-by-round
;;   "Return a map from round number -> vector of WB matches in the same order they appear
;;    in the `wb` vector. (If a round has no matches, key may be absent.)"
;;   [wb]
;;   (->> wb
;;        (group-by :round)
;;        (map (fn [[k v]] [k (vec v)]))
;;        (into {}))) ;; map of int -> vector

;; ------------------------
;; Core: make-lb (pure)
;; ------------------------

(defn wb-by-round-ordered
  "Return a map of round -> vector of WB matches in the same order they appear in `wb`."
  [wb]
  (let [max-round (apply max (map :round wb))]
    (into {}
          (for [r (range 1 (inc max-round))]
            [r (vec (filter #(= (:round %) r) wb))]))))

(defn wb-by-round
  "Group WB matches by round number: {1 [...], 2 [...], ...}."
  [wb]
  (group-by :round wb))


(comment
  
  
  (wb-by-round-ordered (make-wb 6))

  (wb-by-round (make-wb 6))
  
  )            

(defn drop-order-for-wb-round
  "Return the drop-order for a given winners round (1-based)."
  [r]
  (let [drop-orders [:standard :reverse :half-reverse :half-swap]]
    (nth drop-orders (mod (dec r) (count drop-orders)))))

(defn apply-drop-order
  "Apply the named drop-order to a vector of losers (strings)."
  [order v]
  (case order
    :standard v
    :reverse (vec (reverse v))
    :half-reverse (let [half (quot (count v) 2)
                        a (subvec (vec v) 0 half)
                        b (subvec (vec v) half)]
                    (vec (concat (reverse a) (reverse b))))
    :half-swap (let [half (quot (count v) 2)]
                 (vec (concat (subvec (vec v) half) (subvec (vec v) 0 half))))
    v))

(defn replace-last-n
  "Replace last n elements of coll with new-subvec."
  [coll n new-subvec]
  (let [m (- (count coll) n)]
    (vec (concat (subvec (vec coll) 0 m) new-subvec))))

(defn make-lb-most-recent-chatgpt
  "Build a *full*, precomputed Losers Bracket for `n` players given `wb` (vector of WB matches).
   Returns {:wb updated-wb :lb lb-matches} with LB matches in round order and WB matches updated
   with :next-loser where appropriate."
  [n wb]
  (let [slots (next-power-of-two n)
        winners-rounds (int (log2 slots))
        winners-by-round (wb-by-round-ordered wb)

        ;; helper: losers strings for WB round r in order, e.g. [\"WB-M1-loser\" ...]
        losers-for-wb-round (fn [r]
                              (->> (get winners-by-round r [])
                                   (mapv (fn [m] (str (:id m) "-loser")))))

        ;; helper: update a single WB match's :next-loser (pure)
        update-wb-next-loser (fn [wb-vec wb-id lb-id]
                               (mapv (fn [m]
                                       (if (= (:id m) wb-id)
                                         (assoc m :next-loser lb-id)
                                         m))
                                     wb-vec))]

    ;; iterative build over WB rounds 1..winners-rounds
    (loop [wb-upd wb                ; updated WB vector
           lb-flat []               ; accumulated LB matches (flat)
           lb-next-id 1             ; next LB numeric id
           prev-lb-round []         ; vector of match maps created in previous LB round
           r 1]                     ; current WB round that we are consuming (1..winners-rounds)
      (if (<= winners-rounds 0)
        {:wb wb-upd :lb []}
        (if (<= r winners-rounds)
          ;; Process WB round r: pair prev-lb winners (if any) with WB r losers (ordered by drop-order)
          (let [wb-losers (losers-for-wb-round r)
                ordered-losers (apply-drop-order (drop-order-for-wb-round r) wb-losers)
                prev-winner-strings (mapv (fn [m] (str (:id m) "-winner")) prev-lb-round)
                p (count prev-winner-strings)
                q (count ordered-losers)
                minpq (min p q)

                ;; 1) pair prev_winner[i] with wb_loser[i] for i < min(p,q)
                paired-indices (range minpq)

                ;; build matches for paired indices
                [pairs-made lb-id-after-pairs]
                (reduce (fn [[acc next-id] i]
                          (let [left (nth prev-winner-strings i)
                                right (nth ordered-losers i)
                                id (str "LB-M" next-id)
                                match (-> (make-match id r "LB" [:TBD :TBD])
                                          (assoc :prev-left left :prev-right right))]
                            [(conj acc match) (inc next-id)]))
                        [[] lb-next-id]
                        paired-indices)

                ;; 2) handle leftover prev-winners (if p > q): pair them among themselves sequentially
                leftover-prev (subvec prev-winner-strings minpq p)

                [pairs-from-leftover-prev lb-id-after-leftover]
                (loop [idx 0 
                       acc '()
                       pairs-made-next lb-id-after-pairs]
                  (if (>= idx (count leftover-prev))
                    [acc pairs-made-next]
                    (if (>= (inc idx) (count leftover-prev))
                      ;; single leftover prev-winner — carry it forward (we'll handle as a winner string next round)
                      [acc pairs-made-next]
                      (let [left (nth leftover-prev idx)
                            right (nth leftover-prev (inc idx))
                            id (str "LB-M" pairs-made-next)
                            match (-> (make-match id r "LB" [:TBD :TBD])
                                      (assoc :prev-left left :prev-right right))]
                        (recur (+ idx 2) (conj acc match) (inc pairs-made-next))))))

                ;; 3) handle leftover wb-losers (if q > p): these we *do not pair among themselves* for r >= 2
                ;;    instead they will be carried forward and appended to the losers of the next WB round.
                leftover-wb (subvec ordered-losers minpq q)

                ;; combine new matches for this round
                new-matches (vec (concat pairs-made pairs-from-leftover-prev))

                ;; 4) Update WB :next-loser for any WB loser that was consumed in new-matches
                wb-upd-1
                (reduce
                 (fn [acc lbm]
                   ;; for each side (:prev-left, :prev-right) update acc if it's a WB-loser ref
                   (reduce
                    (fn [acc' side]
                      (let [ref (side lbm)]
                        (if (and (string? ref) (.endsWith ref "-loser"))
                          (let [wb-id (subs ref 0 (- (count ref) 6))]
                            (update-wb-next-loser acc' wb-id (:id lbm)))
                          acc')))
                    acc
                    [:prev-left :prev-right]))
                 wb-upd
                 new-matches)

                ;; 5) Wire previous LB matches' :next-winner to these new matches:
                ;; For the i < min(p,q) paired index, prev-lb-round[i] -> new-matches[i]
                ;; For leftover-prev pairs we paired indices relative to leftover; we must map them as well.
                prev-count (count prev-lb-round)
                ;; build updated prev-lb-round (set :next-winner when we know target)
                updated-prev-lb-round
                (if (empty? prev-lb-round)
                  []
                  (let [ ;; prefill with original prev matches
                        orig prev-lb-round
                        ;; mapping for indices 0..(minpq-1) -> new match ids at positions 0..(minpq-1)
                        mapping-min (into {} (map (fn [i] [i (:id (nth new-matches i))]) (range (min prev-count (count new-matches)))))
                        ;; mapping for leftover-prev if any: they were paired starting at index minpq.
                        leftover-start minpq
                        ;; compute number of pairs from leftover prev
                        leftover-pairs-count (quot (count leftover-prev) 2)
                        mapping-leftover
                        (into {}
                              (map-indexed
                               (fn [idx m]
                                 ;; each pair of leftover-prev corresponds to new-matches index (minpq + idx)
                                 (let [pair-idx (+ minpq idx)
                                       base-idx (* 2 idx)
                                       left-index (+ leftover-start base-idx)
                                       right-index (inc left-index)
                                       target-id (:id (nth new-matches (+ minpq idx)))]
                                   {left-index target-id right-index target-id}))
                               (range leftover-pairs-count)))]
                    ;; merge mappings and apply
                    (mapv (fn [i m]
                            (if-let [tid (or (get mapping-min i) (get mapping-leftover i))]
                              (assoc m :next-winner tid)
                              m))
                          (range) orig)))
                ;; 6) update lb-flat (replace last prev-count elements with updated-prev-lb-round, then conj new-matches)
                lb-flat-upd (if (zero? (count prev-lb-round))
                              (vec (concat lb-flat new-matches))
                              (let [lf (replace-last-n lb-flat prev-count updated-prev-lb-round)]
                                (vec (concat lf new-matches))))

                ;; 7) prepare prev-lb-round for next iteration: winners of new-matches (as match maps)
                new-prev-lb-round new-matches

                ;; 8) prepare next iteration: the losers from the *next* WB round will be appended in next loop.
                ;;    But we must carry leftover-wb (if any) forward: we will prepend it to the next WB round's losers list
                next-wb (inc r)]

            ;; Recur to process the next winners round, carrying the updated state.
            (recur wb-upd-1 lb-flat-upd lb-id-after-leftover new-prev-lb-round next-wb))

          ;; After we've processed all winners rounds (r > winners-rounds),
          ;; reduce the remaining prev-lb-round by pairing them among themselves until 1 remains.
          (letfn [(reduce-prev-round
                    [lb-flat prev-round next-id round-num]
                    (if (<= (count prev-round) 1)
                      {:lb-flat lb-flat :final-prev prev-round :next-id next-id}
                      (let [pairs (partition 2 prev-round)
                            [new-matches next-id']
                            (reduce (fn [[acc nid] [pleft pright]]
                                      (let [id (str "LB-M" nid)
                                            left-str (str (:id pleft) "-winner")
                                            right-str (str (:id pright) "-winner")
                                            m (-> (make-match id round-num "LB" [:TBD :TBD])
                                                  (assoc :prev-left left-str :prev-right right-str))]
                                        [(conj acc m) (inc nid)]))
                                    [[] next-id] pairs)
                            ;; set :next-winner for each prev pleft/pright to the corresponding new-match id
                            updated-prev (mapv (fn [[p nm]]
                                                 (assoc p :next-winner (:id nm)))
                                               (map vector prev-round new-matches))
                            lb-flat' (-> lb-flat
                                         (replace-last-n (count prev-round) updated-prev)
                                         (into new-matches))]
                        (recur lb-flat' new-matches next-id' (inc round-num)))))]
            (let [{:keys [lb-flat final-prev next-id]} (reduce-prev-round lb-flat prev-lb-round lb-next-id (inc winners-rounds))]
              {:wb wb-upd :lb lb-flat})))))))





(comment

  (def brackets
    (let [players 8]
      (make-lb players (make-wb players))))
  
  ;; works for 4 players
  ;; broken for 5 players? only works with powers of 2 passed to make-lb?

  brackets


  
  
  )



;;;;;;;;; PROBABLY BROKEN BELOW HERE

;; ------------------------
;; Loser’s Bracket
;; ------------------------

;; (defn lb-round-count
;;   "Number of loser's bracket rounds for n players."
;;   [n]
;;   (let [slots (next-power-of-two n)
;;         wb-rounds (int (log2 slots))]
;;     ;; LB has (2 * WB-rounds - 1) rounds (including LB final)
;;     (dec (* 2 wb-rounds))))

;; (defn make-lb
;;   "Construct the loser's bracket given WB matches.
;;    Returns a vector of LB matches up to LB Final."
;;   [wb]
;;   (let [wb-rounds (wb-by-round wb)
;;         total-lb-rounds (lb-round-count (count (mapcat :players (wb 0))))
;;         total-lb-rounds (lb-round-count
;;                          (count
;;                           (set
;;                            (mapcat :players wb)))) ;; safer calculation
;;         total-lb-rounds (lb-round-count (count (:players (first wb)))) ;; fallback
;;         total-lb-rounds (lb-round-count 16) ;; we'll override properly below anyway
;;         total-lb-rounds (dec (* 2 (count wb-rounds))) ;; simpler
;;         counter (atom 1)]

;;     ;; Generate LB matches round by round
;;     (loop [r 1
;;            matches []]
;;       (if (> r total-lb-rounds)
;;         matches
;;         (let [round (keyword (str "LB-R" r))
;;               entering-wb-round (if (odd? r)
;;                                   (inc (/ r 2))
;;                                   (/ r 2))
;;               losers (wb-rounds entering-wb-round)
;;               entering-players (map #(keyword (str "Loser-" (:id %))) losers)
;;               num-matches (max 1 (int (Math/ceil (/ (count entering-players) 2))))
;;               pairs (partition-all 2 entering-players)
;;               ;; fill up with :TBD for the opponents from previous LB round
;;               pairs (mapv (fn [p]
;;                             (if (= r 1)
;;                               p
;;                               (if (= (count p) 1)
;;                                 [(first p) :TBD]
;;                                 p)))
;;                           pairs)
;;               round-matches
;;               (map-indexed
;;                (fn [i p]
;;                  (let [id (keyword (str "LB" (swap! counter inc)))]
;;                    (make-match id round :LB (vec p))))
;;                pairs)]
;;           (recur (inc r) (into matches round-matches)))))))

;; ;; ------------------------
;; ;; Final
;; ;; ------------------------

;; (defn make-final
;;   "Create the Grand Final match."
;;   []
;;   (make-match :GF :GF :Final [:TBD :TBD]))

;; ;; ------------------------
;; ;; Tournament
;; ;; ------------------------

;; (defn make-tournament
;;   "Create a double-elimination tournament data structure for n players.
;;    Returns a map with :players, :winners, :losers, and :final."
;;   [n]
;;   (let [wb (make-wb n)
;;         lb (make-lb wb)
;;         final (make-final)]
;;     {:players n
;;      :winners wb
;;      :losers lb
;;      :final final}))


(comment

  (seeding-order 13)

  (seed-list 8)

  (seed-list 5)

  (seed-list 13)

  (initial-wb-pairs 13)

  (make-wb 5)


  (make-tournament 5)

  (make-tournament 13))