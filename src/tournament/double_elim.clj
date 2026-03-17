(ns tournament.double-elim)

;; ------------------------
;; Utilities
;; ------------------------

(defn next-power-of-two
  "Returns smallest power of 2 >= n"
  [n]
  (loop [p 1]
    (if (>= p n) p (recur (* 2 p)))))

(defn floor-log2
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
        rounds (int (floor-log2 slots))
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
        ;; finished: flatten rounds in round order, then wire the final match's winner to GF
        (let [wb (vec (apply concat rounds-vec))]
          (assoc-in wb [(dec (count wb)) :next-winner] {:bracket :GF :number 0}))
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

;; ------------------------
;; Drop-order helpers
;; ------------------------

;; I'm reducing the chances of rematches using different drop orders for
;; winner bracket losers, borrowed from here:
;; https://blog.start.gg/changes-in-the-world-of-brackets-695ecb777a4c

(def drop-orders
  "Cycle of drop orders used for successive winner rounds."
  [:standard :reverse :half-reverse :half-swap])

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
                                                                idx)}))
                 pairs)))

(defn set-wb-next-loser-from-lb-match
  "Given a WB vector (indexed by match number), an LB match, and a side
   keyword (:prev-left or :prev-right), returns an updated WB vector with
   :next-loser set on the referenced WB match if that side is a WB loser reference.
   If the side references an LB match or is nil, returns the WB vector unchanged."
  [wb lb-match side]
  (let [ref (get lb-match side)]
    (if (and (map? ref)
             (= :WB (:bracket ref))
             (= :loser (:result ref)))
      (assoc-in wb [(:number ref) :next-loser]
                {:bracket :LB :number (:number lb-match)})
      wb)))

(defn make-lb
  "Build a full, precomputed Losers Bracket for `n` players given `wb` (vector of WB matches).
   Returns {:WB updated-wb :LB lb-matches} with LB matches in round order and WB matches updated
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
          (let [;; Wire the final LB match's winner to Grand Finals
                ;;   (note: GF match itself is constructed separately by make-gf)
                final-lb (vec (concat lb
                                      (assoc-in (vec this-round-lb)
                                                [(dec (count this-round-lb)) :next-winner]
                                                {:bracket :GF :number 0})))
                ;; For each LB match, check both prev refs and set :next-loser on any
                ;; referenced WB match to point back to this LB match
                updated-wb (reduce (fn [wb lb-match]
                                     (-> wb
                                         (set-wb-next-loser-from-lb-match lb-match :prev-left)
                                         (set-wb-next-loser-from-lb-match lb-match :prev-right)))
                                   wb
                                   final-lb)]
            {:WB updated-wb :LB final-lb})
          (recur (inc round)
                 (concat lb this-round-lb)
                 this-round-lb))))))


;; ------------------------
;; Grand Finals
;; ------------------------

(defn make-gf
  "Creates the Grand Finals bracket — a vector containing the single GF match.
   The left player comes from the winner of the final WB match; the right player
   comes from the winner of the final LB match."
  [wb lb]
  [(assoc (make-match :GF 1 0 [:TBD :TBD])
          :prev-left  {:bracket :WB :number (:number (last wb)) :result :winner}
          :prev-right {:bracket :LB :number (:number (last lb)) :result :winner})])

;; ------------------------
;; Full Tournament
;; ------------------------

(defn make-double-elimination
  "Makes full double elimination tournament with n players.
   Returns map with keys :WB, :LB, and :GF for the winner's bracket, loser's
   bracket, and grand finals respectively. Each of those is a vector of games."
  [n]
  (let [initial-wb (make-wb n)
        wb-and-lb (make-lb initial-wb)
        grand-finals (make-gf (:WB wb-and-lb) (:LB wb-and-lb))]
    (assoc wb-and-lb
           :GF grand-finals)))


;; ------------------------
;; Not sure if I still need any of this:
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
