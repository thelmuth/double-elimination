(ns tournament.double-elim-test
  (:require [clojure.test :refer [deftest is testing]]
            [tournament.double-elim :as de]))

(deftest utils-test
  (is (= '(1 2 4 4 8 8 8 8 16 16 16 16 16 16 16 16 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 64 64 64)
         (map de/next-power-of-two (map inc (range 35)))))
  (is (= '(0 1 1 2 2 2 2 3 3 3 3 3 3 3 3 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 5 5 5 5)
         (map de/log2 (map inc (range 35))))))

(deftest seeding-test
  (testing "seeding-order"
    (is (= [1 16 8 9 4 13 5 12 2 15 7 10 3 14 6 11]
           (de/seeding-order 16)))
    (is (= [1 8 4 5 2 7 3 6]
           (de/seeding-order 8)))
    (is (= [1 8 4 5 2 7 3 6]
           (de/seeding-order 5))))
  (testing "seed-list"
    (is (= [1 16 8 9 4 13 5 12 2 15 7 10 3 14 6 11]
           (de/seed-list 16)))
    (is (= [1 8 4 5 2 7 3 6]
           (de/seed-list 8)))
    (is (= [1 :BYE 4 5 2 :BYE 3 :BYE]
           (de/seed-list 5)))
    (is (= [1 :BYE 8 9 4 :BYE 5 :BYE 2 :BYE 7 10 3 :BYE 6 :BYE]
           (de/seed-list 10)))))

(deftest make-match-test
  (is (= {:id "WB-M3"
          :round 2
          :bracket "WB"
          :players [5 2]
          :winner nil
          :loser nil
          :next-winner nil
          :next-loser nil
          :prev-left nil
          :prev-right nil}
         (de/make-match "WB-M3" 2 "WB" [5 2]))))

(deftest make-wb-test
  (is (= [(assoc (de/make-match "WB-M1" 1 "WB" [1 :BYE]) :next-winner "WB-M3")
          (assoc (de/make-match "WB-M2" 1 "WB" [2 3]) :next-winner "WB-M3")
          (assoc (de/make-match "WB-M3" 2 "WB" [:TBD :TBD])
                 :prev-left "WB-M1-winner"
                 :prev-right "WB-M2-winner")]
         (de/make-wb 3)))
  
  (is (= [(assoc (de/make-match "WB-M1" 1 "WB" [1 :BYE]) :next-winner "WB-M5")
          (assoc (de/make-match "WB-M2" 1 "WB" [4 5]) :next-winner "WB-M5")
          (assoc (de/make-match "WB-M3" 1 "WB" [2 :BYE]) :next-winner "WB-M6")
          (assoc (de/make-match "WB-M4" 1 "WB" [3 :BYE]) :next-winner "WB-M6")

          (assoc (de/make-match "WB-M5" 2 "WB" [:TBD :TBD])
                 :next-winner "WB-M7"
                 :prev-left "WB-M1-winner"
                 :prev-right "WB-M2-winner")
          (assoc (de/make-match "WB-M6" 2 "WB" [:TBD :TBD])
                 :next-winner "WB-M7"
                 :prev-left "WB-M3-winner"
                 :prev-right "WB-M4-winner")
          
          (assoc (de/make-match "WB-M7" 3 "WB" [:TBD :TBD])
                 :prev-left "WB-M5-winner"
                 :prev-right "WB-M6-winner")]
         (de/make-wb 5))))


;; TODO: Make match :ids into maps, with following keys:
;; ACTUALLY: get rid of :id, and replace with :number
;; all matches already have a :bracket and a :round.
;;  {:bracket :number}
;; can add :result for prev-left and prev-right, to identify the winner or loser
;; For example, :prev-left might be {:bracket :WB :number 5 :result :winner}
;; to indicate that the winner from winners bracket match 5 is the left player
;; Also, change all :bracket to :WB or :LB instead of strings