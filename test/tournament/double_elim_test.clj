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
  (is (= {:bracket :WB
          :round 2
          :number 3
          :players [5 2]
          :winner nil
          :loser nil
          :next-winner nil
          :next-loser nil
          :prev-left nil
          :prev-right nil}
         (de/make-match :WB 2 3 [5 2]))))

(deftest make-wb-test
  (is (= [(assoc (de/make-match :WB 1 0 [1 :BYE]) :next-winner {:bracket :WB :number 2})
          (assoc (de/make-match :WB 1 1 [2 3]) :next-winner {:bracket :WB :number 2})
          (assoc (de/make-match :WB 2 2 [:TBD :TBD])
                 :prev-left {:bracket :WB :number 0 :result :winner}
                 :prev-right {:bracket :WB :number 1 :result :winner})]
         (de/make-wb 3)))

  (is (= [(assoc (de/make-match :WB 1 0 [1 4]) :next-winner {:bracket :WB :number 2})
          (assoc (de/make-match :WB 1 1 [2 3]) :next-winner {:bracket :WB :number 2})
          (assoc (de/make-match :WB 2 2 [:TBD :TBD])
                 :prev-left {:bracket :WB :number 0 :result :winner}
                 :prev-right {:bracket :WB :number 1 :result :winner})]
         (de/make-wb 4)))

  (is (= [(assoc (de/make-match :WB 1 0 [1 :BYE]) :next-winner {:bracket :WB :number 4})
          (assoc (de/make-match :WB 1 1 [4 5]) :next-winner {:bracket :WB :number 4})
          (assoc (de/make-match :WB 1 2 [2 :BYE]) :next-winner {:bracket :WB :number 5})
          (assoc (de/make-match :WB 1 3 [3 :BYE]) :next-winner {:bracket :WB :number 5})
          ;; round 2
          (assoc (de/make-match :WB 2 4 [:TBD :TBD])
                 :next-winner {:bracket :WB :number 6}
                 :prev-left {:bracket :WB :number 0 :result :winner}
                 :prev-right {:bracket :WB :number 1 :result :winner})
          (assoc (de/make-match :WB 2 5 [:TBD :TBD])
                 :next-winner {:bracket :WB :number 6}
                 :prev-left {:bracket :WB :number 2 :result :winner}
                 :prev-right {:bracket :WB :number 3 :result :winner})
          ;; round 3
          (assoc (de/make-match :WB 3 6 [:TBD :TBD])
                 :prev-left {:bracket :WB :number 4 :result :winner}
                 :prev-right {:bracket :WB :number 5 :result :winner})]
         (de/make-wb 5)))

  (is (= [(assoc (de/make-match :WB 1 0 [1 :BYE]) :next-winner {:bracket :WB :number 8})
          (assoc (de/make-match :WB 1 1 [8 9]) :next-winner {:bracket :WB :number 8})
          (assoc (de/make-match :WB 1 2 [4 13]) :next-winner {:bracket :WB :number 9})
          (assoc (de/make-match :WB 1 3 [5 12]) :next-winner {:bracket :WB :number 9})
          (assoc (de/make-match :WB 1 4 [2 :BYE]) :next-winner {:bracket :WB :number 10})
          (assoc (de/make-match :WB 1 5 [7 10]) :next-winner {:bracket :WB :number 10})
          (assoc (de/make-match :WB 1 6 [3 :BYE]) :next-winner {:bracket :WB :number 11})
          (assoc (de/make-match :WB 1 7 [6 11]) :next-winner {:bracket :WB :number 11})
          ;; round 2
          (assoc (de/make-match :WB 2 8 [:TBD :TBD])
                 :next-winner {:bracket :WB :number 12}
                 :prev-left {:bracket :WB :number 0 :result :winner}
                 :prev-right {:bracket :WB :number 1 :result :winner})
          (assoc (de/make-match :WB 2 9 [:TBD :TBD])
                 :next-winner {:bracket :WB :number 12}
                 :prev-left {:bracket :WB :number 2 :result :winner}
                 :prev-right {:bracket :WB :number 3 :result :winner})
          (assoc (de/make-match :WB 2 10 [:TBD :TBD])
                 :next-winner {:bracket :WB :number 13}
                 :prev-left {:bracket :WB :number 4 :result :winner}
                 :prev-right {:bracket :WB :number 5 :result :winner})
          (assoc (de/make-match :WB 2 11 [:TBD :TBD])
                 :next-winner {:bracket :WB :number 13}
                 :prev-left {:bracket :WB :number 6 :result :winner}
                 :prev-right {:bracket :WB :number 7 :result :winner})
          ;; round 3
          (assoc (de/make-match :WB 3 12 [:TBD :TBD])
                 :next-winner {:bracket :WB :number 14}
                 :prev-left {:bracket :WB :number 8 :result :winner}
                 :prev-right {:bracket :WB :number 9 :result :winner})
          (assoc (de/make-match :WB 3 13 [:TBD :TBD])
                 :next-winner {:bracket :WB :number 14}
                 :prev-left {:bracket :WB :number 10 :result :winner}
                 :prev-right {:bracket :WB :number 11 :result :winner})
          ;; round 4
          (assoc (de/make-match :WB 4 14 [:TBD :TBD])
                 :prev-left {:bracket :WB :number 12 :result :winner}
                 :prev-right {:bracket :WB :number 13 :result :winner})]
         (de/make-wb 13))))

(deftest drop-order-for-wb-round-test
  (is (= :reverse
         (de/drop-order-for-wb-round 2)))
  (is (= :half-reverse
         (de/drop-order-for-wb-round 3)))
  (is (= :half-swap
         (de/drop-order-for-wb-round 8))))

(deftest apply-drop-order-test
  (let [eight (vec (range 1 9))]
    (is (= [8 7 6 5 4 3 2 1]
           (de/apply-drop-order :reverse eight)))
    (is (= [1 2 3 4 5 6 7 8]
           (de/apply-drop-order :standard eight)))
    (is (= [4 3 2 1 8 7 6 5]
           (de/apply-drop-order :half-reverse eight)))
    (is (= [5 6 7 8 1 2 3 4]
           (de/apply-drop-order :half-swap eight)))))

(deftest make-lb-test
  (testing "first round of losers bracket"
    ;; test 4 players
    (let [num-players 4
          {:keys [lb]} (de/make-lb (de/make-wb num-players))]
      (is (= [(assoc (de/make-match :LB 1 0 [:TBD :TBD]) ;; LB round 1
                     :prev-left {:bracket :WB :number 0 :result :loser}
                     :prev-right {:bracket :WB :number 1 :result :loser}
                     :next-winner {:bracket :LB :number 1})]
             (vec (take 1 lb)))))

    ;; test 8 players
    (let [num-players 8
          {:keys [lb]} (de/make-lb (de/make-wb num-players))]
      (is (= [(assoc (de/make-match :LB 1 0 [:TBD :TBD]) ;; LB round 1
                     :prev-left {:bracket :WB :number 0 :result :loser}
                     :prev-right {:bracket :WB :number 1 :result :loser}
                     :next-winner {:bracket :LB :number 2})
              (assoc (de/make-match :LB 1 1 [:TBD :TBD])
                     :prev-left {:bracket :WB :number 2 :result :loser}
                     :prev-right {:bracket :WB :number 3 :result :loser}
                     :next-winner {:bracket :LB :number 3})]
             (vec (take 2 lb)))))

    ;; test 13 players
    (let [num-players 13
          {:keys [lb]} (de/make-lb (de/make-wb num-players))]
      (is (= [(assoc (de/make-match :LB 1 0 [:TBD :TBD]) ;; LB round 1
                     :prev-left {:bracket :WB :number 0 :result :loser}
                     :prev-right {:bracket :WB :number 1 :result :loser}
                     :next-winner {:bracket :LB :number 4})
              (assoc (de/make-match :LB 1 1 [:TBD :TBD])
                     :prev-left {:bracket :WB :number 2 :result :loser}
                     :prev-right {:bracket :WB :number 3 :result :loser}
                     :next-winner {:bracket :LB :number 5})
              (assoc (de/make-match :LB 1 2 [:TBD :TBD])
                     :prev-left {:bracket :WB :number 4 :result :loser}
                     :prev-right {:bracket :WB :number 5 :result :loser}
                     :next-winner {:bracket :LB :number 6})
              (assoc (de/make-match :LB 1 3 [:TBD :TBD])
                     :prev-left {:bracket :WB :number 6 :result :loser}
                     :prev-right {:bracket :WB :number 7 :result :loser}
                     :next-winner {:bracket :LB :number 7})]
             (vec (take 4 lb))))))

  (testing "first 2 rounds of losers bracket, and full winners bracket"
    (testing "- 4 players"
      (let [num-players 4
            {:keys [wb lb]} (de/make-lb (de/make-wb num-players))]
        #_(is (= [(assoc (de/make-match :WB 1 0 [1 4])
                         :next-winner {:bracket :WB :number 2}
                         :next-loser  {:bracket :LB :number 0})
                  (assoc (de/make-match :WB 1 1 [2 3])
                         :next-winner {:bracket :WB :number 2}
                         :next-loser  {:bracket :LB :number 0})
                  (assoc (de/make-match :WB 2 2 [:TBD :TBD])
                         :prev-left {:bracket :WB :number 0 :result :winner}
                         :prev-right {:bracket :WB :number 1 :result :winner}
                         :next-loser  {:bracket :LB :number 1})]
                 wb))
        (is (= [(assoc (de/make-match :LB 1 0 [:TBD :TBD]) ;; LB round 1
                       :prev-left {:bracket :WB :number 0 :result :loser}
                       :prev-right {:bracket :WB :number 1 :result :loser}
                       :next-winner {:bracket :LB :number 1})
                (assoc (de/make-match :LB 2 1 [:TBD :TBD]) ;; LB round 2
                       :prev-left {:bracket :WB :number 2 :result :loser}
                       :prev-right {:bracket :LB :number 0 :result :winner}
                       :next-winner {:bracket :GF :number 0})]
               (vec (take 2 lb))))))

    (testing "- 8 players"
      (let [num-players 8
            {:keys [wb lb]} (de/make-lb (de/make-wb num-players))]
        #_(is (= [(assoc (de/make-match :WB 1 0 [1 8]) :next-winner {:bracket :WB :number 4})
                  (assoc (de/make-match :WB 1 1 [4 5]) :next-winner {:bracket :WB :number 4})
                  (assoc (de/make-match :WB 1 2 [2 7]) :next-winner {:bracket :WB :number 5})
                  (assoc (de/make-match :WB 1 3 [3 6]) :next-winner {:bracket :WB :number 5})
                  ;; round 2
                  (assoc (de/make-match :WB 2 4 [:TBD :TBD])
                         :next-winner {:bracket :WB :number 6}
                         :prev-left {:bracket :WB :number 0 :result :winner}
                         :prev-right {:bracket :WB :number 1 :result :winner})
                  (assoc (de/make-match :WB 2 5 [:TBD :TBD])
                         :next-winner {:bracket :WB :number 6}
                         :prev-left {:bracket :WB :number 2 :result :winner}
                         :prev-right {:bracket :WB :number 3 :result :winner})
                  ;; round 3
                  (assoc (de/make-match :WB 3 6 [:TBD :TBD])
                         :prev-left {:bracket :WB :number 4 :result :winner}
                         :prev-right {:bracket :WB :number 5 :result :winner})]
                 wb))
        (is (= [;; LB round 1
                (assoc (de/make-match :LB 1 0 [:TBD :TBD])
                       :prev-left {:bracket :WB :number 0 :result :loser}
                       :prev-right {:bracket :WB :number 1 :result :loser}
                       :next-winner {:bracket :LB :number 2})
                (assoc (de/make-match :LB 1 1 [:TBD :TBD])
                       :prev-left {:bracket :WB :number 2 :result :loser}
                       :prev-right {:bracket :WB :number 3 :result :loser}
                       :next-winner {:bracket :LB :number 3})
                ;; LB round 2
                (assoc (de/make-match :LB 2 2 [:TBD :TBD])
                       :prev-left {:bracket :WB :number 5 :result :loser}
                       :prev-right {:bracket :LB :number 0 :result :winner}
                       :next-winner {:bracket :LB :number 4})
                (assoc (de/make-match :LB 2 3 [:TBD :TBD])
                       :prev-left {:bracket :WB :number 4 :result :loser}
                       :prev-right {:bracket :LB :number 1 :result :winner}
                       :next-winner {:bracket :LB :number 4})]
               (vec (take 4 lb)))))))

  (testing "full losers bracket"
    #_(is (= {:wb [(assoc (de/make-match :WB 1 0 [1 4])
                        :next-winner {:bracket :WB :number 2}
                        :next-loser  {:bracket :LB :number 0})
                 (assoc (de/make-match :WB 1 1 [2 3])
                        :next-winner {:bracket :WB :number 2}
                        :next-loser  {:bracket :LB :number 0})
                 (assoc (de/make-match :WB 2 2 [:TBD :TBD])
                        :prev-left {:bracket :WB :number 0 :result :winner}
                        :prev-right {:bracket :WB :number 1 :result :winner}
                        :next-loser  {:bracket :LB :number 1})]
            :lb [(assoc (de/make-match :LB 1 0 [:TBD :TBD])
                        :prev-left {:bracket :WB :number 0 :result :loser}
                        :prev-right {:bracket :WB :number 1 :result :loser}
                        :next-winner {:bracket :LB :number 1})
                 (assoc (de/make-match :LB 2 1 [:TBD :TBD])
                        :prev-left {:bracket :WB :number 2 :result :loser}
                        :prev-right {:bracket :LB :number 0 :result :winner}
                        :next-winner {:bracket :GF :number 0}
                        )]}

           (let [num-players 4]
             (de/make-lb (de/make-wb num-players)))))

    
    (let [num-players 8
          {:keys [wb lb]} (de/make-lb (de/make-wb num-players))]
      #_(is (= [(assoc (de/make-match :WB 1 0 [1 8]) :next-winner {:bracket :WB :number 4})
                (assoc (de/make-match :WB 1 1 [4 5]) :next-winner {:bracket :WB :number 4})
                (assoc (de/make-match :WB 1 2 [2 7]) :next-winner {:bracket :WB :number 5})
                (assoc (de/make-match :WB 1 3 [3 6]) :next-winner {:bracket :WB :number 5})
                ;; round 2
                (assoc (de/make-match :WB 2 4 [:TBD :TBD])
                       :next-winner {:bracket :WB :number 6}
                       :prev-left {:bracket :WB :number 0 :result :winner}
                       :prev-right {:bracket :WB :number 1 :result :winner})
                (assoc (de/make-match :WB 2 5 [:TBD :TBD])
                       :next-winner {:bracket :WB :number 6}
                       :prev-left {:bracket :WB :number 2 :result :winner}
                       :prev-right {:bracket :WB :number 3 :result :winner})
                ;; round 3
                (assoc (de/make-match :WB 3 6 [:TBD :TBD])
                       :prev-left {:bracket :WB :number 4 :result :winner}
                       :prev-right {:bracket :WB :number 5 :result :winner})]
               wb))
      (is (= [;; LB round 1
              (assoc (de/make-match :LB 1 0 [:TBD :TBD])
                     :prev-left {:bracket :WB :number 0 :result :loser}
                     :prev-right {:bracket :WB :number 1 :result :loser}
                     :next-winner {:bracket :LB :number 2})
              (assoc (de/make-match :LB 1 1 [:TBD :TBD])
                     :prev-left {:bracket :WB :number 2 :result :loser}
                     :prev-right {:bracket :WB :number 3 :result :loser}
                     :next-winner {:bracket :LB :number 3})
              ;; LB round 2
              (assoc (de/make-match :LB 2 2 [:TBD :TBD])
                     :prev-left {:bracket :WB :number 5 :result :loser}
                     :prev-right {:bracket :LB :number 0 :result :winner}
                     :next-winner {:bracket :LB :number 4})
              (assoc (de/make-match :LB 2 3 [:TBD :TBD])
                     :prev-left {:bracket :WB :number 4 :result :loser}
                     :prev-right {:bracket :LB :number 1 :result :winner}
                     :next-winner {:bracket :LB :number 4})
              ;; LB round 3
              (assoc (de/make-match :LB 3 4 [:TBD :TBD])
                     :prev-left {:bracket :LB :number 2 :result :winner}
                     :prev-right {:bracket :LB :number 3 :result :winner}
                     :next-winner {:bracket :LB :number 5})
              ;; LB round 4
              (assoc (de/make-match :LB 4 5 [:TBD :TBD])
                     :prev-left {:bracket :WB :number 6 :result :loser}
                     :prev-right {:bracket :LB :number 4 :result :winner}
                     :next-winner {:bracket :GF :number 0}
                     )]
             lb)))))