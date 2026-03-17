(ns tournament.double-elim-test
  (:require [clojure.test :refer [deftest is testing]]
            [tournament.double-elim :as de]))

(deftest utils-test
  (is (= '(1 2 4 4 8 8 8 8 16 16 16 16 16 16 16 16 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 64 64 64)
         (map de/next-power-of-two (map inc (range 35)))))
  (is (= '(0 1 1 2 2 2 2 3 3 3 3 3 3 3 3 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 5 5 5 5)
         (map de/floor-log2 (map inc (range 35))))))

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
  (testing "3-player WB"
    (is (= [(assoc (de/make-match :WB 1 0 [1 :BYE])
                   :next-winner {:bracket :WB :number 2})
            (assoc (de/make-match :WB 1 1 [2 3])
                   :next-winner {:bracket :WB :number 2})
            (assoc (de/make-match :WB 2 2 [:TBD :TBD])
                   :prev-left {:bracket :WB :number 0 :result :winner}
                   :prev-right {:bracket :WB :number 1 :result :winner}
                   :next-winner {:bracket :GF :number 0})]
           (de/make-wb 3))))

  (testing "4-player WB"
    (is (= [(assoc (de/make-match :WB 1 0 [1 4]) :next-winner {:bracket :WB :number 2})
            (assoc (de/make-match :WB 1 1 [2 3]) :next-winner {:bracket :WB :number 2})
            (assoc (de/make-match :WB 2 2 [:TBD :TBD])
                   :prev-left {:bracket :WB :number 0 :result :winner}
                   :prev-right {:bracket :WB :number 1 :result :winner}
                   :next-winner {:bracket :GF :number 0})]
           (de/make-wb 4))))

  (testing "5-player WB"
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
                   :prev-right {:bracket :WB :number 5 :result :winner}
                   :next-winner {:bracket :GF :number 0})]
           (de/make-wb 5))))

  (testing "13-player WB"
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
                   :prev-right {:bracket :WB :number 13 :result :winner}
                   :next-winner {:bracket :GF :number 0})]
           (de/make-wb 13)))))

(deftest drop-order-for-wb-round-test
  (is (= :standard
         (de/drop-order-for-wb-round 1)))
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
          {lb :LB} (de/make-lb (de/make-wb num-players))]
      (is (= [(assoc (de/make-match :LB 1 0 [:TBD :TBD]) ;; LB round 1
                     :prev-left {:bracket :WB :number 0 :result :loser}
                     :prev-right {:bracket :WB :number 1 :result :loser}
                     :next-winner {:bracket :LB :number 1})]
             (vec (take 1 lb)))))

    ;; test 8 players
    (let [num-players 8
          {lb :LB} (de/make-lb (de/make-wb num-players))]
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
          {lb :LB} (de/make-lb (de/make-wb num-players))]
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
            {wb :WB lb :LB} (de/make-lb (de/make-wb num-players))]
        (is (= [(assoc (de/make-match :WB 1 0 [1 4])
                       :next-winner {:bracket :WB :number 2}
                       :next-loser  {:bracket :LB :number 0})
                (assoc (de/make-match :WB 1 1 [2 3])
                       :next-winner {:bracket :WB :number 2}
                       :next-loser  {:bracket :LB :number 0})
                (assoc (de/make-match :WB 2 2 [:TBD :TBD])
                       :prev-left {:bracket :WB :number 0 :result :winner}
                       :prev-right {:bracket :WB :number 1 :result :winner}
                       :next-winner {:bracket :GF :number 0}
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
            {wb :WB lb :LB} (de/make-lb (de/make-wb num-players))]
        (is (= [(assoc (de/make-match :WB 1 0 [1 8])
                       :next-winner {:bracket :WB :number 4}
                       :next-loser {:bracket :LB :number 0})
                (assoc (de/make-match :WB 1 1 [4 5])
                       :next-winner {:bracket :WB :number 4}
                       :next-loser {:bracket :LB :number 0})
                (assoc (de/make-match :WB 1 2 [2 7])
                       :next-winner {:bracket :WB :number 5}
                       :next-loser {:bracket :LB :number 1})
                (assoc (de/make-match :WB 1 3 [3 6])
                       :next-winner {:bracket :WB :number 5}
                       :next-loser {:bracket :LB :number 1})
                ;; round 2
                (assoc (de/make-match :WB 2 4 [:TBD :TBD])
                       :next-winner {:bracket :WB :number 6}
                       :next-loser {:bracket :LB :number 3}
                       :prev-left {:bracket :WB :number 0 :result :winner}
                       :prev-right {:bracket :WB :number 1 :result :winner})
                (assoc (de/make-match :WB 2 5 [:TBD :TBD])
                       :next-winner {:bracket :WB :number 6}
                       :next-loser {:bracket :LB :number 2}
                       :prev-left {:bracket :WB :number 2 :result :winner}
                       :prev-right {:bracket :WB :number 3 :result :winner})
                ;; round 3
                (assoc (de/make-match :WB 3 6 [:TBD :TBD])
                       :next-winner {:bracket :GF :number 0}
                       :next-loser {:bracket :LB :number 5}
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
    (testing "- 4 players"
      (let [num-players 4
            {wb :WB lb :LB} (de/make-lb (de/make-wb num-players))]
        (is (= [(assoc (de/make-match :WB 1 0 [1 4])
                       :next-winner {:bracket :WB :number 2}
                       :next-loser  {:bracket :LB :number 0})
                (assoc (de/make-match :WB 1 1 [2 3])
                       :next-winner {:bracket :WB :number 2}
                       :next-loser  {:bracket :LB :number 0})
                (assoc (de/make-match :WB 2 2 [:TBD :TBD])
                       :prev-left {:bracket :WB :number 0 :result :winner}
                       :prev-right {:bracket :WB :number 1 :result :winner}
                       :next-loser  {:bracket :LB :number 1}
                       :next-winner {:bracket :GF :number 0})]
               wb))
        (is (= [(assoc (de/make-match :LB 1 0 [:TBD :TBD])
                       :prev-left {:bracket :WB :number 0 :result :loser}
                       :prev-right {:bracket :WB :number 1 :result :loser}
                       :next-winner {:bracket :LB :number 1})
                (assoc (de/make-match :LB 2 1 [:TBD :TBD])
                       :prev-left {:bracket :WB :number 2 :result :loser}
                       :prev-right {:bracket :LB :number 0 :result :winner}
                       :next-winner {:bracket :GF :number 0})]
               lb))))

    (testing "- 8 players"
      (let [num-players 8
            {wb :WB lb :LB} (de/make-lb (de/make-wb num-players))]
        (is (= [(assoc (de/make-match :WB 1 0 [1 8])
                       :next-winner {:bracket :WB :number 4}
                       :next-loser {:bracket :LB :number 0})
                (assoc (de/make-match :WB 1 1 [4 5])
                       :next-winner {:bracket :WB :number 4}
                       :next-loser {:bracket :LB :number 0})
                (assoc (de/make-match :WB 1 2 [2 7])
                       :next-winner {:bracket :WB :number 5}
                       :next-loser {:bracket :LB :number 1})
                (assoc (de/make-match :WB 1 3 [3 6])
                       :next-winner {:bracket :WB :number 5}
                       :next-loser {:bracket :LB :number 1})
                ;; round 2
                (assoc (de/make-match :WB 2 4 [:TBD :TBD])
                       :next-winner {:bracket :WB :number 6}
                       :next-loser {:bracket :LB :number 3}
                       :prev-left {:bracket :WB :number 0 :result :winner}
                       :prev-right {:bracket :WB :number 1 :result :winner})
                (assoc (de/make-match :WB 2 5 [:TBD :TBD])
                       :next-winner {:bracket :WB :number 6}
                       :next-loser {:bracket :LB :number 2}
                       :prev-left {:bracket :WB :number 2 :result :winner}
                       :prev-right {:bracket :WB :number 3 :result :winner})
                ;; round 3
                (assoc (de/make-match :WB 3 6 [:TBD :TBD])
                       :next-winner {:bracket :GF :number 0}
                       :next-loser {:bracket :LB :number 5}
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
                       :next-winner {:bracket :GF :number 0})]
               lb))))))

(deftest make-gf-test
  (testing "4 players"
    (let [wb (de/make-wb 4)
          {lb :LB} (de/make-lb wb)]
      (is (= [(assoc (de/make-match :GF 1 0 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 2 :result :winner}
                     :prev-right {:bracket :LB :number 1 :result :winner})]
             (de/make-gf wb lb)))))

  (testing "8 players"
    (let [wb (de/make-wb 8)
          {lb :LB} (de/make-lb wb)]
      (is (= [(assoc (de/make-match :GF 1 0 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 6 :result :winner}
                     :prev-right {:bracket :LB :number 5 :result :winner})]
             (de/make-gf wb lb))))))

(deftest make-double-elimination-test
  (testing "4 players"
    (let [{wb :WB lb :LB gf :GF} (de/make-double-elimination 4)]
      (is (= [(assoc (de/make-match :WB 1 0 [1 4])
                     :next-winner {:bracket :WB :number 2}
                     :next-loser  {:bracket :LB :number 0})
              (assoc (de/make-match :WB 1 1 [2 3])
                     :next-winner {:bracket :WB :number 2}
                     :next-loser  {:bracket :LB :number 0})
              (assoc (de/make-match :WB 2 2 [:TBD :TBD])
                     :prev-left   {:bracket :WB :number 0 :result :winner}
                     :prev-right  {:bracket :WB :number 1 :result :winner}
                     :next-loser  {:bracket :LB :number 1}
                     :next-winner {:bracket :GF :number 0})]
             wb))
      (is (= [(assoc (de/make-match :LB 1 0 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 0 :result :loser}
                     :prev-right {:bracket :WB :number 1 :result :loser}
                     :next-winner {:bracket :LB :number 1})
              (assoc (de/make-match :LB 2 1 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 2 :result :loser}
                     :prev-right {:bracket :LB :number 0 :result :winner}
                     :next-winner {:bracket :GF :number 0})]
             lb))
      (is (= [(assoc (de/make-match :GF 1 0 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 2 :result :winner}
                     :prev-right {:bracket :LB :number 1 :result :winner})]
             gf))))

  (testing "8 players"
    (let [{wb :WB lb :LB gf :GF} (de/make-double-elimination 8)]
      (is (= [;; round 1
              (assoc (de/make-match :WB 1 0 [1 8])
                     :next-winner {:bracket :WB :number 4}
                     :next-loser  {:bracket :LB :number 0})
              (assoc (de/make-match :WB 1 1 [4 5])
                     :next-winner {:bracket :WB :number 4}
                     :next-loser  {:bracket :LB :number 0})
              (assoc (de/make-match :WB 1 2 [2 7])
                     :next-winner {:bracket :WB :number 5}
                     :next-loser  {:bracket :LB :number 1})
              (assoc (de/make-match :WB 1 3 [3 6])
                     :next-winner {:bracket :WB :number 5}
                     :next-loser  {:bracket :LB :number 1})
              ;; round 2
              (assoc (de/make-match :WB 2 4 [:TBD :TBD])
                     :prev-left   {:bracket :WB :number 0 :result :winner}
                     :prev-right  {:bracket :WB :number 1 :result :winner}
                     :next-winner {:bracket :WB :number 6}
                     :next-loser  {:bracket :LB :number 3})
              (assoc (de/make-match :WB 2 5 [:TBD :TBD])
                     :prev-left   {:bracket :WB :number 2 :result :winner}
                     :prev-right  {:bracket :WB :number 3 :result :winner}
                     :next-winner {:bracket :WB :number 6}
                     :next-loser  {:bracket :LB :number 2})
              ;; round 3
              (assoc (de/make-match :WB 3 6 [:TBD :TBD])
                     :prev-left   {:bracket :WB :number 4 :result :winner}
                     :prev-right  {:bracket :WB :number 5 :result :winner}
                     :next-winner {:bracket :GF :number 0}
                     :next-loser  {:bracket :LB :number 5})]
             wb))
      (is (= [;; LB round 1
              (assoc (de/make-match :LB 1 0 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 0 :result :loser}
                     :prev-right {:bracket :WB :number 1 :result :loser}
                     :next-winner {:bracket :LB :number 2})
              (assoc (de/make-match :LB 1 1 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 2 :result :loser}
                     :prev-right {:bracket :WB :number 3 :result :loser}
                     :next-winner {:bracket :LB :number 3})
              ;; LB round 2
              (assoc (de/make-match :LB 2 2 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 5 :result :loser}
                     :prev-right {:bracket :LB :number 0 :result :winner}
                     :next-winner {:bracket :LB :number 4})
              (assoc (de/make-match :LB 2 3 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 4 :result :loser}
                     :prev-right {:bracket :LB :number 1 :result :winner}
                     :next-winner {:bracket :LB :number 4})
              ;; LB round 3
              (assoc (de/make-match :LB 3 4 [:TBD :TBD])
                     :prev-left  {:bracket :LB :number 2 :result :winner}
                     :prev-right {:bracket :LB :number 3 :result :winner}
                     :next-winner {:bracket :LB :number 5})
              ;; LB round 4
              (assoc (de/make-match :LB 4 5 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 6 :result :loser}
                     :prev-right {:bracket :LB :number 4 :result :winner}
                     :next-winner {:bracket :GF :number 0})]
             lb))
      (is (= [(assoc (de/make-match :GF 1 0 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 6 :result :winner}
                     :prev-right {:bracket :LB :number 5 :result :winner})]
             gf))))

  (testing "16 players"
    (let [{wb :WB lb :LB gf :GF} (de/make-double-elimination 16)]
      (is (= [;; round 1
              (assoc (de/make-match :WB 1 0 [1 16])
                     :next-winner {:bracket :WB :number 8}
                     :next-loser  {:bracket :LB :number 0})
              (assoc (de/make-match :WB 1 1 [8 9])
                     :next-winner {:bracket :WB :number 8}
                     :next-loser  {:bracket :LB :number 0})
              (assoc (de/make-match :WB 1 2 [4 13])
                     :next-winner {:bracket :WB :number 9}
                     :next-loser  {:bracket :LB :number 1})
              (assoc (de/make-match :WB 1 3 [5 12])
                     :next-winner {:bracket :WB :number 9}
                     :next-loser  {:bracket :LB :number 1})
              (assoc (de/make-match :WB 1 4 [2 15])
                     :next-winner {:bracket :WB :number 10}
                     :next-loser  {:bracket :LB :number 2})
              (assoc (de/make-match :WB 1 5 [7 10])
                     :next-winner {:bracket :WB :number 10}
                     :next-loser  {:bracket :LB :number 2})
              (assoc (de/make-match :WB 1 6 [3 14])
                     :next-winner {:bracket :WB :number 11}
                     :next-loser  {:bracket :LB :number 3})
              (assoc (de/make-match :WB 1 7 [6 11])
                     :next-winner {:bracket :WB :number 11}
                     :next-loser  {:bracket :LB :number 3})
              ;; round 2
              (assoc (de/make-match :WB 2 8 [:TBD :TBD])
                     :prev-left   {:bracket :WB :number 0 :result :winner}
                     :prev-right  {:bracket :WB :number 1 :result :winner}
                     :next-winner {:bracket :WB :number 12}
                     :next-loser  {:bracket :LB :number 7})
              (assoc (de/make-match :WB 2 9 [:TBD :TBD])
                     :prev-left   {:bracket :WB :number 2 :result :winner}
                     :prev-right  {:bracket :WB :number 3 :result :winner}
                     :next-winner {:bracket :WB :number 12}
                     :next-loser  {:bracket :LB :number 6})
              (assoc (de/make-match :WB 2 10 [:TBD :TBD])
                     :prev-left   {:bracket :WB :number 4 :result :winner}
                     :prev-right  {:bracket :WB :number 5 :result :winner}
                     :next-winner {:bracket :WB :number 13}
                     :next-loser  {:bracket :LB :number 5})
              (assoc (de/make-match :WB 2 11 [:TBD :TBD])
                     :prev-left   {:bracket :WB :number 6 :result :winner}
                     :prev-right  {:bracket :WB :number 7 :result :winner}
                     :next-winner {:bracket :WB :number 13}
                     :next-loser  {:bracket :LB :number 4})
              ;; round 3
              (assoc (de/make-match :WB 3 12 [:TBD :TBD])
                     :prev-left   {:bracket :WB :number 8 :result :winner}
                     :prev-right  {:bracket :WB :number 9 :result :winner}
                     :next-winner {:bracket :WB :number 14}
                     :next-loser  {:bracket :LB :number 10})
              (assoc (de/make-match :WB 3 13 [:TBD :TBD])
                     :prev-left   {:bracket :WB :number 10 :result :winner}
                     :prev-right  {:bracket :WB :number 11 :result :winner}
                     :next-winner {:bracket :WB :number 14}
                     :next-loser  {:bracket :LB :number 11})
              ;; round 4
              (assoc (de/make-match :WB 4 14 [:TBD :TBD])
                     :prev-left   {:bracket :WB :number 12 :result :winner}
                     :prev-right  {:bracket :WB :number 13 :result :winner}
                     :next-winner {:bracket :GF :number 0}
                     :next-loser  {:bracket :LB :number 13})]
             wb))
      (is (= [;; LB round 1
              (assoc (de/make-match :LB 1 0 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 0 :result :loser}
                     :prev-right {:bracket :WB :number 1 :result :loser}
                     :next-winner {:bracket :LB :number 4})
              (assoc (de/make-match :LB 1 1 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 2 :result :loser}
                     :prev-right {:bracket :WB :number 3 :result :loser}
                     :next-winner {:bracket :LB :number 5})
              (assoc (de/make-match :LB 1 2 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 4 :result :loser}
                     :prev-right {:bracket :WB :number 5 :result :loser}
                     :next-winner {:bracket :LB :number 6})
              (assoc (de/make-match :LB 1 3 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 6 :result :loser}
                     :prev-right {:bracket :WB :number 7 :result :loser}
                     :next-winner {:bracket :LB :number 7})
              ;; LB round 2
              (assoc (de/make-match :LB 2 4 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 11 :result :loser}
                     :prev-right {:bracket :LB :number 0 :result :winner}
                     :next-winner {:bracket :LB :number 8})
              (assoc (de/make-match :LB 2 5 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 10 :result :loser}
                     :prev-right {:bracket :LB :number 1 :result :winner}
                     :next-winner {:bracket :LB :number 8})
              (assoc (de/make-match :LB 2 6 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 9 :result :loser}
                     :prev-right {:bracket :LB :number 2 :result :winner}
                     :next-winner {:bracket :LB :number 9})
              (assoc (de/make-match :LB 2 7 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 8 :result :loser}
                     :prev-right {:bracket :LB :number 3 :result :winner}
                     :next-winner {:bracket :LB :number 9})
              ;; LB round 3
              (assoc (de/make-match :LB 3 8 [:TBD :TBD])
                     :prev-left  {:bracket :LB :number 4 :result :winner}
                     :prev-right {:bracket :LB :number 5 :result :winner}
                     :next-winner {:bracket :LB :number 10})
              (assoc (de/make-match :LB 3 9 [:TBD :TBD])
                     :prev-left  {:bracket :LB :number 6 :result :winner}
                     :prev-right {:bracket :LB :number 7 :result :winner}
                     :next-winner {:bracket :LB :number 11})
              ;; LB round 4
              (assoc (de/make-match :LB 4 10 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 12 :result :loser}
                     :prev-right {:bracket :LB :number 8 :result :winner}
                     :next-winner {:bracket :LB :number 12})
              (assoc (de/make-match :LB 4 11 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 13 :result :loser}
                     :prev-right {:bracket :LB :number 9 :result :winner}
                     :next-winner {:bracket :LB :number 12})
              ;; LB round 5
              (assoc (de/make-match :LB 5 12 [:TBD :TBD])
                     :prev-left  {:bracket :LB :number 10 :result :winner}
                     :prev-right {:bracket :LB :number 11 :result :winner}
                     :next-winner {:bracket :LB :number 13})
              ;; LB round 6
              (assoc (de/make-match :LB 6 13 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 14 :result :loser}
                     :prev-right {:bracket :LB :number 12 :result :winner}
                     :next-winner {:bracket :GF :number 0})]
             lb))
      (is (= [(assoc (de/make-match :GF 1 0 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 14 :result :winner}
                     :prev-right {:bracket :LB :number 13 :result :winner})]
             gf))))

  (testing "13 players"
    (let [{wb :WB lb :LB gf :GF} (de/make-double-elimination 13)]
      (is (= [;; round 1 -- same as 16 players except seeds 14, 15, 16 replaced with :BYE
              (assoc (de/make-match :WB 1 0 [1 :BYE])
                     :next-winner {:bracket :WB :number 8}
                     :next-loser  {:bracket :LB :number 0})
              (assoc (de/make-match :WB 1 1 [8 9])
                     :next-winner {:bracket :WB :number 8}
                     :next-loser  {:bracket :LB :number 0})
              (assoc (de/make-match :WB 1 2 [4 13])
                     :next-winner {:bracket :WB :number 9}
                     :next-loser  {:bracket :LB :number 1})
              (assoc (de/make-match :WB 1 3 [5 12])
                     :next-winner {:bracket :WB :number 9}
                     :next-loser  {:bracket :LB :number 1})
              (assoc (de/make-match :WB 1 4 [2 :BYE])
                     :next-winner {:bracket :WB :number 10}
                     :next-loser  {:bracket :LB :number 2})
              (assoc (de/make-match :WB 1 5 [7 10])
                     :next-winner {:bracket :WB :number 10}
                     :next-loser  {:bracket :LB :number 2})
              (assoc (de/make-match :WB 1 6 [3 :BYE])
                     :next-winner {:bracket :WB :number 11}
                     :next-loser  {:bracket :LB :number 3})
              (assoc (de/make-match :WB 1 7 [6 11])
                     :next-winner {:bracket :WB :number 11}
                     :next-loser  {:bracket :LB :number 3})
              ;; rounds 2-4 identical to 16 players
              (assoc (de/make-match :WB 2 8 [:TBD :TBD])
                     :prev-left   {:bracket :WB :number 0 :result :winner}
                     :prev-right  {:bracket :WB :number 1 :result :winner}
                     :next-winner {:bracket :WB :number 12}
                     :next-loser  {:bracket :LB :number 7})
              (assoc (de/make-match :WB 2 9 [:TBD :TBD])
                     :prev-left   {:bracket :WB :number 2 :result :winner}
                     :prev-right  {:bracket :WB :number 3 :result :winner}
                     :next-winner {:bracket :WB :number 12}
                     :next-loser  {:bracket :LB :number 6})
              (assoc (de/make-match :WB 2 10 [:TBD :TBD])
                     :prev-left   {:bracket :WB :number 4 :result :winner}
                     :prev-right  {:bracket :WB :number 5 :result :winner}
                     :next-winner {:bracket :WB :number 13}
                     :next-loser  {:bracket :LB :number 5})
              (assoc (de/make-match :WB 2 11 [:TBD :TBD])
                     :prev-left   {:bracket :WB :number 6 :result :winner}
                     :prev-right  {:bracket :WB :number 7 :result :winner}
                     :next-winner {:bracket :WB :number 13}
                     :next-loser  {:bracket :LB :number 4})
              (assoc (de/make-match :WB 3 12 [:TBD :TBD])
                     :prev-left   {:bracket :WB :number 8 :result :winner}
                     :prev-right  {:bracket :WB :number 9 :result :winner}
                     :next-winner {:bracket :WB :number 14}
                     :next-loser  {:bracket :LB :number 10})
              (assoc (de/make-match :WB 3 13 [:TBD :TBD])
                     :prev-left   {:bracket :WB :number 10 :result :winner}
                     :prev-right  {:bracket :WB :number 11 :result :winner}
                     :next-winner {:bracket :WB :number 14}
                     :next-loser  {:bracket :LB :number 11})
              (assoc (de/make-match :WB 4 14 [:TBD :TBD])
                     :prev-left   {:bracket :WB :number 12 :result :winner}
                     :prev-right  {:bracket :WB :number 13 :result :winner}
                     :next-winner {:bracket :GF :number 0}
                     :next-loser  {:bracket :LB :number 13})]
             wb))
      ;; LB and GF are identical to 16 players
      (is (= [;; LB round 1
              (assoc (de/make-match :LB 1 0 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 0 :result :loser}
                     :prev-right {:bracket :WB :number 1 :result :loser}
                     :next-winner {:bracket :LB :number 4})
              (assoc (de/make-match :LB 1 1 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 2 :result :loser}
                     :prev-right {:bracket :WB :number 3 :result :loser}
                     :next-winner {:bracket :LB :number 5})
              (assoc (de/make-match :LB 1 2 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 4 :result :loser}
                     :prev-right {:bracket :WB :number 5 :result :loser}
                     :next-winner {:bracket :LB :number 6})
              (assoc (de/make-match :LB 1 3 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 6 :result :loser}
                     :prev-right {:bracket :WB :number 7 :result :loser}
                     :next-winner {:bracket :LB :number 7})
              ;; LB round 2
              (assoc (de/make-match :LB 2 4 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 11 :result :loser}
                     :prev-right {:bracket :LB :number 0 :result :winner}
                     :next-winner {:bracket :LB :number 8})
              (assoc (de/make-match :LB 2 5 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 10 :result :loser}
                     :prev-right {:bracket :LB :number 1 :result :winner}
                     :next-winner {:bracket :LB :number 8})
              (assoc (de/make-match :LB 2 6 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 9 :result :loser}
                     :prev-right {:bracket :LB :number 2 :result :winner}
                     :next-winner {:bracket :LB :number 9})
              (assoc (de/make-match :LB 2 7 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 8 :result :loser}
                     :prev-right {:bracket :LB :number 3 :result :winner}
                     :next-winner {:bracket :LB :number 9})
              ;; LB round 3
              (assoc (de/make-match :LB 3 8 [:TBD :TBD])
                     :prev-left  {:bracket :LB :number 4 :result :winner}
                     :prev-right {:bracket :LB :number 5 :result :winner}
                     :next-winner {:bracket :LB :number 10})
              (assoc (de/make-match :LB 3 9 [:TBD :TBD])
                     :prev-left  {:bracket :LB :number 6 :result :winner}
                     :prev-right {:bracket :LB :number 7 :result :winner}
                     :next-winner {:bracket :LB :number 11})
              ;; LB round 4
              (assoc (de/make-match :LB 4 10 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 12 :result :loser}
                     :prev-right {:bracket :LB :number 8 :result :winner}
                     :next-winner {:bracket :LB :number 12})
              (assoc (de/make-match :LB 4 11 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 13 :result :loser}
                     :prev-right {:bracket :LB :number 9 :result :winner}
                     :next-winner {:bracket :LB :number 12})
              ;; LB round 5
              (assoc (de/make-match :LB 5 12 [:TBD :TBD])
                     :prev-left  {:bracket :LB :number 10 :result :winner}
                     :prev-right {:bracket :LB :number 11 :result :winner}
                     :next-winner {:bracket :LB :number 13})
              ;; LB round 6
              (assoc (de/make-match :LB 6 13 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 14 :result :loser}
                     :prev-right {:bracket :LB :number 12 :result :winner}
                     :next-winner {:bracket :GF :number 0})]
             lb))
      (is (= [(assoc (de/make-match :GF 1 0 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 14 :result :winner}
                     :prev-right {:bracket :LB :number 13 :result :winner})]
             gf))))

  (testing "32 players"
    (let [{wb :WB lb :LB gf :GF} (de/make-double-elimination 32)]
      (is (= [;; WB round 1
              (assoc (de/make-match :WB 1 0 [1 32])
                     :next-winner {:bracket :WB :number 16}
                     :next-loser  {:bracket :LB :number 0})
              (assoc (de/make-match :WB 1 1 [16 17])
                     :next-winner {:bracket :WB :number 16}
                     :next-loser  {:bracket :LB :number 0})
              (assoc (de/make-match :WB 1 2 [8 25])
                     :next-winner {:bracket :WB :number 17}
                     :next-loser  {:bracket :LB :number 1})
              (assoc (de/make-match :WB 1 3 [9 24])
                     :next-winner {:bracket :WB :number 17}
                     :next-loser  {:bracket :LB :number 1})
              (assoc (de/make-match :WB 1 4 [4 29])
                     :next-winner {:bracket :WB :number 18}
                     :next-loser  {:bracket :LB :number 2})
              (assoc (de/make-match :WB 1 5 [13 20])
                     :next-winner {:bracket :WB :number 18}
                     :next-loser  {:bracket :LB :number 2})
              (assoc (de/make-match :WB 1 6 [5 28])
                     :next-winner {:bracket :WB :number 19}
                     :next-loser  {:bracket :LB :number 3})
              (assoc (de/make-match :WB 1 7 [12 21])
                     :next-winner {:bracket :WB :number 19}
                     :next-loser  {:bracket :LB :number 3})
              (assoc (de/make-match :WB 1 8 [2 31])
                     :next-winner {:bracket :WB :number 20}
                     :next-loser  {:bracket :LB :number 4})
              (assoc (de/make-match :WB 1 9 [15 18])
                     :next-winner {:bracket :WB :number 20}
                     :next-loser  {:bracket :LB :number 4})
              (assoc (de/make-match :WB 1 10 [7 26])
                     :next-winner {:bracket :WB :number 21}
                     :next-loser  {:bracket :LB :number 5})
              (assoc (de/make-match :WB 1 11 [10 23])
                     :next-winner {:bracket :WB :number 21}
                     :next-loser  {:bracket :LB :number 5})
              (assoc (de/make-match :WB 1 12 [3 30])
                     :next-winner {:bracket :WB :number 22}
                     :next-loser  {:bracket :LB :number 6})
              (assoc (de/make-match :WB 1 13 [14 19])
                     :next-winner {:bracket :WB :number 22}
                     :next-loser  {:bracket :LB :number 6})
              (assoc (de/make-match :WB 1 14 [6 27])
                     :next-winner {:bracket :WB :number 23}
                     :next-loser  {:bracket :LB :number 7})
              (assoc (de/make-match :WB 1 15 [11 22])
                     :next-winner {:bracket :WB :number 23}
                     :next-loser  {:bracket :LB :number 7})
              ;; round 2
              (assoc (de/make-match :WB 2 16 [:TBD :TBD])
                     :prev-left   {:bracket :WB :number 0 :result :winner}
                     :prev-right  {:bracket :WB :number 1 :result :winner}
                     :next-winner {:bracket :WB :number 24}
                     :next-loser  {:bracket :LB :number 15})
              (assoc (de/make-match :WB 2 17 [:TBD :TBD])
                     :prev-left   {:bracket :WB :number 2 :result :winner}
                     :prev-right  {:bracket :WB :number 3 :result :winner}
                     :next-winner {:bracket :WB :number 24}
                     :next-loser  {:bracket :LB :number 14})
              (assoc (de/make-match :WB 2 18 [:TBD :TBD])
                     :prev-left   {:bracket :WB :number 4 :result :winner}
                     :prev-right  {:bracket :WB :number 5 :result :winner}
                     :next-winner {:bracket :WB :number 25}
                     :next-loser  {:bracket :LB :number 13})
              (assoc (de/make-match :WB 2 19 [:TBD :TBD])
                     :prev-left   {:bracket :WB :number 6 :result :winner}
                     :prev-right  {:bracket :WB :number 7 :result :winner}
                     :next-winner {:bracket :WB :number 25}
                     :next-loser  {:bracket :LB :number 12})
              (assoc (de/make-match :WB 2 20 [:TBD :TBD])
                     :prev-left   {:bracket :WB :number 8 :result :winner}
                     :prev-right  {:bracket :WB :number 9 :result :winner}
                     :next-winner {:bracket :WB :number 26}
                     :next-loser  {:bracket :LB :number 11})
              (assoc (de/make-match :WB 2 21 [:TBD :TBD])
                     :prev-left   {:bracket :WB :number 10 :result :winner}
                     :prev-right  {:bracket :WB :number 11 :result :winner}
                     :next-winner {:bracket :WB :number 26}
                     :next-loser  {:bracket :LB :number 10})
              (assoc (de/make-match :WB 2 22 [:TBD :TBD])
                     :prev-left   {:bracket :WB :number 12 :result :winner}
                     :prev-right  {:bracket :WB :number 13 :result :winner}
                     :next-winner {:bracket :WB :number 27}
                     :next-loser  {:bracket :LB :number 9})
              (assoc (de/make-match :WB 2 23 [:TBD :TBD])
                     :prev-left   {:bracket :WB :number 14 :result :winner}
                     :prev-right  {:bracket :WB :number 15 :result :winner}
                     :next-winner {:bracket :WB :number 27}
                     :next-loser  {:bracket :LB :number 8})
              ;; round 3
              (assoc (de/make-match :WB 3 24 [:TBD :TBD])
                     :prev-left   {:bracket :WB :number 16 :result :winner}
                     :prev-right  {:bracket :WB :number 17 :result :winner}
                     :next-winner {:bracket :WB :number 28}
                     :next-loser  {:bracket :LB :number 21})
              (assoc (de/make-match :WB 3 25 [:TBD :TBD])
                     :prev-left   {:bracket :WB :number 18 :result :winner}
                     :prev-right  {:bracket :WB :number 19 :result :winner}
                     :next-winner {:bracket :WB :number 28}
                     :next-loser  {:bracket :LB :number 20})
              (assoc (de/make-match :WB 3 26 [:TBD :TBD])
                     :prev-left   {:bracket :WB :number 20 :result :winner}
                     :prev-right  {:bracket :WB :number 21 :result :winner}
                     :next-winner {:bracket :WB :number 29}
                     :next-loser  {:bracket :LB :number 23})
              (assoc (de/make-match :WB 3 27 [:TBD :TBD])
                     :prev-left   {:bracket :WB :number 22 :result :winner}
                     :prev-right  {:bracket :WB :number 23 :result :winner}
                     :next-winner {:bracket :WB :number 29}
                     :next-loser  {:bracket :LB :number 22})
              ;; round 4
              (assoc (de/make-match :WB 4 28 [:TBD :TBD])
                     :prev-left   {:bracket :WB :number 24 :result :winner}
                     :prev-right  {:bracket :WB :number 25 :result :winner}
                     :next-winner {:bracket :WB :number 30}
                     :next-loser  {:bracket :LB :number 27})
              (assoc (de/make-match :WB 4 29 [:TBD :TBD])
                     :prev-left   {:bracket :WB :number 26 :result :winner}
                     :prev-right  {:bracket :WB :number 27 :result :winner}
                     :next-winner {:bracket :WB :number 30}
                     :next-loser  {:bracket :LB :number 26})
              ;; round 5
              (assoc (de/make-match :WB 5 30 [:TBD :TBD])
                     :prev-left   {:bracket :WB :number 28 :result :winner}
                     :prev-right  {:bracket :WB :number 29 :result :winner}
                     :next-winner {:bracket :GF :number 0}
                     :next-loser  {:bracket :LB :number 29})]
             wb))
      (is (= [;; LB round 1
              (assoc (de/make-match :LB 1 0 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 0 :result :loser}
                     :prev-right {:bracket :WB :number 1 :result :loser}
                     :next-winner {:bracket :LB :number 8})
              (assoc (de/make-match :LB 1 1 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 2 :result :loser}
                     :prev-right {:bracket :WB :number 3 :result :loser}
                     :next-winner {:bracket :LB :number 9})
              (assoc (de/make-match :LB 1 2 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 4 :result :loser}
                     :prev-right {:bracket :WB :number 5 :result :loser}
                     :next-winner {:bracket :LB :number 10})
              (assoc (de/make-match :LB 1 3 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 6 :result :loser}
                     :prev-right {:bracket :WB :number 7 :result :loser}
                     :next-winner {:bracket :LB :number 11})
              (assoc (de/make-match :LB 1 4 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 8 :result :loser}
                     :prev-right {:bracket :WB :number 9 :result :loser}
                     :next-winner {:bracket :LB :number 12})
              (assoc (de/make-match :LB 1 5 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 10 :result :loser}
                     :prev-right {:bracket :WB :number 11 :result :loser}
                     :next-winner {:bracket :LB :number 13})
              (assoc (de/make-match :LB 1 6 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 12 :result :loser}
                     :prev-right {:bracket :WB :number 13 :result :loser}
                     :next-winner {:bracket :LB :number 14})
              (assoc (de/make-match :LB 1 7 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 14 :result :loser}
                     :prev-right {:bracket :WB :number 15 :result :loser}
                     :next-winner {:bracket :LB :number 15})
              ;; LB round 2 -- WB round 2 drops in reversed: WB23..WB16
              (assoc (de/make-match :LB 2 8 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 23 :result :loser}
                     :prev-right {:bracket :LB :number 0 :result :winner}
                     :next-winner {:bracket :LB :number 16})
              (assoc (de/make-match :LB 2 9 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 22 :result :loser}
                     :prev-right {:bracket :LB :number 1 :result :winner}
                     :next-winner {:bracket :LB :number 16})
              (assoc (de/make-match :LB 2 10 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 21 :result :loser}
                     :prev-right {:bracket :LB :number 2 :result :winner}
                     :next-winner {:bracket :LB :number 17})
              (assoc (de/make-match :LB 2 11 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 20 :result :loser}
                     :prev-right {:bracket :LB :number 3 :result :winner}
                     :next-winner {:bracket :LB :number 17})
              (assoc (de/make-match :LB 2 12 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 19 :result :loser}
                     :prev-right {:bracket :LB :number 4 :result :winner}
                     :next-winner {:bracket :LB :number 18})
              (assoc (de/make-match :LB 2 13 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 18 :result :loser}
                     :prev-right {:bracket :LB :number 5 :result :winner}
                     :next-winner {:bracket :LB :number 18})
              (assoc (de/make-match :LB 2 14 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 17 :result :loser}
                     :prev-right {:bracket :LB :number 6 :result :winner}
                     :next-winner {:bracket :LB :number 19})
              (assoc (de/make-match :LB 2 15 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 16 :result :loser}
                     :prev-right {:bracket :LB :number 7 :result :winner}
                     :next-winner {:bracket :LB :number 19})
              ;; LB round 3
              (assoc (de/make-match :LB 3 16 [:TBD :TBD])
                     :prev-left  {:bracket :LB :number 8 :result :winner}
                     :prev-right {:bracket :LB :number 9 :result :winner}
                     :next-winner {:bracket :LB :number 20})
              (assoc (de/make-match :LB 3 17 [:TBD :TBD])
                     :prev-left  {:bracket :LB :number 10 :result :winner}
                     :prev-right {:bracket :LB :number 11 :result :winner}
                     :next-winner {:bracket :LB :number 21})
              (assoc (de/make-match :LB 3 18 [:TBD :TBD])
                     :prev-left  {:bracket :LB :number 12 :result :winner}
                     :prev-right {:bracket :LB :number 13 :result :winner}
                     :next-winner {:bracket :LB :number 22})
              (assoc (de/make-match :LB 3 19 [:TBD :TBD])
                     :prev-left  {:bracket :LB :number 14 :result :winner}
                     :prev-right {:bracket :LB :number 15 :result :winner}
                     :next-winner {:bracket :LB :number 23})
              ;; LB round 4 -- WB round 3 drops in half-reversed: WB25,WB24,WB27,WB26
              (assoc (de/make-match :LB 4 20 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 25 :result :loser}
                     :prev-right {:bracket :LB :number 16 :result :winner}
                     :next-winner {:bracket :LB :number 24})
              (assoc (de/make-match :LB 4 21 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 24 :result :loser}
                     :prev-right {:bracket :LB :number 17 :result :winner}
                     :next-winner {:bracket :LB :number 24})
              (assoc (de/make-match :LB 4 22 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 27 :result :loser}
                     :prev-right {:bracket :LB :number 18 :result :winner}
                     :next-winner {:bracket :LB :number 25})
              (assoc (de/make-match :LB 4 23 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 26 :result :loser}
                     :prev-right {:bracket :LB :number 19 :result :winner}
                     :next-winner {:bracket :LB :number 25})
              ;; LB round 5
              (assoc (de/make-match :LB 5 24 [:TBD :TBD])
                     :prev-left  {:bracket :LB :number 20 :result :winner}
                     :prev-right {:bracket :LB :number 21 :result :winner}
                     :next-winner {:bracket :LB :number 26})
              (assoc (de/make-match :LB 5 25 [:TBD :TBD])
                     :prev-left  {:bracket :LB :number 22 :result :winner}
                     :prev-right {:bracket :LB :number 23 :result :winner}
                     :next-winner {:bracket :LB :number 27})
              ;; LB round 6 -- WB round 4 drops in half-swapped: WB29,WB28
              (assoc (de/make-match :LB 6 26 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 29 :result :loser}
                     :prev-right {:bracket :LB :number 24 :result :winner}
                     :next-winner {:bracket :LB :number 28})
              (assoc (de/make-match :LB 6 27 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 28 :result :loser}
                     :prev-right {:bracket :LB :number 25 :result :winner}
                     :next-winner {:bracket :LB :number 28})
              ;; LB round 7
              (assoc (de/make-match :LB 7 28 [:TBD :TBD])
                     :prev-left  {:bracket :LB :number 26 :result :winner}
                     :prev-right {:bracket :LB :number 27 :result :winner}
                     :next-winner {:bracket :LB :number 29})
              ;; LB round 8 -- WB round 5 drops in standard order: WB30
              (assoc (de/make-match :LB 8 29 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 30 :result :loser}
                     :prev-right {:bracket :LB :number 28 :result :winner}
                     :next-winner {:bracket :GF :number 0})]
             lb))
      (is (= [(assoc (de/make-match :GF 1 0 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 30 :result :winner}
                     :prev-right {:bracket :LB :number 29 :result :winner})]
             gf))))

  (testing "64 players"
    (let [{wb :WB lb :LB gf :GF} (de/make-double-elimination 64)]
      (is (= [;; WB round 1 -- seeding order for 64 players
              (assoc (de/make-match :WB 1 0 [1 64])
                     :next-winner {:bracket :WB :number 32}
                     :next-loser  {:bracket :LB :number 0})
              (assoc (de/make-match :WB 1 1 [32 33])
                     :next-winner {:bracket :WB :number 32}
                     :next-loser  {:bracket :LB :number 0})
              (assoc (de/make-match :WB 1 2 [16 49])
                     :next-winner {:bracket :WB :number 33}
                     :next-loser  {:bracket :LB :number 1})
              (assoc (de/make-match :WB 1 3 [17 48])
                     :next-winner {:bracket :WB :number 33}
                     :next-loser  {:bracket :LB :number 1})
              (assoc (de/make-match :WB 1 4 [8 57])
                     :next-winner {:bracket :WB :number 34}
                     :next-loser  {:bracket :LB :number 2})
              (assoc (de/make-match :WB 1 5 [25 40])
                     :next-winner {:bracket :WB :number 34}
                     :next-loser  {:bracket :LB :number 2})
              (assoc (de/make-match :WB 1 6 [9 56])
                     :next-winner {:bracket :WB :number 35}
                     :next-loser  {:bracket :LB :number 3})
              (assoc (de/make-match :WB 1 7 [24 41])
                     :next-winner {:bracket :WB :number 35}
                     :next-loser  {:bracket :LB :number 3})
              (assoc (de/make-match :WB 1 8 [4 61])
                     :next-winner {:bracket :WB :number 36}
                     :next-loser  {:bracket :LB :number 4})
              (assoc (de/make-match :WB 1 9 [29 36])
                     :next-winner {:bracket :WB :number 36}
                     :next-loser  {:bracket :LB :number 4})
              (assoc (de/make-match :WB 1 10 [13 52])
                     :next-winner {:bracket :WB :number 37}
                     :next-loser  {:bracket :LB :number 5})
              (assoc (de/make-match :WB 1 11 [20 45])
                     :next-winner {:bracket :WB :number 37}
                     :next-loser  {:bracket :LB :number 5})
              (assoc (de/make-match :WB 1 12 [5 60])
                     :next-winner {:bracket :WB :number 38}
                     :next-loser  {:bracket :LB :number 6})
              (assoc (de/make-match :WB 1 13 [28 37])
                     :next-winner {:bracket :WB :number 38}
                     :next-loser  {:bracket :LB :number 6})
              (assoc (de/make-match :WB 1 14 [12 53])
                     :next-winner {:bracket :WB :number 39}
                     :next-loser  {:bracket :LB :number 7})
              (assoc (de/make-match :WB 1 15 [21 44])
                     :next-winner {:bracket :WB :number 39}
                     :next-loser  {:bracket :LB :number 7})
              (assoc (de/make-match :WB 1 16 [2 63])
                     :next-winner {:bracket :WB :number 40}
                     :next-loser  {:bracket :LB :number 8})
              (assoc (de/make-match :WB 1 17 [31 34])
                     :next-winner {:bracket :WB :number 40}
                     :next-loser  {:bracket :LB :number 8})
              (assoc (de/make-match :WB 1 18 [15 50])
                     :next-winner {:bracket :WB :number 41}
                     :next-loser  {:bracket :LB :number 9})
              (assoc (de/make-match :WB 1 19 [18 47])
                     :next-winner {:bracket :WB :number 41}
                     :next-loser  {:bracket :LB :number 9})
              (assoc (de/make-match :WB 1 20 [7 58])
                     :next-winner {:bracket :WB :number 42}
                     :next-loser  {:bracket :LB :number 10})
              (assoc (de/make-match :WB 1 21 [26 39])
                     :next-winner {:bracket :WB :number 42}
                     :next-loser  {:bracket :LB :number 10})
              (assoc (de/make-match :WB 1 22 [10 55])
                     :next-winner {:bracket :WB :number 43}
                     :next-loser  {:bracket :LB :number 11})
              (assoc (de/make-match :WB 1 23 [23 42])
                     :next-winner {:bracket :WB :number 43}
                     :next-loser  {:bracket :LB :number 11})
              (assoc (de/make-match :WB 1 24 [3 62])
                     :next-winner {:bracket :WB :number 44}
                     :next-loser  {:bracket :LB :number 12})
              (assoc (de/make-match :WB 1 25 [30 35])
                     :next-winner {:bracket :WB :number 44}
                     :next-loser  {:bracket :LB :number 12})
              (assoc (de/make-match :WB 1 26 [14 51])
                     :next-winner {:bracket :WB :number 45}
                     :next-loser  {:bracket :LB :number 13})
              (assoc (de/make-match :WB 1 27 [19 46])
                     :next-winner {:bracket :WB :number 45}
                     :next-loser  {:bracket :LB :number 13})
              (assoc (de/make-match :WB 1 28 [6 59])
                     :next-winner {:bracket :WB :number 46}
                     :next-loser  {:bracket :LB :number 14})
              (assoc (de/make-match :WB 1 29 [27 38])
                     :next-winner {:bracket :WB :number 46}
                     :next-loser  {:bracket :LB :number 14})
              (assoc (de/make-match :WB 1 30 [11 54])
                     :next-winner {:bracket :WB :number 47}
                     :next-loser  {:bracket :LB :number 15})
              (assoc (de/make-match :WB 1 31 [22 43])
                     :next-winner {:bracket :WB :number 47}
                     :next-loser  {:bracket :LB :number 15})
              ;; WB round 2 -- 16 matches; losers drop to LB round 2 in reverse order (LB31..LB16)
              (assoc (de/make-match :WB 2 32 [:TBD :TBD])
                     :prev-left   {:bracket :WB :number 0 :result :winner}
                     :prev-right  {:bracket :WB :number 1 :result :winner}
                     :next-winner {:bracket :WB :number 48}
                     :next-loser  {:bracket :LB :number 31})
              (assoc (de/make-match :WB 2 33 [:TBD :TBD])
                     :prev-left   {:bracket :WB :number 2 :result :winner}
                     :prev-right  {:bracket :WB :number 3 :result :winner}
                     :next-winner {:bracket :WB :number 48}
                     :next-loser  {:bracket :LB :number 30})
              (assoc (de/make-match :WB 2 34 [:TBD :TBD])
                     :prev-left   {:bracket :WB :number 4 :result :winner}
                     :prev-right  {:bracket :WB :number 5 :result :winner}
                     :next-winner {:bracket :WB :number 49}
                     :next-loser  {:bracket :LB :number 29})
              (assoc (de/make-match :WB 2 35 [:TBD :TBD])
                     :prev-left   {:bracket :WB :number 6 :result :winner}
                     :prev-right  {:bracket :WB :number 7 :result :winner}
                     :next-winner {:bracket :WB :number 49}
                     :next-loser  {:bracket :LB :number 28})
              (assoc (de/make-match :WB 2 36 [:TBD :TBD])
                     :prev-left   {:bracket :WB :number 8 :result :winner}
                     :prev-right  {:bracket :WB :number 9 :result :winner}
                     :next-winner {:bracket :WB :number 50}
                     :next-loser  {:bracket :LB :number 27})
              (assoc (de/make-match :WB 2 37 [:TBD :TBD])
                     :prev-left   {:bracket :WB :number 10 :result :winner}
                     :prev-right  {:bracket :WB :number 11 :result :winner}
                     :next-winner {:bracket :WB :number 50}
                     :next-loser  {:bracket :LB :number 26})
              (assoc (de/make-match :WB 2 38 [:TBD :TBD])
                     :prev-left   {:bracket :WB :number 12 :result :winner}
                     :prev-right  {:bracket :WB :number 13 :result :winner}
                     :next-winner {:bracket :WB :number 51}
                     :next-loser  {:bracket :LB :number 25})
              (assoc (de/make-match :WB 2 39 [:TBD :TBD])
                     :prev-left   {:bracket :WB :number 14 :result :winner}
                     :prev-right  {:bracket :WB :number 15 :result :winner}
                     :next-winner {:bracket :WB :number 51}
                     :next-loser  {:bracket :LB :number 24})
              (assoc (de/make-match :WB 2 40 [:TBD :TBD])
                     :prev-left   {:bracket :WB :number 16 :result :winner}
                     :prev-right  {:bracket :WB :number 17 :result :winner}
                     :next-winner {:bracket :WB :number 52}
                     :next-loser  {:bracket :LB :number 23})
              (assoc (de/make-match :WB 2 41 [:TBD :TBD])
                     :prev-left   {:bracket :WB :number 18 :result :winner}
                     :prev-right  {:bracket :WB :number 19 :result :winner}
                     :next-winner {:bracket :WB :number 52}
                     :next-loser  {:bracket :LB :number 22})
              (assoc (de/make-match :WB 2 42 [:TBD :TBD])
                     :prev-left   {:bracket :WB :number 20 :result :winner}
                     :prev-right  {:bracket :WB :number 21 :result :winner}
                     :next-winner {:bracket :WB :number 53}
                     :next-loser  {:bracket :LB :number 21})
              (assoc (de/make-match :WB 2 43 [:TBD :TBD])
                     :prev-left   {:bracket :WB :number 22 :result :winner}
                     :prev-right  {:bracket :WB :number 23 :result :winner}
                     :next-winner {:bracket :WB :number 53}
                     :next-loser  {:bracket :LB :number 20})
              (assoc (de/make-match :WB 2 44 [:TBD :TBD])
                     :prev-left   {:bracket :WB :number 24 :result :winner}
                     :prev-right  {:bracket :WB :number 25 :result :winner}
                     :next-winner {:bracket :WB :number 54}
                     :next-loser  {:bracket :LB :number 19})
              (assoc (de/make-match :WB 2 45 [:TBD :TBD])
                     :prev-left   {:bracket :WB :number 26 :result :winner}
                     :prev-right  {:bracket :WB :number 27 :result :winner}
                     :next-winner {:bracket :WB :number 54}
                     :next-loser  {:bracket :LB :number 18})
              (assoc (de/make-match :WB 2 46 [:TBD :TBD])
                     :prev-left   {:bracket :WB :number 28 :result :winner}
                     :prev-right  {:bracket :WB :number 29 :result :winner}
                     :next-winner {:bracket :WB :number 55}
                     :next-loser  {:bracket :LB :number 17})
              (assoc (de/make-match :WB 2 47 [:TBD :TBD])
                     :prev-left   {:bracket :WB :number 30 :result :winner}
                     :prev-right  {:bracket :WB :number 31 :result :winner}
                     :next-winner {:bracket :WB :number 55}
                     :next-loser  {:bracket :LB :number 16})
              ;; WB round 3 -- 8 matches; losers drop to LB round 4 in half-reverse order
              ;; half-reverse of [48..55] = [51,50,49,48, 55,54,53,52]
              (assoc (de/make-match :WB 3 48 [:TBD :TBD])
                     :prev-left   {:bracket :WB :number 32 :result :winner}
                     :prev-right  {:bracket :WB :number 33 :result :winner}
                     :next-winner {:bracket :WB :number 56}
                     :next-loser  {:bracket :LB :number 43})
              (assoc (de/make-match :WB 3 49 [:TBD :TBD])
                     :prev-left   {:bracket :WB :number 34 :result :winner}
                     :prev-right  {:bracket :WB :number 35 :result :winner}
                     :next-winner {:bracket :WB :number 56}
                     :next-loser  {:bracket :LB :number 42})
              (assoc (de/make-match :WB 3 50 [:TBD :TBD])
                     :prev-left   {:bracket :WB :number 36 :result :winner}
                     :prev-right  {:bracket :WB :number 37 :result :winner}
                     :next-winner {:bracket :WB :number 57}
                     :next-loser  {:bracket :LB :number 41})
              (assoc (de/make-match :WB 3 51 [:TBD :TBD])
                     :prev-left   {:bracket :WB :number 38 :result :winner}
                     :prev-right  {:bracket :WB :number 39 :result :winner}
                     :next-winner {:bracket :WB :number 57}
                     :next-loser  {:bracket :LB :number 40})
              (assoc (de/make-match :WB 3 52 [:TBD :TBD])
                     :prev-left   {:bracket :WB :number 40 :result :winner}
                     :prev-right  {:bracket :WB :number 41 :result :winner}
                     :next-winner {:bracket :WB :number 58}
                     :next-loser  {:bracket :LB :number 47})
              (assoc (de/make-match :WB 3 53 [:TBD :TBD])
                     :prev-left   {:bracket :WB :number 42 :result :winner}
                     :prev-right  {:bracket :WB :number 43 :result :winner}
                     :next-winner {:bracket :WB :number 58}
                     :next-loser  {:bracket :LB :number 46})
              (assoc (de/make-match :WB 3 54 [:TBD :TBD])
                     :prev-left   {:bracket :WB :number 44 :result :winner}
                     :prev-right  {:bracket :WB :number 45 :result :winner}
                     :next-winner {:bracket :WB :number 59}
                     :next-loser  {:bracket :LB :number 45})
              (assoc (de/make-match :WB 3 55 [:TBD :TBD])
                     :prev-left   {:bracket :WB :number 46 :result :winner}
                     :prev-right  {:bracket :WB :number 47 :result :winner}
                     :next-winner {:bracket :WB :number 59}
                     :next-loser  {:bracket :LB :number 44})
              ;; WB round 4 -- 4 matches; losers drop to LB round 6 in half-swap order
              ;; half-swap of [56,57,58,59] = [58,59,56,57]
              (assoc (de/make-match :WB 4 56 [:TBD :TBD])
                     :prev-left   {:bracket :WB :number 48 :result :winner}
                     :prev-right  {:bracket :WB :number 49 :result :winner}
                     :next-winner {:bracket :WB :number 60}
                     :next-loser  {:bracket :LB :number 54})
              (assoc (de/make-match :WB 4 57 [:TBD :TBD])
                     :prev-left   {:bracket :WB :number 50 :result :winner}
                     :prev-right  {:bracket :WB :number 51 :result :winner}
                     :next-winner {:bracket :WB :number 60}
                     :next-loser  {:bracket :LB :number 55})
              (assoc (de/make-match :WB 4 58 [:TBD :TBD])
                     :prev-left   {:bracket :WB :number 52 :result :winner}
                     :prev-right  {:bracket :WB :number 53 :result :winner}
                     :next-winner {:bracket :WB :number 61}
                     :next-loser  {:bracket :LB :number 52})
              (assoc (de/make-match :WB 4 59 [:TBD :TBD])
                     :prev-left   {:bracket :WB :number 54 :result :winner}
                     :prev-right  {:bracket :WB :number 55 :result :winner}
                     :next-winner {:bracket :WB :number 61}
                     :next-loser  {:bracket :LB :number 53})
              ;; WB round 5 -- 2 matches; losers drop to LB round 8 in standard order (cycling back to index 0)
              (assoc (de/make-match :WB 5 60 [:TBD :TBD])
                     :prev-left   {:bracket :WB :number 56 :result :winner}
                     :prev-right  {:bracket :WB :number 57 :result :winner}
                     :next-winner {:bracket :WB :number 62}
                     :next-loser  {:bracket :LB :number 58})
              (assoc (de/make-match :WB 5 61 [:TBD :TBD])
                     :prev-left   {:bracket :WB :number 58 :result :winner}
                     :prev-right  {:bracket :WB :number 59 :result :winner}
                     :next-winner {:bracket :WB :number 62}
                     :next-loser  {:bracket :LB :number 59})
              ;; WB round 6 -- 1 match (final); loser drops to LB round 10 in reverse order (trivially [62])
              (assoc (de/make-match :WB 6 62 [:TBD :TBD])
                     :prev-left   {:bracket :WB :number 60 :result :winner}
                     :prev-right  {:bracket :WB :number 61 :result :winner}
                     :next-winner {:bracket :GF :number 0}
                     :next-loser  {:bracket :LB :number 61})]
             wb))
      (is (= [;; LB round 1 -- pairs of WB round 1 losers
              (assoc (de/make-match :LB 1 0 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 0 :result :loser}
                     :prev-right {:bracket :WB :number 1 :result :loser}
                     :next-winner {:bracket :LB :number 16})
              (assoc (de/make-match :LB 1 1 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 2 :result :loser}
                     :prev-right {:bracket :WB :number 3 :result :loser}
                     :next-winner {:bracket :LB :number 17})
              (assoc (de/make-match :LB 1 2 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 4 :result :loser}
                     :prev-right {:bracket :WB :number 5 :result :loser}
                     :next-winner {:bracket :LB :number 18})
              (assoc (de/make-match :LB 1 3 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 6 :result :loser}
                     :prev-right {:bracket :WB :number 7 :result :loser}
                     :next-winner {:bracket :LB :number 19})
              (assoc (de/make-match :LB 1 4 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 8 :result :loser}
                     :prev-right {:bracket :WB :number 9 :result :loser}
                     :next-winner {:bracket :LB :number 20})
              (assoc (de/make-match :LB 1 5 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 10 :result :loser}
                     :prev-right {:bracket :WB :number 11 :result :loser}
                     :next-winner {:bracket :LB :number 21})
              (assoc (de/make-match :LB 1 6 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 12 :result :loser}
                     :prev-right {:bracket :WB :number 13 :result :loser}
                     :next-winner {:bracket :LB :number 22})
              (assoc (de/make-match :LB 1 7 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 14 :result :loser}
                     :prev-right {:bracket :WB :number 15 :result :loser}
                     :next-winner {:bracket :LB :number 23})
              (assoc (de/make-match :LB 1 8 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 16 :result :loser}
                     :prev-right {:bracket :WB :number 17 :result :loser}
                     :next-winner {:bracket :LB :number 24})
              (assoc (de/make-match :LB 1 9 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 18 :result :loser}
                     :prev-right {:bracket :WB :number 19 :result :loser}
                     :next-winner {:bracket :LB :number 25})
              (assoc (de/make-match :LB 1 10 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 20 :result :loser}
                     :prev-right {:bracket :WB :number 21 :result :loser}
                     :next-winner {:bracket :LB :number 26})
              (assoc (de/make-match :LB 1 11 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 22 :result :loser}
                     :prev-right {:bracket :WB :number 23 :result :loser}
                     :next-winner {:bracket :LB :number 27})
              (assoc (de/make-match :LB 1 12 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 24 :result :loser}
                     :prev-right {:bracket :WB :number 25 :result :loser}
                     :next-winner {:bracket :LB :number 28})
              (assoc (de/make-match :LB 1 13 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 26 :result :loser}
                     :prev-right {:bracket :WB :number 27 :result :loser}
                     :next-winner {:bracket :LB :number 29})
              (assoc (de/make-match :LB 1 14 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 28 :result :loser}
                     :prev-right {:bracket :WB :number 29 :result :loser}
                     :next-winner {:bracket :LB :number 30})
              (assoc (de/make-match :LB 1 15 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 30 :result :loser}
                     :prev-right {:bracket :WB :number 31 :result :loser}
                     :next-winner {:bracket :LB :number 31})
              ;; LB round 2 -- WB round 2 drops in reverse order: [47,46,...,32]
              (assoc (de/make-match :LB 2 16 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 47 :result :loser}
                     :prev-right {:bracket :LB :number 0 :result :winner}
                     :next-winner {:bracket :LB :number 32})
              (assoc (de/make-match :LB 2 17 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 46 :result :loser}
                     :prev-right {:bracket :LB :number 1 :result :winner}
                     :next-winner {:bracket :LB :number 32})
              (assoc (de/make-match :LB 2 18 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 45 :result :loser}
                     :prev-right {:bracket :LB :number 2 :result :winner}
                     :next-winner {:bracket :LB :number 33})
              (assoc (de/make-match :LB 2 19 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 44 :result :loser}
                     :prev-right {:bracket :LB :number 3 :result :winner}
                     :next-winner {:bracket :LB :number 33})
              (assoc (de/make-match :LB 2 20 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 43 :result :loser}
                     :prev-right {:bracket :LB :number 4 :result :winner}
                     :next-winner {:bracket :LB :number 34})
              (assoc (de/make-match :LB 2 21 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 42 :result :loser}
                     :prev-right {:bracket :LB :number 5 :result :winner}
                     :next-winner {:bracket :LB :number 34})
              (assoc (de/make-match :LB 2 22 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 41 :result :loser}
                     :prev-right {:bracket :LB :number 6 :result :winner}
                     :next-winner {:bracket :LB :number 35})
              (assoc (de/make-match :LB 2 23 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 40 :result :loser}
                     :prev-right {:bracket :LB :number 7 :result :winner}
                     :next-winner {:bracket :LB :number 35})
              (assoc (de/make-match :LB 2 24 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 39 :result :loser}
                     :prev-right {:bracket :LB :number 8 :result :winner}
                     :next-winner {:bracket :LB :number 36})
              (assoc (de/make-match :LB 2 25 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 38 :result :loser}
                     :prev-right {:bracket :LB :number 9 :result :winner}
                     :next-winner {:bracket :LB :number 36})
              (assoc (de/make-match :LB 2 26 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 37 :result :loser}
                     :prev-right {:bracket :LB :number 10 :result :winner}
                     :next-winner {:bracket :LB :number 37})
              (assoc (de/make-match :LB 2 27 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 36 :result :loser}
                     :prev-right {:bracket :LB :number 11 :result :winner}
                     :next-winner {:bracket :LB :number 37})
              (assoc (de/make-match :LB 2 28 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 35 :result :loser}
                     :prev-right {:bracket :LB :number 12 :result :winner}
                     :next-winner {:bracket :LB :number 38})
              (assoc (de/make-match :LB 2 29 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 34 :result :loser}
                     :prev-right {:bracket :LB :number 13 :result :winner}
                     :next-winner {:bracket :LB :number 38})
              (assoc (de/make-match :LB 2 30 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 33 :result :loser}
                     :prev-right {:bracket :LB :number 14 :result :winner}
                     :next-winner {:bracket :LB :number 39})
              (assoc (de/make-match :LB 2 31 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 32 :result :loser}
                     :prev-right {:bracket :LB :number 15 :result :winner}
                     :next-winner {:bracket :LB :number 39})
              ;; LB round 3 -- pairs of LB round 2 winners
              (assoc (de/make-match :LB 3 32 [:TBD :TBD])
                     :prev-left  {:bracket :LB :number 16 :result :winner}
                     :prev-right {:bracket :LB :number 17 :result :winner}
                     :next-winner {:bracket :LB :number 40})
              (assoc (de/make-match :LB 3 33 [:TBD :TBD])
                     :prev-left  {:bracket :LB :number 18 :result :winner}
                     :prev-right {:bracket :LB :number 19 :result :winner}
                     :next-winner {:bracket :LB :number 41})
              (assoc (de/make-match :LB 3 34 [:TBD :TBD])
                     :prev-left  {:bracket :LB :number 20 :result :winner}
                     :prev-right {:bracket :LB :number 21 :result :winner}
                     :next-winner {:bracket :LB :number 42})
              (assoc (de/make-match :LB 3 35 [:TBD :TBD])
                     :prev-left  {:bracket :LB :number 22 :result :winner}
                     :prev-right {:bracket :LB :number 23 :result :winner}
                     :next-winner {:bracket :LB :number 43})
              (assoc (de/make-match :LB 3 36 [:TBD :TBD])
                     :prev-left  {:bracket :LB :number 24 :result :winner}
                     :prev-right {:bracket :LB :number 25 :result :winner}
                     :next-winner {:bracket :LB :number 44})
              (assoc (de/make-match :LB 3 37 [:TBD :TBD])
                     :prev-left  {:bracket :LB :number 26 :result :winner}
                     :prev-right {:bracket :LB :number 27 :result :winner}
                     :next-winner {:bracket :LB :number 45})
              (assoc (de/make-match :LB 3 38 [:TBD :TBD])
                     :prev-left  {:bracket :LB :number 28 :result :winner}
                     :prev-right {:bracket :LB :number 29 :result :winner}
                     :next-winner {:bracket :LB :number 46})
              (assoc (de/make-match :LB 3 39 [:TBD :TBD])
                     :prev-left  {:bracket :LB :number 30 :result :winner}
                     :prev-right {:bracket :LB :number 31 :result :winner}
                     :next-winner {:bracket :LB :number 47})
              ;; LB round 4 -- WB round 3 drops in half-reverse order
              ;; half-reverse of [48..55] = [51,50,49,48, 55,54,53,52]
              (assoc (de/make-match :LB 4 40 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 51 :result :loser}
                     :prev-right {:bracket :LB :number 32 :result :winner}
                     :next-winner {:bracket :LB :number 48})
              (assoc (de/make-match :LB 4 41 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 50 :result :loser}
                     :prev-right {:bracket :LB :number 33 :result :winner}
                     :next-winner {:bracket :LB :number 48})
              (assoc (de/make-match :LB 4 42 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 49 :result :loser}
                     :prev-right {:bracket :LB :number 34 :result :winner}
                     :next-winner {:bracket :LB :number 49})
              (assoc (de/make-match :LB 4 43 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 48 :result :loser}
                     :prev-right {:bracket :LB :number 35 :result :winner}
                     :next-winner {:bracket :LB :number 49})
              (assoc (de/make-match :LB 4 44 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 55 :result :loser}
                     :prev-right {:bracket :LB :number 36 :result :winner}
                     :next-winner {:bracket :LB :number 50})
              (assoc (de/make-match :LB 4 45 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 54 :result :loser}
                     :prev-right {:bracket :LB :number 37 :result :winner}
                     :next-winner {:bracket :LB :number 50})
              (assoc (de/make-match :LB 4 46 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 53 :result :loser}
                     :prev-right {:bracket :LB :number 38 :result :winner}
                     :next-winner {:bracket :LB :number 51})
              (assoc (de/make-match :LB 4 47 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 52 :result :loser}
                     :prev-right {:bracket :LB :number 39 :result :winner}
                     :next-winner {:bracket :LB :number 51})
              ;; LB round 5 -- pairs of LB round 4 winners
              (assoc (de/make-match :LB 5 48 [:TBD :TBD])
                     :prev-left  {:bracket :LB :number 40 :result :winner}
                     :prev-right {:bracket :LB :number 41 :result :winner}
                     :next-winner {:bracket :LB :number 52})
              (assoc (de/make-match :LB 5 49 [:TBD :TBD])
                     :prev-left  {:bracket :LB :number 42 :result :winner}
                     :prev-right {:bracket :LB :number 43 :result :winner}
                     :next-winner {:bracket :LB :number 53})
              (assoc (de/make-match :LB 5 50 [:TBD :TBD])
                     :prev-left  {:bracket :LB :number 44 :result :winner}
                     :prev-right {:bracket :LB :number 45 :result :winner}
                     :next-winner {:bracket :LB :number 54})
              (assoc (de/make-match :LB 5 51 [:TBD :TBD])
                     :prev-left  {:bracket :LB :number 46 :result :winner}
                     :prev-right {:bracket :LB :number 47 :result :winner}
                     :next-winner {:bracket :LB :number 55})
              ;; LB round 6 -- WB round 4 drops in half-swap order
              ;; half-swap of [56,57,58,59] = [58,59,56,57]
              (assoc (de/make-match :LB 6 52 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 58 :result :loser}
                     :prev-right {:bracket :LB :number 48 :result :winner}
                     :next-winner {:bracket :LB :number 56})
              (assoc (de/make-match :LB 6 53 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 59 :result :loser}
                     :prev-right {:bracket :LB :number 49 :result :winner}
                     :next-winner {:bracket :LB :number 56})
              (assoc (de/make-match :LB 6 54 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 56 :result :loser}
                     :prev-right {:bracket :LB :number 50 :result :winner}
                     :next-winner {:bracket :LB :number 57})
              (assoc (de/make-match :LB 6 55 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 57 :result :loser}
                     :prev-right {:bracket :LB :number 51 :result :winner}
                     :next-winner {:bracket :LB :number 57})
              ;; LB round 7 -- pairs of LB round 6 winners
              (assoc (de/make-match :LB 7 56 [:TBD :TBD])
                     :prev-left  {:bracket :LB :number 52 :result :winner}
                     :prev-right {:bracket :LB :number 53 :result :winner}
                     :next-winner {:bracket :LB :number 58})
              (assoc (de/make-match :LB 7 57 [:TBD :TBD])
                     :prev-left  {:bracket :LB :number 54 :result :winner}
                     :prev-right {:bracket :LB :number 55 :result :winner}
                     :next-winner {:bracket :LB :number 59})
              ;; LB round 8 -- WB round 5 drops in standard order (cycling back): [60,61]
              (assoc (de/make-match :LB 8 58 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 60 :result :loser}
                     :prev-right {:bracket :LB :number 56 :result :winner}
                     :next-winner {:bracket :LB :number 60})
              (assoc (de/make-match :LB 8 59 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 61 :result :loser}
                     :prev-right {:bracket :LB :number 57 :result :winner}
                     :next-winner {:bracket :LB :number 60})
              ;; LB round 9 -- pair of LB round 8 winners
              (assoc (de/make-match :LB 9 60 [:TBD :TBD])
                     :prev-left  {:bracket :LB :number 58 :result :winner}
                     :prev-right {:bracket :LB :number 59 :result :winner}
                     :next-winner {:bracket :LB :number 61})
              ;; LB round 10 -- WB round 6 drops in reverse order (trivially [62])
              (assoc (de/make-match :LB 10 61 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 62 :result :loser}
                     :prev-right {:bracket :LB :number 60 :result :winner}
                     :next-winner {:bracket :GF :number 0})]
             lb))
      (is (= [(assoc (de/make-match :GF 1 0 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 62 :result :winner}
                     :prev-right {:bracket :LB :number 61 :result :winner})]
             gf)))))
