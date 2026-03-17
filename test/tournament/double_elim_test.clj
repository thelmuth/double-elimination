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
            {:keys [wb lb]} (de/make-lb (de/make-wb num-players))]
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
            {:keys [wb lb]} (de/make-lb (de/make-wb num-players))]
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
            {:keys [wb lb]} (de/make-lb (de/make-wb num-players))]
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
          {:keys [lb]} (de/make-lb wb)]
      (is (= [(assoc (de/make-match :GF 1 0 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 2 :result :winner}
                     :prev-right {:bracket :LB :number 1 :result :winner})]
             (de/make-gf wb lb)))))

  (testing "8 players"
    (let [wb (de/make-wb 8)
          {:keys [lb]} (de/make-lb wb)]
      (is (= [(assoc (de/make-match :GF 1 0 [:TBD :TBD])
                     :prev-left  {:bracket :WB :number 6 :result :winner}
                     :prev-right {:bracket :LB :number 5 :result :winner})]
             (de/make-gf wb lb))))))

(deftest make-double-elimination-test
  (testing "4 players"
    (let [{:keys [wb lb gf]} (de/make-double-elimination 4)]
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
    (let [{:keys [wb lb gf]} (de/make-double-elimination 8)]
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
    (let [{:keys [wb lb gf]} (de/make-double-elimination 16)]
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
    (let [{:keys [wb lb gf]} (de/make-double-elimination 13)]
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
    (let [{:keys [wb lb gf]} (de/make-double-elimination 32)]
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
             gf)))))
