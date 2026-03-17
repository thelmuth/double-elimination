(ns tournament.play-order-test
  (:require [clojure.test :refer [deftest is testing]]
            [tournament.play :as play]
            [tournament.play-order :as order]
            [tournament.double-elim :as de]))

;; Four-player tournament bracket (see play_test.clj for full diagram).
(def four-player-tournament
  (assoc (de/make-double-elimination 4)
         :players [nil "Alice" "Bob" "Carol" "Dave"]))

;; After round 1: WB2 [1 2] and LB0 [4 3] are both ready.
(def after-round-1
  (-> four-player-tournament
      (play/record-result :WB 0 1)   ; 1 beats 4
      (play/record-result :WB 1 2))) ; 2 beats 3

(deftest ready-to-play?-test
  (testing "match with two integer seeds and no winner is ready"
    (is (order/ready-to-play? (play/get-match four-player-tournament :WB 0))))

  (testing "match with :TBD players is not ready"
    (is (not (order/ready-to-play? (play/get-match four-player-tournament :WB 2)))))

  (testing "match with one :TBD player is not ready"
    (let [t (play/record-result four-player-tournament :WB 0 1)]
      (is (not (order/ready-to-play? (play/get-match t :WB 2))))))

  (testing "already-played match is not ready"
    (let [t (play/record-result four-player-tournament :WB 0 1)]
      (is (not (order/ready-to-play? (play/get-match t :WB 0)))))))

(deftest ready-matches-wb-first-test
  (testing "at tournament start, only WB round-1 matches are ready, in order"
    (let [ready (order/ready-matches four-player-tournament {:bracket-order :wb-first
                                                             :within-round  :in-order})]
      (is (= [[:WB 0] [:WB 1]]
             (map (juxt :bracket :number) ready)))))

  (testing "after round 1, WB2 comes before LB0 under :wb-first"
    (let [ready (order/ready-matches after-round-1 {:bracket-order :wb-first
                                                    :within-round  :in-order})]
      (is (= [[:WB 2] [:LB 0]]
             (map (juxt :bracket :number) ready))))))

(deftest ready-matches-interleaved-test
  (testing "at tournament start, only WB round-1 matches are ready, in order"
    (let [ready (order/ready-matches four-player-tournament {:bracket-order :interleaved
                                                             :within-round  :in-order})]
      (is (= [[:WB 0] [:WB 1]]
             (map (juxt :bracket :number) ready)))))

  (testing "after round 1, LB0 (wave 1) comes before WB2 (wave 2) under :interleaved"
    (let [ready (order/ready-matches after-round-1 {:bracket-order :interleaved
                                                    :within-round  :in-order})]
      (is (= [[:LB 0] [:WB 2]]
             (map (juxt :bracket :number) ready))))))

(deftest ready-matches-random-test
  (testing "returns the same set of matches as :in-order, just potentially shuffled"
    (let [in-order (set (map (juxt :bracket :number)
                             (order/ready-matches four-player-tournament {:bracket-order :interleaved
                                                                          :within-round  :in-order})))
          random   (set (map (juxt :bracket :number)
                             (order/ready-matches four-player-tournament {:bracket-order :interleaved
                                                                          :within-round  :random})))]
      (is (= in-order random)))))

(deftest ready-matches-defaults-test
  (testing "defaults to :interleaved and :in-order"
    (is (= (order/ready-matches after-round-1 {:bracket-order :interleaved :within-round :in-order})
           (order/ready-matches after-round-1 {})))))
