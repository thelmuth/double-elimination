(ns tournament.play-test
  (:require [clojure.test :refer [deftest is testing]]
            [tournament.play :as play]
            [tournament.double-elim :as de]))

;; Use a 4-player bracket for most tests -- small enough to reason about by hand.
;;
;; WB:
;;   WB0 [1 4]  next-winner→WB2, next-loser→LB0
;;   WB1 [2 3]  next-winner→WB2, next-loser→LB0
;;   WB2 [:TBD :TBD]  prev-left={WB0 winner}, prev-right={WB1 winner}
;;                    next-winner→GF0, next-loser→LB1
;; LB:
;;   LB0 [:TBD :TBD]  prev-left={WB0 loser}, prev-right={WB1 loser}
;;                    next-winner→LB1
;;   LB1 [:TBD :TBD]  prev-left={WB2 loser}, prev-right={LB0 winner}
;;                    next-winner→GF0
;; GF:
;;   GF0 [:TBD :TBD]  prev-left={WB2 winner}, prev-right={LB1 winner}

(def four-player-tournament
  (assoc (de/make-double-elimination 4)
         :players [nil "Alice" "Bob" "Carol" "Dave"]))

(deftest make-tournament-test
  (testing "loads players and builds bracket"
    (let [t (play/make-tournament "test/resources/test_players.csv")]
      (is (= 5 (count (:players t))))
      (is (nil? (nth (:players t) 0)))
      (is (= {:artist "The Beatles" :title "Hey Jude" :year "1968"}
             (nth (:players t) 1)))
      (is (= 3 (count (:WB t))))
      (is (= 2 (count (:LB t))))
      (is (= 1 (count (:GF t)))))))

(deftest get-match-test
  (let [t four-player-tournament]
    (is (= [1 4] (:players (play/get-match t :WB 0))))
    (is (= [2 3] (:players (play/get-match t :WB 1))))
    (is (= [:TBD :TBD] (:players (play/get-match t :LB 0))))
    (is (= [:TBD :TBD] (:players (play/get-match t :GF 0))))))

(deftest record-result-test
  (testing "sets :winner and :loser on the played match"
    (let [t  four-player-tournament
          t' (play/record-result t :WB 0 1)]
      (is (= 1 (:winner (play/get-match t' :WB 0))))
      (is (= 4 (:loser  (play/get-match t' :WB 0))))))

  (testing "winner advances to correct slot in next match"
    (let [t  four-player-tournament
          ;; seed 1 wins WB0 → should go into slot 0 of WB2 (prev-left = WB0 winner)
          t' (play/record-result t :WB 0 1)]
      (is (= [1 :TBD] (:players (play/get-match t' :WB 2)))))
    (let [t  four-player-tournament
          ;; seed 2 wins WB1 → should go into slot 1 of WB2 (prev-right = WB1 winner)
          t' (play/record-result t :WB 1 2)]
      (is (= [:TBD 2] (:players (play/get-match t' :WB 2))))))

  (testing "loser advances to correct slot in LB"
    (let [t  four-player-tournament
          ;; seed 4 loses WB0 → slot 0 of LB0 (prev-left = WB0 loser)
          t' (play/record-result t :WB 0 1)]
      (is (= [4 :TBD] (:players (play/get-match t' :LB 0)))))
    (let [t  four-player-tournament
          ;; seed 3 loses WB1 → slot 1 of LB0 (prev-right = WB1 loser)
          t' (play/record-result t :WB 1 2)]
      (is (= [:TBD 3] (:players (play/get-match t' :LB 0))))))

  (testing "playing both WB round 1 matches fills WB2 and LB0 completely"
    (let [t  four-player-tournament
          t' (-> t
                 (play/record-result :WB 0 1)   ; 1 beats 4
                 (play/record-result :WB 1 2))]  ; 2 beats 3
      (is (= [1 2] (:players (play/get-match t' :WB 2))))
      (is (= [4 3] (:players (play/get-match t' :LB 0))))))

  (testing "playing through to GF fills all brackets"
    (let [t (-> four-player-tournament
                (play/record-result :WB 0 1)   ; 1 beats 4
                (play/record-result :WB 1 2)   ; 2 beats 3
                (play/record-result :LB 0 4)   ; 4 beats 3
                (play/record-result :WB 2 1)   ; 1 beats 2 (WB final)
                (play/record-result :LB 1 4)   ; 4 beats 2 (LB final)
                )]
      (is (= [1 4] (:players (play/get-match t :GF 0))))))
  
  (testing "full tournament of record-results"
    (is (= {:GF [{:bracket :GF,
                  :loser 4,
                  :next-loser nil,
                  :next-winner nil,
                  :number 0,
                  :players [4 2],
                  :prev-left {:bracket :WB, :number 2, :result :winner},
                  :prev-right {:bracket :LB, :number 1, :result :winner},
                  :round 1,
                  :winner 2}],
            :LB [{:bracket :LB,
                  :loser 1,
                  :next-loser nil,
                  :next-winner {:bracket :LB, :number 1},
                  :number 0,
                  :players [1 3],
                  :prev-left {:bracket :WB, :number 0, :result :loser},
                  :prev-right {:bracket :WB, :number 1, :result :loser},
                  :round 1,
                  :winner 3}
                 {:bracket :LB,
                  :loser 3,
                  :next-loser nil,
                  :next-winner {:bracket :GF, :number 0},
                  :number 1,
                  :players [2 3],
                  :prev-left {:bracket :WB, :number 2, :result :loser},
                  :prev-right {:bracket :LB, :number 0, :result :winner},
                  :round 2,
                  :winner 2}],
            :WB [{:bracket :WB,
                  :loser 1,
                  :next-loser {:bracket :LB, :number 0},
                  :next-winner {:bracket :WB, :number 2},
                  :number 0,
                  :players [1 4],
                  :prev-left nil,
                  :prev-right nil,
                  :round 1,
                  :winner 4}
                 {:bracket :WB,
                  :loser 3,
                  :next-loser {:bracket :LB, :number 0},
                  :next-winner {:bracket :WB, :number 2},
                  :number 1,
                  :players [2 3],
                  :prev-left nil,
                  :prev-right nil,
                  :round 1,
                  :winner 2}
                 {:bracket :WB,
                  :loser 2,
                  :next-loser {:bracket :LB, :number 1},
                  :next-winner {:bracket :GF, :number 0},
                  :number 2,
                  :players [4 2],
                  :prev-left {:bracket :WB, :number 0, :result :winner},
                  :prev-right {:bracket :WB, :number 1, :result :winner},
                  :round 2,
                  :winner 4}],
            :players [nil "Alice" "Bob" "Carol" "Dave"]}
           (-> four-player-tournament
               (play/record-result :WB 0 4)
               (play/record-result :WB 1 2)
               (play/record-result :WB 2 4)
               (play/record-result :LB 0 3)
               (play/record-result :LB 1 2)
               (play/record-result :GF 0 2))))))

(deftest play-match-test
  (testing "always-first winner-fn picks seed in slot 0"
    (let [always-first (fn [s1 _s2 _players] s1)
          t  four-player-tournament
          t' (play/play-match t :WB 0 always-first)]
      (is (= 1 (:winner (play/get-match t' :WB 0))))
      (is (= 4 (:loser  (play/get-match t' :WB 0))))))

  (testing "always-second winner-fn picks seed in slot 1"
    (let [always-second (fn [_s1 s2 _players] s2)
          t  four-player-tournament
          t' (play/play-match t :WB 0 always-second)]
      (is (= 4 (:winner (play/get-match t' :WB 0))))
      (is (= 1 (:loser  (play/get-match t' :WB 0)))))))
