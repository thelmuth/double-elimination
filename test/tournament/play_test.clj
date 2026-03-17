(ns tournament.play-test
  (:require [clojure.test :refer [deftest is testing]]
            [tournament.play :as play]
            [tournament.winner-fns :as wfn]
            [tournament.double-elim :as de]
            [tournament.players :as players]))

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

(deftest record-result-exceptions-test
  (testing "throws when left player is TBD"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Left player is TBD"
                          (play/record-result four-player-tournament :WB 2 1))))

  (testing "throws when right player is TBD"
    (let [t (play/record-result four-player-tournament :WB 0 1)] ; fills slot 0 of WB2, slot 1 still TBD
      (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Right player is TBD"
                            (play/record-result t :WB 2 1)))))

  (testing "throws when winner is not one of the players"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Winner is not one of the players"
                          (play/record-result four-player-tournament :WB 0 99)))))

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
      (is (= 1 (:loser  (play/get-match t' :WB 0))))))

  (testing "full tournament after play-match"
    (is (= {:GF [{:bracket :GF,
                  :round 1,
                  :number 0,
                  :players [:TBD :TBD],
                  :winner nil,
                  :loser nil,
                  :prev-left {:bracket :WB, :number 6, :result :winner},
                  :prev-right {:bracket :LB, :number 5, :result :winner},
                  :next-winner nil,
                  :next-loser nil}],
            :LB [{:bracket :LB,
                  :round 1,
                  :number 0,
                  :players [:TBD 5],
                  :winner nil,
                  :loser nil,
                  :prev-left {:bracket :WB, :number 0, :result :loser},
                  :prev-right {:bracket :WB, :number 1, :result :loser},
                  :next-winner {:bracket :LB, :number 2},
                  :next-loser nil}
                 {:bracket :LB,
                  :round 1,
                  :number 1,
                  :players [:TBD :TBD],
                  :winner nil,
                  :loser nil,
                  :prev-left {:bracket :WB, :number 2, :result :loser},
                  :prev-right {:bracket :WB, :number 3, :result :loser},
                  :next-winner {:bracket :LB, :number 3},
                  :next-loser nil}
                 {:bracket :LB,
                  :round 2,
                  :number 2,
                  :players [:TBD :TBD],
                  :winner nil,
                  :loser nil,
                  :prev-left {:bracket :WB, :number 5, :result :loser},
                  :prev-right {:bracket :LB, :number 0, :result :winner},
                  :next-winner {:bracket :LB, :number 4},
                  :next-loser nil}
                 {:bracket :LB,
                  :round 2,
                  :number 3,
                  :players [:TBD :TBD],
                  :winner nil,
                  :loser nil,
                  :prev-left {:bracket :WB, :number 4, :result :loser},
                  :prev-right {:bracket :LB, :number 1, :result :winner},
                  :next-winner {:bracket :LB, :number 4},
                  :next-loser nil}
                 {:bracket :LB,
                  :round 3,
                  :number 4,
                  :players [:TBD :TBD],
                  :winner nil,
                  :loser nil,
                  :prev-left {:bracket :LB, :number 2, :result :winner},
                  :prev-right {:bracket :LB, :number 3, :result :winner},
                  :next-winner {:bracket :LB, :number 5},
                  :next-loser nil}
                 {:bracket :LB,
                  :round 4,
                  :number 5,
                  :players [:TBD :TBD],
                  :winner nil,
                  :loser nil,
                  :prev-left {:bracket :WB, :number 6, :result :loser},
                  :prev-right {:bracket :LB, :number 4, :result :winner},
                  :next-winner {:bracket :GF, :number 0},
                  :next-loser nil}],
            :WB [{:bracket :WB,
                  :round 1,
                  :number 0,
                  :players [1 :BYE],
                  :winner nil,
                  :loser nil,
                  :prev-left nil,
                  :prev-right nil,
                  :next-winner {:bracket :WB, :number 4},
                  :next-loser {:bracket :LB, :number 0}}
                 {:bracket :WB,
                  :round 1,
                  :number 1,
                  :players [4 5],
                  :winner 4,
                  :loser 5,
                  :prev-left nil,
                  :prev-right nil,
                  :next-winner {:bracket :WB, :number 4},
                  :next-loser {:bracket :LB, :number 0}}
                 {:bracket :WB,
                  :round 1,
                  :number 2,
                  :players [2 :BYE],
                  :winner nil,
                  :loser nil,
                  :prev-left nil,
                  :prev-right nil,
                  :next-winner {:bracket :WB, :number 5},
                  :next-loser {:bracket :LB, :number 1}}
                 {:bracket :WB,
                  :round 1,
                  :number 3,
                  :players [3 6],
                  :winner nil,
                  :loser nil,
                  :prev-left nil,
                  :prev-right nil,
                  :next-winner {:bracket :WB, :number 5},
                  :next-loser {:bracket :LB, :number 1}}
                 {:bracket :WB,
                  :round 2,
                  :number 4,
                  :players [:TBD 4],
                  :winner nil,
                  :loser nil,
                  :prev-left {:bracket :WB, :number 0, :result :winner},
                  :prev-right {:bracket :WB, :number 1, :result :winner},
                  :next-winner {:bracket :WB, :number 6},
                  :next-loser {:bracket :LB, :number 3}}
                 {:bracket :WB,
                  :round 2,
                  :number 5,
                  :players [:TBD :TBD],
                  :winner nil,
                  :loser nil,
                  :prev-left {:bracket :WB, :number 2, :result :winner},
                  :prev-right {:bracket :WB, :number 3, :result :winner},
                  :next-winner {:bracket :WB, :number 6},
                  :next-loser {:bracket :LB, :number 2}}
                 {:bracket :WB,
                  :round 3,
                  :number 6,
                  :players [:TBD :TBD],
                  :winner nil,
                  :loser nil,
                  :prev-left {:bracket :WB, :number 4, :result :winner},
                  :prev-right {:bracket :WB, :number 5, :result :winner},
                  :next-winner {:bracket :GF, :number 0},
                  :next-loser {:bracket :LB, :number 5}}],
            :players [nil
                      {:album "OC ReMix, Secret of Mana",
                       :artist "Nase",
                       :genre "Game",
                       :name "Step Off My Flower Bed",
                       :plays "98",
                       :seed "1",
                       :seed-group "0"}
                      {:album "OC ReMix, Chrono Trigger",
                       :artist "Star Salzman, Paul Baxter",
                       :genre "Game",
                       :name "Forever Until Tomorrow",
                       :plays "63",
                       :seed "2",
                       :seed-group "0"}
                      {:album "OC ReMix, Zelda 03: A Link to the Past",
                       :artist "Insert Rupee (Ben Briggs, halc)",
                       :genre "Game",
                       :name "Great Job!",
                       :plays "61",
                       :seed "3",
                       :seed-group "0"}
                      {:album "Mighty Switch Force! 2 OST",
                       :artist "Jake Kaufman",
                       :genre "Game",
                       :name "Dalmatian Station",
                       :plays "67",
                       :seed "4",
                       :seed-group "0"}
                      {:album "Album 2 - Choose Your Character!",
                       :artist "The 8-Bit Big Band",
                       :genre "Game",
                       :name "Luigi's Mansion Theme (From \"Luigi's Mansion\")",
                       :plays "26",
                       :seed "393",
                       :seed-group "5"}
                      {:album "test",
                       :artist "test",
                       :genre "metal",
                       :name "test",
                       :plays "123",
                       :seed "5",
                       :seed-group "1"}]}
           (play/play-match (play/make-tournament "test/resources/very_very_short.csv")
                            :WB
                            1
                            wfn/higher-seed-wins))))

  (testing "full tournament after play-match with a :BYE"
    (is (= {:GF [{:bracket :GF,
                  :loser nil,
                  :next-loser nil,
                  :next-winner nil,
                  :number 0,
                  :players [:TBD :TBD],
                  :prev-left {:bracket :WB, :number 6, :result :winner},
                  :prev-right {:bracket :LB, :number 5, :result :winner},
                  :round 1,
                  :winner nil}],
            :LB [{:bracket :LB,
                  :loser nil,
                  :next-loser nil,
                  :next-winner {:bracket :LB, :number 2},
                  :number 0,
                  :players [:BYE :TBD],
                  :prev-left {:bracket :WB, :number 0, :result :loser},
                  :prev-right {:bracket :WB, :number 1, :result :loser},
                  :round 1,
                  :winner nil}
                 {:bracket :LB,
                  :loser nil,
                  :next-loser nil,
                  :next-winner {:bracket :LB, :number 3},
                  :number 1,
                  :players [:TBD :TBD],
                  :prev-left {:bracket :WB, :number 2, :result :loser},
                  :prev-right {:bracket :WB, :number 3, :result :loser},
                  :round 1,
                  :winner nil}
                 {:bracket :LB,
                  :loser nil,
                  :next-loser nil,
                  :next-winner {:bracket :LB, :number 4},
                  :number 2,
                  :players [:TBD :TBD],
                  :prev-left {:bracket :WB, :number 5, :result :loser},
                  :prev-right {:bracket :LB, :number 0, :result :winner},
                  :round 2,
                  :winner nil}
                 {:bracket :LB,
                  :loser nil,
                  :next-loser nil,
                  :next-winner {:bracket :LB, :number 4},
                  :number 3,
                  :players [:TBD :TBD],
                  :prev-left {:bracket :WB, :number 4, :result :loser},
                  :prev-right {:bracket :LB, :number 1, :result :winner},
                  :round 2,
                  :winner nil}
                 {:bracket :LB,
                  :loser nil,
                  :next-loser nil,
                  :next-winner {:bracket :LB, :number 5},
                  :number 4,
                  :players [:TBD :TBD],
                  :prev-left {:bracket :LB, :number 2, :result :winner},
                  :prev-right {:bracket :LB, :number 3, :result :winner},
                  :round 3,
                  :winner nil}
                 {:bracket :LB,
                  :loser nil,
                  :next-loser nil,
                  :next-winner {:bracket :GF, :number 0},
                  :number 5,
                  :players [:TBD :TBD],
                  :prev-left {:bracket :WB, :number 6, :result :loser},
                  :prev-right {:bracket :LB, :number 4, :result :winner},
                  :round 4,
                  :winner nil}],
            :WB [{:bracket :WB,
                  :loser :BYE,
                  :next-loser {:bracket :LB, :number 0},
                  :next-winner {:bracket :WB, :number 4},
                  :number 0,
                  :players [1 :BYE],
                  :prev-left nil,
                  :prev-right nil,
                  :round 1,
                  :winner 1}
                 {:bracket :WB,
                  :loser nil,
                  :next-loser {:bracket :LB, :number 0},
                  :next-winner {:bracket :WB, :number 4},
                  :number 1,
                  :players [4 5],
                  :prev-left nil,
                  :prev-right nil,
                  :round 1,
                  :winner nil}
                 {:bracket :WB,
                  :loser nil,
                  :next-loser {:bracket :LB, :number 1},
                  :next-winner {:bracket :WB, :number 5},
                  :number 2,
                  :players [2 :BYE],
                  :prev-left nil,
                  :prev-right nil,
                  :round 1,
                  :winner nil}
                 {:bracket :WB,
                  :loser nil,
                  :next-loser {:bracket :LB, :number 1},
                  :next-winner {:bracket :WB, :number 5},
                  :number 3,
                  :players [3 6],
                  :prev-left nil,
                  :prev-right nil,
                  :round 1,
                  :winner nil}
                 {:bracket :WB,
                  :loser nil,
                  :next-loser {:bracket :LB, :number 3},
                  :next-winner {:bracket :WB, :number 6},
                  :number 4,
                  :players [1 :TBD],
                  :prev-left {:bracket :WB, :number 0, :result :winner},
                  :prev-right {:bracket :WB, :number 1, :result :winner},
                  :round 2,
                  :winner nil}
                 {:bracket :WB,
                  :loser nil,
                  :next-loser {:bracket :LB, :number 2},
                  :next-winner {:bracket :WB, :number 6},
                  :number 5,
                  :players [:TBD :TBD],
                  :prev-left {:bracket :WB, :number 2, :result :winner},
                  :prev-right {:bracket :WB, :number 3, :result :winner},
                  :round 2,
                  :winner nil}
                 {:bracket :WB,
                  :loser nil,
                  :next-loser {:bracket :LB, :number 5},
                  :next-winner {:bracket :GF, :number 0},
                  :number 6,
                  :players [:TBD :TBD],
                  :prev-left {:bracket :WB, :number 4, :result :winner},
                  :prev-right {:bracket :WB, :number 5, :result :winner},
                  :round 3,
                  :winner nil}],
            :players [nil
                      {:album "OC ReMix, Secret of Mana",
                       :artist "Nase",
                       :genre "Game",
                       :name "Step Off My Flower Bed",
                       :plays "98",
                       :seed "1",
                       :seed-group "0"}
                      {:album "OC ReMix, Chrono Trigger",
                       :artist "Star Salzman, Paul Baxter",
                       :genre "Game",
                       :name "Forever Until Tomorrow",
                       :plays "63",
                       :seed "2",
                       :seed-group "0"}
                      {:album "OC ReMix, Zelda 03: A Link to the Past",
                       :artist "Insert Rupee (Ben Briggs, halc)",
                       :genre "Game",
                       :name "Great Job!",
                       :plays "61",
                       :seed "3",
                       :seed-group "0"}
                      {:album "Mighty Switch Force! 2 OST",
                       :artist "Jake Kaufman",
                       :genre "Game",
                       :name "Dalmatian Station",
                       :plays "67",
                       :seed "4",
                       :seed-group "0"}
                      {:album "Album 2 - Choose Your Character!",
                       :artist "The 8-Bit Big Band",
                       :genre "Game",
                       :name "Luigi's Mansion Theme (From \"Luigi's Mansion\")",
                       :plays "26",
                       :seed "393",
                       :seed-group "5"}
                      {:album "test", :artist "test", :genre "metal", :name "test", :plays "123", :seed "5", :seed-group "1"}]}
           (play/play-match (play/make-tournament "test/resources/very_very_short.csv")
                            :WB
                            0
                            wfn/higher-seed-wins)))))


(deftest tournament-complete-test
  (testing "returns false on a fresh tournament"
    (is (false? (play/tournament-complete? four-player-tournament))))

  (testing "returns true after all matches are played"
    (let [completed (-> four-player-tournament
                        (play/record-result :WB 0 1)
                        (play/record-result :WB 1 2)
                        (play/record-result :LB 0 4)
                        (play/record-result :WB 2 1)
                        (play/record-result :LB 1 4)
                        (play/record-result :GF 0 1))]
      (is (true? (play/tournament-complete? completed))))))

(deftest play-tournament-test
  (testing "seed 1 wins everything with higher-seed-wins"
    (let [completed (play/play-tournament four-player-tournament wfn/higher-seed-wins {})]
      (is (play/tournament-complete? completed))
      (is (= 1 (:winner (first (:GF completed)))))))

  (testing ":wb-first and :interleaved produce same final state with deterministic winner-fn"
    (let [wb-first    (play/play-tournament four-player-tournament wfn/higher-seed-wins {:bracket-order :wb-first})
          interleaved (play/play-tournament four-player-tournament wfn/higher-seed-wins {:bracket-order :interleaved})]
      (is (= wb-first interleaved)))))

(comment

  ;; TMH: Keep this around for now. It shows how to use play-match
  ;; with cli-winner-fn
  (play/play-match (play/make-tournament "test/resources/very_very_short.csv")
                   :WB
                   1
                   (wfn/cli-winner-fn players/default-player->str))

  ;; this one has a :BYE
  (play/play-match (play/make-tournament "test/resources/very_very_short.csv")
                   :WB
                   0
                   (wfn/cli-winner-fn players/default-player->str))

  (play/play-tournament (play/make-tournament "test/resources/very_very_short.csv")
                        wfn/higher-seed-wins
                        {})
  
  (play/play-tournament (play/make-tournament "test/resources/very_very_short.csv")
                        (wfn/cli-winner-fn players/default-player->str)
                        {})


  ;
  )