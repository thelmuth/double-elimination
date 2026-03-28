(ns tournament.winner-fns-test
  (:require [clojure.test :refer [deftest is testing]]
            [tournament.winner-fns :as winner-fns]))

(def dummy-players    [nil "Alice" "Bob" "Carol" "Dave"])
(def dummy-match      {:bracket :WB :round 1 :number 0})
(def dummy-tournament {})

(deftest higher-seed-wins-test
  (testing "returns the lower seed number (higher seed)"
    (is (= 1 (winner-fns/higher-seed-wins 1 4 dummy-players dummy-match dummy-tournament)))
    (is (= 1 (winner-fns/higher-seed-wins 4 1 dummy-players dummy-match dummy-tournament)))
    (is (= 2 (winner-fns/higher-seed-wins 2 3 dummy-players dummy-match dummy-tournament)))
    (is (= 2 (winner-fns/higher-seed-wins 3 2 dummy-players dummy-match dummy-tournament)))))

(deftest key->label-test
  (testing "single-word keyword"
    (is (= "Name" (#'winner-fns/key->label :name)))
    (is (= "Seed" (#'winner-fns/key->label :seed))))
  (testing "multi-word hyphenated keyword"
    (is (= "Seed Group" (#'winner-fns/key->label :seed-group)))
    (is (= "Play Count" (#'winner-fns/key->label :play-count))))
  (testing "each word is capitalized"
    (is (= "Winner Bracket Round" (#'winner-fns/key->label :winner-bracket-round)))))

(deftest word-wrap-test
  (testing "short text that fits on one line"
    (is (= ["hello"] (#'winner-fns/word-wrap "hello" 10)))
    (is (= ["hello world"] (#'winner-fns/word-wrap "hello world" 20))))
  (testing "text that wraps at word boundaries"
    (is (= ["hello" "world"] (#'winner-fns/word-wrap "hello world" 8)))
    (is (= ["one two" "three"] (#'winner-fns/word-wrap "one two three" 8))))
  (testing "single word longer than width is split mid-word"
    (is (= ["abcde" "fghij"] (#'winner-fns/word-wrap "abcdefghij" 5))))
  (testing "exact-width text is not split"
    (is (= ["hello"] (#'winner-fns/word-wrap "hello" 5))))
  (testing "multiple spaces are treated as single separator"
    (is (= ["hello" "world"] (#'winner-fns/word-wrap "hello   world" 8))))
  (testing "empty string returns empty sequence"
    (is (= [] (#'winner-fns/word-wrap "" 10)))))
