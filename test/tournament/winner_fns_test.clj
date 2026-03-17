(ns tournament.winner-fns-test
  (:require [clojure.test :refer [deftest is testing]]
            [tournament.winner-fns :as winner-fns]))

(def dummy-players [nil "Alice" "Bob" "Carol" "Dave"])
(def dummy-match {:bracket :WB :round 1 :number 0})

(deftest higher-seed-wins-test
  (testing "returns the lower seed number (higher seed)"
    (is (= 1 (winner-fns/higher-seed-wins 1 4 dummy-players dummy-match)))
    (is (= 1 (winner-fns/higher-seed-wins 4 1 dummy-players dummy-match)))
    (is (= 2 (winner-fns/higher-seed-wins 2 3 dummy-players dummy-match)))
    (is (= 2 (winner-fns/higher-seed-wins 3 2 dummy-players dummy-match)))))
