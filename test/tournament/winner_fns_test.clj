(ns tournament.winner-fns-test
  (:require [clojure.test :refer [deftest is testing]]
            [tournament.winner-fns :as winner-fns]))

(def dummy-players [nil "Alice" "Bob" "Carol" "Dave"])

(deftest higher-seed-wins-test
  (testing "returns the lower seed number (higher seed)"
    (is (= 1 (winner-fns/higher-seed-wins 1 4 dummy-players)))
    (is (= 1 (winner-fns/higher-seed-wins 4 1 dummy-players)))
    (is (= 2 (winner-fns/higher-seed-wins 2 3 dummy-players)))
    (is (= 2 (winner-fns/higher-seed-wins 3 2 dummy-players)))))
