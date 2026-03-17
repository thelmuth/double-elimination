(ns tournament.players-test
  (:require [clojure.test :refer [deftest is testing]]
            [tournament.players :as p]
            [clojure.string :as str]))

(def test-csv "test/resources/test_players.csv")

(deftest load-players-test
  (testing "returns a 1-indexed vector (index 0 is nil)"
    (let [players (p/load-players test-csv)]
      (is (nil? (nth players 0)))))

  (testing "seeds map directly to indices"
    (let [players (p/load-players test-csv)]
      (is (= {:artist "The Beatles" :title "Hey Jude" :year "1968"}
             (nth players 1)))
      (is (= {:artist "David Bowie" :title "Heroes" :year "1977"}
             (nth players 2)))
      (is (= {:artist "Fleetwood Mac" :title "Dreams" :year "1977"}
             (nth players 3)))
      (is (= {:artist "Led Zeppelin" :title "Stairway to Heaven" :year "1971"}
             (nth players 4)))))

  (testing "count is n+1 (nil sentinel at index 0)"
    (let [players (p/load-players test-csv)]
      (is (= 5 (count players)))))

  (testing "headers are lowercased keywords"
    (let [players (p/load-players test-csv)]
      (is (= #{:artist :title :year} (set (keys (nth players 1))))))))

(deftest default-player->str-test
  (testing "joins all values with ' - '"
    (let [player {:artist "The Beatles" :title "Hey Jude" :year "1968"}
          s (p/default-player->str player)]
      ;; map ordering is insertion order for small maps; test that all parts appear 
      (is (str/includes? s "The Beatles"))
      (is (str/includes? s "Hey Jude"))
      (is (str/includes? s "1968")))))
