(ns tournament.players-test
  (:require [clojure.test :refer [deftest is testing]]
            [tournament.players :as p]
            [clojure.string :as str]))

(def test-csv "test/resources/test_players.csv")

(deftest load-players-test
  (testing "returns a 0-indexed vector"
    (let [players (p/load-players test-csv)]
      (is (= {:artist "The Beatles" :title "Hey Jude" :year "1968"}
             (nth players 0)))
      (is (= {:artist "David Bowie" :title "Heroes" :year "1977"}
             (nth players 1)))
      (is (= {:artist "Fleetwood Mac" :title "Dreams" :year "1977"}
             (nth players 2)))
      (is (= {:artist "Led Zeppelin" :title "Stairway to Heaven" :year "1971"}
             (nth players 3)))))

  (testing "count equals number of players"
    (let [players (p/load-players test-csv)]
      (is (= 4 (count players)))))

  (testing "headers are lowercased keywords"
    (let [players (p/load-players test-csv)]
      (is (= #{:artist :title :year} (set (keys (nth players 0))))))))

(def very-very-short-csv "test/resources/very_very_short.csv")

(deftest load-players-special-chars-test
  (testing "commas inside a quoted field are preserved as one field"
    (let [players (p/load-players very-very-short-csv)]
      ;; Row 1 (index 0): album is "OC ReMix, Secret of Mana" -- comma inside quotes
      (is (= "OC ReMix, Secret of Mana" (:album (nth players 0))))
      ;; Row 2 (index 1): both artist and album contain commas
      (is (= "Star Salzman, Paul Baxter" (:artist (nth players 1))))
      (is (= "OC ReMix, Chrono Trigger" (:album (nth players 1))))
      ;; Row 3 (index 2): artist contains a comma
      (is (= "Insert Rupee (Ben Briggs, halc)" (:artist (nth players 2))))))

  (testing "escaped double quotes (two double quotes) are unescaped to one double quote"
    (let [players (p/load-players very-very-short-csv)]
      ;; Row 5 (index 4): name is: Luigi's Mansion Theme (From "Luigi's Mansion")
      ;; CSV encodes the inner quotes as "" inside an outer-quoted field
      (is (= "Luigi's Mansion Theme (From \"Luigi's Mansion\")"
             (:name (nth players 4))))))

  (testing "unquoted fields with no special chars are unaffected"
    (let [players (p/load-players very-very-short-csv)]
      (is (= "Dalmatian Station" (:name (nth players 3))))
      (is (= "Jake Kaufman" (:artist (nth players 3))))))

  (testing "spaces in header names become dashes"
    (let [players (p/load-players very-very-short-csv)]
      (is (contains? (nth players 0) :seed-group))
      (is (not (contains? (nth players 0) (keyword "seed group"))))))

  (testing "count is correct"
    (let [players (p/load-players very-very-short-csv)]
      (is (= 6 (count players))))))

(deftest default-player->str-test
  (testing "joins all values with ' - '"
    (let [player {:artist "The Beatles" :title "Hey Jude" :year "1968"}
          s (p/default-player->str player)]
      ;; map ordering is insertion order for small maps; test that all parts appear
      (is (str/includes? s "The Beatles"))
      (is (str/includes? s "Hey Jude"))
      (is (str/includes? s "1968")))))
