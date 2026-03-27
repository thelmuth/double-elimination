(ns tournament.storage-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.java.io :as io]
            [tournament.play :as play]
            [tournament.storage :as storage]))

(deftest save-path-test
  (testing "replaces .csv extension with .edn"
    (is (= "players.edn"      (storage/save-path "players.csv")))
    (is (= "data/players.edn" (storage/save-path "data/players.csv")))
    (is (= "a/b/c.edn"        (storage/save-path "a/b/c.csv")))))

(deftest save-load-round-trip-test
  (testing "save and load produce identical tournament"
    (let [tournament {:players [nil {:name "Alice"} {:name "Bob"}]
                      :WB      {0 {:players [1 2] :winner nil}}
                      :LB      {}
                      :GF      {0 {:players [:TBD :TBD] :winner nil}}}
          tmp-path   (str (System/getProperty "java.io.tmpdir") "/tournament-test.edn")]
      (try
        (storage/save-tournament tournament tmp-path)
        (is (= tournament (storage/load-tournament tmp-path)))
        (finally
          (.delete (io/file tmp-path)))))))

(deftest save-load-real-tournament-test
  (testing "round-trip of a tournament built from a real CSV"
    (let [tournament (play/make-tournament "test/resources/test_players.csv")
          tmp-path   (str (System/getProperty "java.io.tmpdir") "/tournament-real-test.edn")]
      (try
        (storage/save-tournament tournament tmp-path)
        (is (= tournament (storage/load-tournament tmp-path)))
        (finally
          (.delete (io/file tmp-path)))))))
