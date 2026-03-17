(ns tournament.play
  (:require [tournament.double-elim :as tde]))

;; To Do:
;; - start work on creating and playing a tournament bracket

;; Notes for future:
;; - when adapting for music tourney, make it so that players are provided by a CSV
;;   that is automatically converted to a map with a key for each column, and also
;;   provide a function that takes a player map and returns a string representation
;;   That way, could be used for different types of tournaments
