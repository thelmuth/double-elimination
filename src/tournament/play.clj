(ns tournament.play
  (:require [tournament.double-elim :as tde]))

;; To Do:
;; - start work on creating and playing a tournament bracket
;; - be able to play through a tournament using command line interface
;;   note: it might be smart to provide a function that determines which
;;   player wins a game. Normally, I would use a function that uses command
;;   line IO with the user to determine the winner, but I could provide
;;   deterministic functions to test that playing works
;; - take info from a CSV and use it to populate tournament

;; Notes for future:
;; - when adapting for music tourney, make it so that players are provided by a CSV
;;   that is automatically converted to a map with a key for each column, and also
;;   provide a function that takes a player map and returns a string representation
;;   That way, could be used for different types of tournaments
;; - Just store vector of players, 1 indexed, indexed by seed, and then in the 
;;   actual bracket only put seed? Then don't need to duplicate player info
;;   everywhere.
