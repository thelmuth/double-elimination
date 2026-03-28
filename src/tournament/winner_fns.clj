(ns tournament.winner-fns
  (:require [clojure.string :as str]
            [tournament.play :as play]))

;; ------------------------
;; Deterministic winner functions
;; ------------------------

(defn higher-seed-wins
  "Winner function that always returns the higher-seeded (lower seed number) player.

   Args:
     left-seed  - integer seed of the left player
     right-seed - integer seed of the right player
     players    - the tournament's 1-indexed player vector (unused)
     match      - the full match map (unused)
     tournament - the full tournament map (unused)"
  [left-seed right-seed _players _match _tournament]
  (min left-seed right-seed))

;; ------------------------
;; CLI display helpers
;; ------------------------

;; Layout constants (all widths in characters).
;; Total line width:  1 + 1 + label-width + 1 + 1 + 1 + col-width + 1 + 1 + 1 + col-width + 1 + 1 = 110
(def ^:private total-width  110)
(def ^:private label-width   14)
(def ^:private col-width     43)

(defn- key->label
  "Convert a keyword like :seed-group to a display string 'Seed Group'."
  [k]
  (->> (str/split (name k) #"-")
       (map str/capitalize)
       (str/join " ")))

(defn- pad-right
  "Return s padded with spaces on the right to exactly width characters."
  [s width]
  (let [s (str s)]
    (str s (str/join (repeat (max 0 (- width (count s))) " ")))))

(defn- center
  "Return s centered within a field of width characters."
  [s width]
  (let [s         (str s)
        total-pad (max 0 (- width (count s)))
        left-pad  (quot total-pad 2)]
    (str (str/join (repeat left-pad " "))
         s
         (str/join (repeat (- total-pad left-pad) " ")))))

(defn- word-wrap
  "Wrap text into a sequence of lines, each at most width characters.
   Breaks at word boundaries; splits mid-word only when a single word
   exceeds width, so no content is ever lost."
  [text width]
  (loop [words   (str/split (str text) #"\s+")
         current ""
         lines   []]
    (if (empty? words)
      (if (seq current) (conj lines current) lines)
      (let [word      (first words)
            rest-words (next words)
            candidate (if (seq current) (str current " " word) word)]
        (cond
          (<= (count candidate) width)
          (recur rest-words candidate lines)

          (seq current)
          (recur words "" (conj lines current))

          :else
          (recur (if (> (count word) width)
                   (cons (subs word width) rest-words)
                   rest-words)
                 ""
                 (conj lines (subs word 0 (min width (count word))))))))))

(defn- hline
  "Build a horizontal rule spanning all three columns."
  [left mid-1 mid-2 right]
  (str left
       (str/join (repeat (+ label-width 2) "─"))
       mid-1
       (str/join (repeat (+ col-width 2) "─"))
       mid-2
       (str/join (repeat (+ col-width 2) "─"))
       right))

(defn- table-row
  "Format one data row, wrapping long values across multiple lines."
  [label left-value right-value]
  (let [label-lines (word-wrap label label-width)
        left-lines  (word-wrap left-value col-width)
        right-lines (word-wrap right-value col-width)
        num-lines   (max 1 (count label-lines) (count left-lines) (count right-lines))]
    (str/join "\n"
              (map (fn [i]
                     (str "│ "
                          (pad-right (nth label-lines i "") label-width)
                          " │ "
                          (pad-right (nth left-lines i "") col-width)
                          " │ "
                          (pad-right (nth right-lines i "") col-width)
                          " │"))
                   (range num-lines)))))

(defn- format-match-header
  "Render the bracket/round/match title in a standalone box."
  [match]
  (let [inner-width (- total-width 2)
        bracket-str (case (:bracket match)
                      :WB "Winner Bracket"
                      :LB "Loser Bracket"
                      :GF "Grand Finals")
        title       (format "%s  ·  Round %d  ·  Match %d"
                            bracket-str (:round match) (:number match))]
    (str/join "\n"
              [(str "┌" (str/join (repeat inner-width "─")) "┐")
               (str "│" (center title inner-width) "│")])))

(defn- format-player-table
  "Render the two-column player info table."
  [left-seed right-seed players player-keys]
  (let [left-player  (nth players left-seed)
        right-player (nth players right-seed)
        ks           (if (seq player-keys) player-keys (keys left-player))
        col-header   (str "│ " (pad-right "" label-width)
                          " │ " (center "A" col-width)
                          " │ " (center "B" col-width) " │")
        data-rows    (map (fn [k]
                            (table-row (key->label k)
                                       (get left-player k "")
                                       (get right-player k "")))
                          ks)]
    (str/join "\n"
              (concat [(hline "├" "┬" "┬" "┤")
                       col-header
                       (hline "├" "┼" "┼" "┤")]
                      data-rows))))

;; ------------------------
;; Final result display
;; ------------------------

(defn format-final-result
  "Format the tournament results as a two-column table showing winner and runner-up.

   Args:
     winner-player    - player map for the tournament winner
     runner-up-player - player map for the runner-up
     player-keys      - (optional) sequence of keys to display, in order.
                        Defaults to all keys in CSV column order."
  ([winner-player runner-up-player]
   (format-final-result winner-player runner-up-player nil))
  ([winner-player runner-up-player player-keys]
   (let [inner-width (- total-width 2)
         ks          (if (seq player-keys) player-keys (keys winner-player))
         col-header  (str "│ " (pad-right "" label-width)
                          " │ " (center "🥇 Winner 🥇" col-width)
                          " │ " (center "🥈 Runner Up 🥈" col-width) " │")
         data-rows   (map (fn [k]
                            (table-row (key->label k)
                                       (get winner-player k "")
                                       (get runner-up-player k "")))
                          ks)]
     (str/join "\n"
               (concat [(str "┌" (str/join (repeat inner-width "─")) "┐")
                        (str "│" (center "🏆 Tournament Complete! 🏆" inner-width) "│")
                        (hline "├" "┬" "┬" "┤")
                        col-header
                        (hline "├" "┼" "┼" "┤")]
                       data-rows
                       [(hline "└" "┴" "┴" "┘")])))))

;; ------------------------
;; Interactive CLI winner function
;; ------------------------

(def ^:private bracket-by-name {"wb" :WB "lb" :LB "gf" :GF})

(defn- print-commands []
  (println "│ ┌─ Commands ───────────────────────────────────┐                                                           │")
  (println "│ │  a                    Player A wins          │                                                           │")
  (println "│ │  b                    Player B wins          │                                                           │")
  (println "│ │  undo <WB|LB|GF> <n>  Edit a past result     │                                                           │")
  (println "│ └──────────────────────────────────────────────┘                                                           │"))

(defn- show-prompt []
  (print "  > ") (flush))

(defn- handle-undo
  "Process an undo command. Returns {:command :undo ...} if the user confirms,
   :cancelled if the user said no, or nil if the undo was rejected with an error."
  [tokens tournament players player-keys bottom]
  (let [bracket (bracket-by-name (second tokens))
        number  (try (Integer/parseInt (nth tokens 2)) (catch Exception _ nil))]
    (if (or (not= 3 (count tokens)) (nil? bracket) (nil? number))
      (do (println "  Usage: undo <WB|LB|GF> <number>") nil)
      (let [target (play/get-match tournament bracket number)]
        (cond
          (nil? target)
          (do (println (str "  No match found: " (name bracket) " " number)) nil)

          (nil? (:winner target))
          (do (println (str "  " (name bracket) " " number " has not been played yet.")) nil)

          :else
          (let [outcome (play/undo-result tournament bracket number)]
            (if (:error outcome)
              (do (println (str "  " (:error outcome))) nil)
              (let [[target-left target-right] (:players target)
                    winner-label (if (= (:winner target) target-left) "A" "B")]
                (println bottom)
                (println)
                (println (format-match-header target))
                (println (format-player-table target-left target-right players player-keys))
                (println (hline "└" "┴" "┴" "┘"))
                (println (str "  Recorded result: " winner-label " won"))
                (print "  Undo this match? (y/n): ") (flush)
                (if (= "y" (str/lower-case (str/trim (read-line))))
                  {:command :undo :bracket bracket :number number}
                  (do (println "  Undo cancelled.") :cancelled))))))))))

(defn- print-match [left-seed right-seed players match player-keys]
  (println)
  (println (format-match-header match))
  (println (format-player-table left-seed right-seed players player-keys))
  (println (hline "├" "┴" "┴" "┤"))
  (print-commands))

(defn cli-winner-fn
  "Returns a winner-picking function compatible with play-tournament.
   Displays a formatted table of match and player info, then prompts
   the user to pick the winner or enter a command.

   Args:
     player-keys - (optional) sequence of player map keys to display,
                   in order. Defaults to all keys in CSV column order.
"
  ([] (cli-winner-fn nil))
  ([player-keys]
   (fn [left-seed right-seed players match tournament]
     (print-match left-seed right-seed players match player-keys)
     (show-prompt)
     (let [bottom (str "└" (str/join (repeat (- total-width 2) "─")) "┘")]
       (loop []
         (let [tokens (str/split (str/lower-case (str/trim (read-line))) #"\s+")]
           (case (first tokens)
             "a"    (do (println bottom) left-seed)
             "b"    (do (println bottom) right-seed)
             "undo"
             (let [result (handle-undo tokens tournament players player-keys bottom)]
               (cond
                 (= result :cancelled)
                 (do (print-match left-seed right-seed players match player-keys)
                     (show-prompt)
                     (recur))
                 (nil? result)
                 (do (show-prompt) (recur))
                 :else result))
             (do (println "  Unknown command.")
                 (show-prompt) (recur)))))))))
