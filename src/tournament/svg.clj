(ns tournament.svg
  (:require [clojure.string :as str]))

;; ─────────────────────────────────────────────────────────────────────────────
;; Layout constants
;; ─────────────────────────────────────────────────────────────────────────────

(def ^:private box-w        180)  ; match box width in pixels
(def ^:private box-gap       24)  ; vertical gap between consecutive boxes in same round
(def ^:private round-gap     60)  ; horizontal gap between bracket columns
(def ^:private section-gap   60)  ; vertical gap between WB bottom and LB top
(def ^:private label-h       22)  ; height reserved above each section for its label
(def ^:private margin        24)  ; outer margin on all sides
(def ^:private col-step (+ box-w round-gap))  ; pixels per bracket column
(def ^:private max-text-chars 22)  ; max display chars per line (truncated with …)
(def ^:private line-spacing   14)  ; vertical spacing between text lines within a player row

;; ─────────────────────────────────────────────────────────────────────────────
;; Layout dimension functions (vary with number of display lines per player)
;; ─────────────────────────────────────────────────────────────────────────────

(defn- player-row-h
  "Height in pixels of one player slot row for n-lines display lines.
   At n=1 returns 26 (backward-compatible); grows for more lines."
  [n-lines]
  (max 26 (+ (* n-lines line-spacing) 8)))

(defn- match-box-h
  "Total height of one match box: 20px header + two player rows."
  [n-lines]
  (+ 20 (* 2 (player-row-h n-lines))))

(defn- slot-h
  "Vertical pitch per round-1 match slot: box height plus inter-box gap."
  [n-lines]
  (+ (match-box-h n-lines) box-gap))

(defn- line-y-offsets
  "Vector of y-offsets (from the top of a player row) for n-lines text items,
   evenly spaced and vertically centered within the row."
  [n-lines]
  (let [row-h      (player-row-h n-lines)
        total-span (* (dec n-lines) line-spacing)
        first-y    (/ (- row-h total-span) 2.0)]
    (mapv #(+ first-y (* % line-spacing)) (range n-lines))))

;; ─────────────────────────────────────────────────────────────────────────────
;; String helpers
;; ─────────────────────────────────────────────────────────────────────────────

(defn- escape-xml
  "Escape characters that are unsafe in SVG text content."
  [s]
  (-> (str s)
      (str/replace "&"  "&amp;")
      (str/replace "<"  "&lt;")
      (str/replace ">"  "&gt;")
      (str/replace "\"" "&quot;")))

(defn- truncate
  "Truncate s to at most max-len characters, appending … if cut."
  [s max-len]
  (let [s (str s)]
    (if (> (count s) max-len)
      (str (subs s 0 (dec max-len)) "...")
      s)))

(defn- fmt
  "Format a number as a rounded integer string for SVG coordinates."
  [x]
  (str (Math/round ^double (double x))))

;; ─────────────────────────────────────────────────────────────────────────────
;; Player display helpers
;; ─────────────────────────────────────────────────────────────────────────────

(defn- default-display-key
  "Returns the first key in the player map, used when no display-key is specified."
  [player]
  (first (keys player)))

(defn- normalize-display-keys
  "Normalize display-keys to a seq of keywords (or nils).
   nil → [nil], keyword → [kw], sequential → its elements."
  [display-keys]
  (if (sequential? display-keys) display-keys [display-keys]))

(defn- display-keys->n-lines
  "Number of display lines implied by display-keys.
   A vector of n keys → n lines; anything else → 1 line."
  [display-keys]
  (if (sequential? display-keys) (count display-keys) 1))

(defn- player-lines
  "Returns {:texts [str ...] :kw? bool} for a player slot.

   For integer seeds, texts[0] = '#seed  first-field-value', texts[1+] = additional
   field values (one per extra display key).
   For :TBD/:BYE, returns a single-element :texts vector with the label.

   Args:
     seed-or-kw      - integer seed index, :TBD, or :BYE
     players         - the tournament's player vector
     display-keys    - keyword, seq of keywords, or nil
     current-bracket - bracket of the containing match (:WB, :LB, :GF)
     prev-match      - source match for a :TBD slot; when provided, 'TBD' is
                       replaced with 'Loser/Winner of BRACKET Mn'"
  [seed-or-kw players display-keys current-bracket prev-match]
  (cond
    (= seed-or-kw :TBD)
    {:texts [(if prev-match
               (let [verb (if (and (= :LB current-bracket) (= :WB (:bracket prev-match)))
                             "Loser" "Winner")]
                 (str verb " of " (name (:bracket prev-match)) " M" (inc (:number prev-match))))
               "TBD")]
     :kw? true}

    (= seed-or-kw :BYE)
    {:texts ["BYE"] :kw? true}

    :else
    (let [player  (nth players seed-or-kw nil)
          dkeys   (normalize-display-keys display-keys)
          ;; Fall back to first player key when display-keys is nil
          dkeys   (if (every? nil? dkeys)
                    (when player [(default-display-key player)])
                    dkeys)
          first-k (first dkeys)
          first-v (when (and player first-k) (str (get player first-k "")))
          first-t (truncate (str "#" seed-or-kw "  " first-v) max-text-chars)
          rest-ts (mapv (fn [k]
                          (truncate (str (when (and player k) (get player k ""))) max-text-chars))
                        (rest dkeys))]
      {:texts (into [first-t] rest-ts) :kw? false})))

(defn- row-fill-class
  "CSS class for a player row rectangle based on match result."
  [seed-or-kw winner]
  (cond
    (keyword? seed-or-kw) "p-tbd"
    (nil? winner)          "p-row"
    (= seed-or-kw winner)  "p-win"
    :else                  "p-lose"))

;; ─────────────────────────────────────────────────────────────────────────────
;; Layout computation
;; ─────────────────────────────────────────────────────────────────────────────

(defn- x-for-col
  "Left x-coordinate for a bracket column (0-indexed)."
  [col]
  (+ margin (* col col-step)))

(defn- wb-y-centers
  "Returns {match-number → y-center} for all WB matches.
   Round-1 matches are evenly spaced from y-start.
   Later rounds are vertically centered between their two WB predecessors.

   Args:
     wb      - WB match sequence
     y-start - top y-coordinate of the WB section
     n-lines - display lines per player (determines box and slot heights)"
  [wb y-start n-lines]
  (let [by-round (group-by :round wb)
        r1       (sort-by :number (get by-round 1))
        half-box (/ (match-box-h n-lines) 2.0)
        pitch    (slot-h n-lines)
        init     (into {}
                       (map-indexed
                         (fn [idx m]
                           [(:number m) (double (+ y-start half-box (* idx pitch)))])
                         r1))]
    (reduce (fn [acc r]
              (reduce (fn [a m]
                        (assoc a (:number m)
                               (/ (+ (get a (-> m :prev-left  :number))
                                     (get a (-> m :prev-right :number)))
                                  2.0)))
                      acc
                      (sort-by :number (get by-round r))))
            init
            (rest (sort (keys by-round))))))

(defn- lb-y-centers
  "Returns {match-number → y-center} for all LB matches.
   Round-1 matches are evenly spaced from y-start.
   Dropout rounds (even): inherit y from single LB predecessor.
   Consolidation rounds (odd > 1): average y of two LB predecessors.

   Args:
     lb      - LB match sequence
     y-start - top y-coordinate of the LB section
     n-lines - display lines per player"
  [lb y-start n-lines]
  (let [by-round (group-by :round lb)
        r1       (sort-by :number (get by-round 1))
        half-box (/ (match-box-h n-lines) 2.0)
        pitch    (slot-h n-lines)
        init     (into {}
                       (map-indexed
                         (fn [idx m]
                           [(:number m) (double (+ y-start half-box (* idx pitch)))])
                         r1))]
    (reduce (fn [acc r]
              (reduce (fn [a m]
                        (let [pl       (:prev-left m)
                              pr       (:prev-right m)
                              lb-prevs (filter #(= :LB (:bracket %)) [pl pr])]
                          (assoc a (:number m)
                                 (case (count lb-prevs)
                                   2 (/ (+ (get a (:number pl))
                                           (get a (:number pr)))
                                        2.0)
                                   1 (get a (:number (first lb-prevs)))
                                   (double (+ y-start half-box))))))
                      acc
                      (sort-by :number (get by-round r))))
            init
            (rest (sort (keys by-round))))))

(defn- compute-layout
  "Returns a map of [bracket match-number] → {:x left-edge-x :y-center center-y}
   for every match in the tournament.

   Args:
     tournament - the full tournament map
     n-lines    - display lines per player (determines box and slot heights)"
  [tournament n-lines]
  (let [wb  (:WB tournament)
        lb  (:LB tournament)

        ;; WB section
        wb-y-start  (double (+ margin label-h))
        wb-centers  (wb-y-centers wb wb-y-start n-lines)
        wb-r1-count (count (filter #(= 1 (:round %)) wb))
        wb-bottom   (+ wb-y-start (* wb-r1-count (slot-h n-lines)) (- box-gap))

        ;; LB section starts below WB with a gap
        lb-y-start  (+ wb-bottom section-gap label-h)
        lb-centers  (lb-y-centers lb lb-y-start n-lines)

        ;; GF column: one column to the right of whichever bracket ends last
        max-wb-col  (dec (apply max (map :round wb)))
        max-lb-col  (dec (apply max (map :round lb)))
        gf-col      (inc (max max-wb-col max-lb-col))

        ;; GF y: centered between WB final and LB final
        wb-final    (first (filter #(= (inc max-wb-col) (:round %)) wb))
        lb-final    (first (filter #(= (inc max-lb-col) (:round %)) lb))
        gf-y-center (/ (+ (get wb-centers (:number wb-final))
                           (get lb-centers (:number lb-final)))
                        2.0)]
    (merge
      (into {} (map (fn [m] [[:WB (:number m)]
                             {:x        (x-for-col (dec (:round m)))
                              :y-center (get wb-centers (:number m))}])
                    wb))
      (into {} (map (fn [m] [[:LB (:number m)]
                             {:x        (x-for-col (dec (:round m)))
                              :y-center (get lb-centers (:number m))}])
                    lb))
      {[:GF 0] {:x (x-for-col gf-col) :y-center gf-y-center}})))

;; ─────────────────────────────────────────────────────────────────────────────
;; SVG element rendering
;; ─────────────────────────────────────────────────────────────────────────────

(defn- render-connector
  "SVG path for an L-shaped wire from the right edge of one match (at y1)
   to the left edge of the next (at y2)."
  [x1 y1 x2 y2]
  (let [x1r   (+ x1 box-w)
        mid-x (/ (+ x1r x2) 2.0)]
    (str "<path class=\"wire\" d=\"M " (fmt x1r) "," (fmt y1)
         " H " (fmt mid-x) " V " (fmt y2) " H " (fmt x2) "\"/>\n")))

(defn- render-all-connectors
  "SVG paths for every within-section match-to-next-winner connector.
   Cross-section dropout lines (WB→LB) and GF connectors are handled separately."
  [tournament layout]
  (str/join
    (keep (fn [m]
            (let [nw (:next-winner m)]
              (when (and nw (= (:bracket m) (:bracket nw)))
                (let [from-pos (get layout [(:bracket m) (:number m)])
                      to-pos   (get layout [(:bracket nw) (:number nw)])]
                  (when (and from-pos to-pos)
                    (render-connector (:x from-pos) (:y-center from-pos)
                                      (:x to-pos)   (:y-center to-pos)))))))
          (concat (:WB tournament) (:LB tournament) (:GF tournament)))))

(defn- render-gf-connectors
  "SVG branching wire: WB final and LB final each run right to a shared merge-x
   midway between the furthest column's right edge and the GF left edge;
   a vertical segment bridges the two arms; a short horizontal enters the GF box."
  [tournament layout]
  (let [gf-pos    (get layout [:GF 0])
        gf-x      (:x gf-pos)
        gf-y      (:y-center gf-pos)
        wb-feeder (first (filter #(= :GF (:bracket (:next-winner %))) (:WB tournament)))
        lb-feeder (first (filter #(= :GF (:bracket (:next-winner %))) (:LB tournament)))
        wb-pos    (when wb-feeder (get layout [:WB (:number wb-feeder)]))
        lb-pos    (when lb-feeder (get layout [:LB (:number lb-feeder)]))]
    (when (and wb-pos lb-pos)
      (let [wb-x1r  (+ (:x wb-pos) box-w)
            lb-x1r  (+ (:x lb-pos) box-w)
            merge-x (/ (+ (max wb-x1r lb-x1r) gf-x) 2.0)
            wb-y    (:y-center wb-pos)
            lb-y    (:y-center lb-pos)]
        (str "<path class=\"wire\" d=\""
             "M " (fmt wb-x1r) "," (fmt wb-y) " H " (fmt merge-x)
             " V " (fmt lb-y)
             " M " (fmt lb-x1r) "," (fmt lb-y) " H " (fmt merge-x)
             " M " (fmt merge-x) "," (fmt gf-y) " H " (fmt gf-x)
             "\"/>\n")))))

(defn- render-player-row
  "SVG fragments for one player slot (background rect, text lines, optional markers).

   Args:
     row-y         - y-offset of the row top within the match box
     row-h         - height of this row in pixels
     texts         - seq of display strings (one per line)
     kw?           - true for TBD/BYE: gray italic, single line centered vertically
     fill-class    - CSS class for the background rect
     n-lines       - number of layout lines (controls text y-offsets when kw? false)
     show-check?   - whether to render the ✓ win indicator on the first text line
     lb-annotation - lb-drop annotation string for this row, or nil"
  [row-y row-h texts kw? fill-class n-lines show-check? lb-annotation]
  (let [;; TBD/BYE gets its single label centered in the (possibly tall) row;
        ;; normal players use evenly-spaced offsets for all lines.
        offsets   (if kw? [(/ row-h 2.0)] (line-y-offsets n-lines))
        txt-class (if kw? "p-gray" "p-txt")]
    (str "  <rect class=\"" fill-class "\" y=\"" (fmt row-y) "\""
         " width=\"" box-w "\" height=\"" (fmt row-h) "\"/>\n"
         (str/join
           (map-indexed
             (fn [idx text]
               (str "  <text x=\"8\" y=\"" (fmt (+ row-y (nth offsets idx))) "\""
                    " dominant-baseline=\"middle\""
                    " class=\"" txt-class "\">"
                    (escape-xml text) "</text>\n"))
             texts))
         (when show-check?
           (str "  <text x=\"172\" y=\"" (fmt (+ row-y (first offsets))) "\""
                " text-anchor=\"end\" dominant-baseline=\"middle\""
                " class=\"win-chk\">✓</text>\n"))
         (when lb-annotation
           (str "  <text x=\"172\" y=\"" (fmt (+ row-y (first offsets))) "\""
                " text-anchor=\"end\" dominant-baseline=\"middle\""
                " class=\"lb-drop\">" (escape-xml lb-annotation) "</text>\n")))))

(defn- render-match-box
  "SVG <g> string for one match box positioned at (x, y-center).

   Args:
     match        - match map
     players      - tournament player vector
     display-keys - keyword or seq of keywords controlling displayed fields;
                    nil defaults to the first player key
     n-lines      - number of text lines per player (derived from display-keys)
     x            - left edge x-coordinate
     y-center     - vertical center y-coordinate"
  [match players display-keys n-lines x y-center]
  (let [row-h        (player-row-h n-lines)
        box-h        (match-box-h n-lines)
        y-top        (- y-center (/ box-h 2.0))
        [left right] (:players match)
        winner       (:winner match)
        hdr-class    (case (:bracket match) :WB "hdr-wb" :LB "hdr-lb" "hdr-gf")
        hdr-label    (if (= :GF (:bracket match))
                       "GRAND FINALS"
                       (str (name (:bracket match))
                            " · R" (:round match)
                            " · M" (inc (:number match))))
        left-result  (player-lines left  players display-keys (:bracket match) (:prev-left  match))
        right-result (player-lines right players display-keys (:bracket match) (:prev-right match))
        left-class   (row-fill-class left  winner)
        right-class  (row-fill-class right winner)
        ;; LB drop annotation: only for WB matches that have a loser destination
        next-loser   (:next-loser match)
        lb-label     (when (and (= :WB (:bracket match)) next-loser)
                       (str "→ LB M" (inc (:number next-loser))))
        ;; After match is decided, annotate the loser's row
        left-lb-ann  (when (and lb-label winner (= winner right)) lb-label)
        right-lb-ann (when (and lb-label winner (= winner left))  lb-label)]
    (str "<g transform=\"translate(" (fmt x) "," (fmt y-top) ")\">\n"
         "  <rect class=\"" hdr-class "\" width=\"" box-w "\" height=\"20\"/>\n"
         (render-player-row 20 row-h
                            (:texts left-result) (:kw? left-result)
                            left-class n-lines
                            (and (not (:kw? left-result)) (= left winner))
                            left-lb-ann)
         (render-player-row (+ 20 row-h) row-h
                            (:texts right-result) (:kw? right-result)
                            right-class n-lines
                            (and (not (:kw? right-result)) (= right winner))
                            right-lb-ann)
         "  <rect class=\"box\" width=\"" box-w "\" height=\"" (fmt box-h) "\"/>\n"
         "  <line class=\"div\" x1=\"0\" y1=\"20\" x2=\"" box-w "\" y2=\"20\"/>\n"
         "  <line class=\"div\" x1=\"0\" y1=\"" (fmt (+ 20 row-h)) "\""
         " x2=\"" box-w "\" y2=\"" (fmt (+ 20 row-h)) "\"/>\n"
         "  <text x=\"8\" y=\"10\" dominant-baseline=\"middle\" class=\"hdr-txt\">"
         (escape-xml hdr-label) "</text>\n"
         ;; Pending LB drop annotation in the header bar
         (when (and lb-label (nil? winner))
           (str "  <text x=\"172\" y=\"10\" text-anchor=\"end\""
                " dominant-baseline=\"middle\" class=\"lb-drop-hdr\">"
                (escape-xml lb-label) "</text>\n"))
         "</g>\n")))

(defn- render-section-labels
  "SVG text labels for the WB, LB, and GF sections."
  [tournament layout n-lines]
  (let [wb-x        (x-for-col 0)
        lb-x        (x-for-col 0)
        half-box    (/ (match-box-h n-lines) 2.0)
        wb-r1-first (first (sort-by :number (filter #(= 1 (:round %)) (:WB tournament))))
        lb-r1-first (first (sort-by :number (filter #(= 1 (:round %)) (:LB tournament))))
        wb-y        (- (:y-center (get layout [:WB (:number wb-r1-first)])) half-box 5)
        lb-y        (- (:y-center (get layout [:LB (:number lb-r1-first)])) half-box 5)
        gf-pos      (get layout [:GF 0])
        gf-y        (- (:y-center gf-pos) half-box 5)]
    (str "<text x=\"" (fmt wb-x) "\" y=\"" (fmt wb-y) "\" class=\"lbl\">WINNER'S BRACKET</text>\n"
         "<text x=\"" (fmt lb-x) "\" y=\"" (fmt lb-y) "\" class=\"lbl\">LOSER'S BRACKET</text>\n"
         "<text x=\"" (fmt (:x gf-pos)) "\" y=\"" (fmt gf-y) "\" class=\"lbl\">GRAND FINALS</text>\n")))

(defn- svg-styles []
  (str "<style>\n"
       "  .hdr-wb  { fill: #3a7bd5; }\n"
       "  .hdr-lb  { fill: #d97706; }\n"
       "  .hdr-gf  { fill: #7c3aed; }\n"
       "  .hdr-txt { fill: #fff; font-size: 10px; font-weight: bold; letter-spacing: 0.6px; }\n"
       "  .p-row   { fill: #ffffff; }\n"
       "  .p-win   { fill: #dcfce7; }\n"
       "  .p-lose  { fill: #fee2e2; }\n"
       "  .p-tbd   { fill: #f1f5f9; }\n"
       "  .p-txt   { fill: #1e293b; font-size: 11px; }\n"
       "  .p-gray  { fill: #94a3b8; font-size: 11px; font-style: italic; }\n"
       "  .win-chk { fill: #16a34a; font-size: 11px; }\n"
       "  .wire    { fill: none; stroke: #94a3b8; stroke-width: 1.5; }\n"
       "  .box     { fill: none; stroke: #cbd5e1; stroke-width: 1.5; }\n"
       "  .div     { fill: none; stroke: #e2e8f0; stroke-width: 1; }\n"
       "  .lbl         { fill: #64748b; font-size: 11px; font-weight: bold; letter-spacing: 0.8px; }\n"
       "  .lb-drop     { fill: #94a3b8; font-size: 9px; font-style: italic; }\n"
       "  .lb-drop-hdr { fill: #ffffff99; font-size: 9px; font-style: italic; }\n"
       "</style>\n"))

;; ─────────────────────────────────────────────────────────────────────────────
;; Public API
;; ─────────────────────────────────────────────────────────────────────────────

(defn render-svg
  "Generate an SVG bracket diagram for the tournament as a string.

   Args:
     tournament   - the full tournament map
     display-keys - (optional) keyword or vector of keywords to show in each
                    player slot.  A single keyword shows one field alongside
                    the seed number.  A vector of keywords shows one line per
                    key: the first line includes the seed number, subsequent
                    lines show the remaining fields.  Defaults to the first
                    key in the player map (one line)."
  ([tournament]
   (render-svg tournament nil))
  ([tournament display-keys]
   (let [n-lines      (display-keys->n-lines display-keys)
         players      (:players tournament)
         first-player (first (filter some? (rest players)))
         ;; Fall back to first player key when display-keys is nil
         dkeys        (if (and (nil? display-keys) first-player)
                        (default-display-key first-player)
                        display-keys)
         layout       (compute-layout tournament n-lines)
         all-matches  (concat (:WB tournament) (:LB tournament) (:GF tournament))
         half-box     (/ (match-box-h n-lines) 2.0)
         svg-w        (+ margin (apply max (map #(+ (:x (get layout [(:bracket %) (:number %)]))
                                                     box-w)
                                                all-matches)))
         svg-h        (+ margin (apply max (map #(+ (:y-center (get layout [(:bracket %) (:number %)]))
                                                     half-box)
                                                all-matches)))]
     (str "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
          "<svg xmlns=\"http://www.w3.org/2000/svg\""
          " width=\""  (fmt svg-w) "\""
          " height=\"" (fmt svg-h) "\""
          " font-family=\"'Helvetica Neue', Arial, sans-serif\">\n"
          (svg-styles)
          (render-all-connectors tournament layout)
          (render-gf-connectors tournament layout)
          (render-section-labels tournament layout n-lines)
          (str/join (map (fn [m]
                           (let [pos (get layout [(:bracket m) (:number m)])]
                             (render-match-box m players dkeys n-lines
                                               (:x pos) (:y-center pos))))
                         all-matches))
          "</svg>\n"))))

(defn save-svg
  "Render the tournament bracket to an SVG file at path.

   Args:
     tournament   - the full tournament map
     path         - output file path (e.g. \"tournament.svg\")
     display-keys - (optional) keyword or vector of keywords; see render-svg"
  ([tournament path]
   (save-svg tournament path nil))
  ([tournament path display-keys]
   (spit path (render-svg tournament display-keys))))
