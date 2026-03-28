(ns tournament.svg
  (:require [clojure.string :as str]))

;; ─────────────────────────────────────────────────────────────────────
;; Layout constants
;; ─────────────────────────────────────────────────────────────────────

(def ^:private box-w      180)  ; match box width in pixels
(def ^:private box-h       72)  ; match box height: 20 header + 26 player-a + 26 player-b
(def ^:private box-gap     24)  ; vertical gap between consecutive boxes in same round
(def ^:private round-gap   60)  ; horizontal gap between bracket columns
(def ^:private section-gap 60)  ; vertical gap between WB bottom and LB top
(def ^:private label-h     22)  ; height reserved above each section for its label
(def ^:private margin      24)  ; outer margin on all sides

(def ^:private col-step (+ box-w round-gap))  ; pixels per bracket column
(def ^:private slot-h   (+ box-h box-gap))    ; pixels per round-1 match slot

(def ^:private max-text-chars 22)  ; truncate player display text beyond this length

;; ─────────────────────────────────────────────────────────────────────
;; String helpers
;; ─────────────────────────────────────────────────────────────────────

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
      (str (subs s 0 (dec max-len)) "…")
      s)))

(defn- fmt
  "Format a floating-point coordinate as a rounded integer string."
  [x]
  (str (Math/round ^double (double x))))

;; ─────────────────────────────────────────────────────────────────────
;; Player display helpers
;; ─────────────────────────────────────────────────────────────────────

(defn- default-display-key
  "Returns the first key in the player map, used when no display-key is specified."
  [player]
  (first (keys player)))

(defn- player-text
  "Returns [display-str keyword?] for a player slot.
   seed-or-kw is an integer seed, :TBD, or :BYE.
   display-key is the player map field to show alongside the seed."
  [seed-or-kw players display-key]
  (cond
    (= seed-or-kw :TBD) ["TBD" true]
    (= seed-or-kw :BYE) ["BYE" true]
    :else
    (let [player (nth players seed-or-kw nil)
          dkey   (or display-key (when player (default-display-key player)))
          val    (when (and player dkey) (str (get player dkey "")))
          text   (truncate (str "#" seed-or-kw "  " val) max-text-chars)]
      [text false])))

(defn- row-fill-class
  "CSS class for a player row rectangle based on match result."
  [seed-or-kw winner]
  (cond
    (keyword? seed-or-kw) "p-tbd"  ; :TBD and :BYE never show win/loss color
    (nil? winner)          "p-row"
    (= seed-or-kw winner)  "p-win"
    :else                  "p-lose"))

;; ─────────────────────────────────────────────────────────────────────
;; Layout computation
;; ─────────────────────────────────────────────────────────────────────

(defn- x-for-col
  "Left x-coordinate for a bracket column (0-indexed)."
  [col]
  (+ margin (* col col-step)))

(defn- wb-y-centers
  "Returns {match-number → y-center} for all WB matches.
   Round-1 matches are spaced evenly from y-start.
   Higher rounds are vertically centered between their two WB predecessors."
  [wb y-start]
  (let [by-round (group-by :round wb)
        r1       (sort-by :number (get by-round 1))
        ;; Round 1: evenly spaced from top
        init     (into {}
                       (map-indexed
                         (fn [idx m]
                           [(:number m) (double (+ y-start (/ box-h 2) (* idx slot-h)))])
                         r1))]
    (reduce (fn [acc r]
              (reduce (fn [a m]
                        ;; Average the y-centers of the two WB matches that feed in
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
   Round-1 matches are spaced evenly from y-start.
   Dropout rounds (even): inherit y-center of their single LB predecessor.
   Consolidation rounds (odd, > 1): average y-centers of two LB predecessors."
  [lb y-start]
  (let [by-round (group-by :round lb)
        r1       (sort-by :number (get by-round 1))
        init     (into {}
                       (map-indexed
                         (fn [idx m]
                           [(:number m) (double (+ y-start (/ box-h 2) (* idx slot-h)))])
                         r1))]
    (reduce (fn [acc r]
              (reduce (fn [a m]
                        (let [pl       (:prev-left m)
                              pr       (:prev-right m)
                              lb-prevs (filter #(= :LB (:bracket %)) [pl pr])]
                          (assoc a (:number m)
                                 (case (count lb-prevs)
                                   ;; Consolidation: average two LB predecessors
                                   2 (/ (+ (get a (:number pl))
                                           (get a (:number pr)))
                                        2.0)
                                   ;; Dropout: inherit y from the single LB predecessor
                                   1 (get a (:number (first lb-prevs)))
                                   ;; Fallback (shouldn't occur for rounds > 1)
                                   (double (+ y-start (/ box-h 2)))))))
                      acc
                      (sort-by :number (get by-round r))))
            init
            (rest (sort (keys by-round))))))

(defn- compute-layout
  "Returns a map of [bracket number] → {:x left-edge-x :y-center center-y}
   for every match in the tournament."
  [tournament]
  (let [wb  (:WB tournament)
        lb  (:LB tournament)

        ;; WB section
        wb-y-start  (double (+ margin label-h))
        wb-centers  (wb-y-centers wb wb-y-start)
        wb-r1-count (count (filter #(= 1 (:round %)) wb))
        wb-bottom   (+ wb-y-start (* wb-r1-count slot-h) (- box-gap))

        ;; LB section (starts below WB with a gap)
        lb-y-start  (+ wb-bottom section-gap label-h)
        lb-centers  (lb-y-centers lb lb-y-start)

        ;; GF column: one column to the right of whatever ends last
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

;; ─────────────────────────────────────────────────────────────────────
;; SVG element rendering
;; ─────────────────────────────────────────────────────────────────────

(defn- render-connector
  "Return an SVG path string for an L-shaped wire from the right edge of
   one match box (at y1) to the left edge of the next (at y2).
   The wire goes: right → horizontal to midpoint → vertical → horizontal to dest."
  [x1 y1 x2 y2]
  (let [x1r   (+ x1 box-w)
        mid-x (/ (+ x1r x2) 2.0)]
    (str "<path class=\"wire\" d=\"M " (fmt x1r) "," (fmt y1)
         " H " (fmt mid-x) " V " (fmt y2) " H " (fmt x2) "\"/>\n")))

(defn- render-all-connectors
  "Return SVG path strings for every match-to-next-winner connector.
   Cross-section dropout lines (WB → LB) are omitted for clarity."
  [tournament layout]
  (str/join
    (keep (fn [m]
            (let [nw (:next-winner m)]
              (when nw
                (let [from-key  [(:bracket m)  (:number m)]
                      to-key    [(:bracket nw) (:number nw)]
                      from-pos  (get layout from-key)
                      to-pos    (get layout to-key)]
                  ;; Only draw same-section or section→GF connectors
                  (when (and from-pos to-pos
                             (or (= (:bracket m) (:bracket nw))
                                 (= :GF (:bracket nw))))
                    (render-connector (:x from-pos) (:y-center from-pos)
                                      (:x to-pos)   (:y-center to-pos)))))))
          (concat (:WB tournament) (:LB tournament) (:GF tournament)))))

(defn- render-match-box
  "Return an SVG <g> string for one match box.
   x is the left edge; y-center is the vertical center."
  [match players display-key x y-center]
  (let [y-top        (- y-center (/ box-h 2.0))
        [left right] (:players match)
        winner       (:winner match)
        hdr-class    (case (:bracket match) :WB "hdr-wb" :LB "hdr-lb" "hdr-gf")
        hdr-label    (if (= :GF (:bracket match))
                       "GRAND FINALS"
                       (str (name (:bracket match))
                            " · R" (:round match)
                            " · M" (inc (:number match))))
        [left-txt  left-kw?]  (player-text left  players display-key)
        [right-txt right-kw?] (player-text right players display-key)
        left-class   (row-fill-class left  winner)
        right-class  (row-fill-class right winner)
        xi           (fmt x)
        yi           (fmt y-top)]
    (str "<g transform=\"translate(" xi "," yi ")\">\n"
         "  <rect class=\"" hdr-class "\" width=\"" box-w "\" height=\"20\"/>\n"
         "  <rect class=\"" left-class  "\" y=\"20\" width=\"" box-w "\" height=\"26\"/>\n"
         "  <rect class=\"" right-class "\" y=\"46\" width=\"" box-w "\" height=\"26\"/>\n"
         "  <rect class=\"box\" width=\"" box-w "\" height=\"" box-h "\"/>\n"
         "  <line class=\"div\" x1=\"0\" y1=\"20\" x2=\"" box-w "\" y2=\"20\"/>\n"
         "  <line class=\"div\" x1=\"0\" y1=\"46\" x2=\"" box-w "\" y2=\"46\"/>\n"
         "  <text x=\"" (/ box-w 2) "\" y=\"10\" text-anchor=\"middle\""
         " dominant-baseline=\"middle\" class=\"hdr-txt\">"
         (escape-xml hdr-label) "</text>\n"
         "  <text x=\"8\" y=\"33\" dominant-baseline=\"middle\""
         " class=\"" (if left-kw? "p-gray" "p-txt") "\">"
         (escape-xml left-txt) "</text>\n"
         (when (and (not left-kw?) (= left winner))
           (str "  <text x=\"172\" y=\"33\" text-anchor=\"end\""
                " dominant-baseline=\"middle\" class=\"win-chk\">✓</text>\n"))
         "  <text x=\"8\" y=\"59\" dominant-baseline=\"middle\""
         " class=\"" (if right-kw? "p-gray" "p-txt") "\">"
         (escape-xml right-txt) "</text>\n"
         (when (and (not right-kw?) (= right winner))
           (str "  <text x=\"172\" y=\"59\" text-anchor=\"end\""
                " dominant-baseline=\"middle\" class=\"win-chk\">✓</text>\n"))
         "</g>\n")))

(defn- render-section-labels
  "Return SVG text elements labeling the WB, LB, and GF sections."
  [tournament layout]
  (let [wb-x (x-for-col 0)
        lb-x (x-for-col 0)
        ;; Label sits just above the first match box in each section
        wb-r1-first (first (sort-by :number (filter #(= 1 (:round %)) (:WB tournament))))
        lb-r1-first (first (sort-by :number (filter #(= 1 (:round %)) (:LB tournament))))
        wb-y (- (:y-center (get layout [:WB (:number wb-r1-first)])) (/ box-h 2.0) 5)
        lb-y (- (:y-center (get layout [:LB (:number lb-r1-first)])) (/ box-h 2.0) 5)
        gf-pos (get layout [:GF 0])
        gf-y   (- (:y-center gf-pos) (/ box-h 2.0) 5)]
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
       "  .lbl     { fill: #64748b; font-size: 11px; font-weight: bold; letter-spacing: 0.8px; }\n"
       "</style>\n"))

;; ─────────────────────────────────────────────────────────────────────
;; Public API
;; ─────────────────────────────────────────────────────────────────────

(defn render-svg
  "Generate an SVG bracket diagram for the tournament as a string.

   Args:
     tournament  - the full tournament map
     display-key - (optional) player map key to show alongside seed in each
                   match box (e.g. :name). Defaults to the first key in the
                   player map."
  ([tournament]
   (render-svg tournament nil))
  ([tournament display-key]
   (let [players      (:players tournament)
         first-player (first (filter some? (rest players)))
         dkey         (or display-key
                          (when first-player (default-display-key first-player)))
         layout       (compute-layout tournament)
         all-matches  (concat (:WB tournament) (:LB tournament) (:GF tournament))
         ;; SVG canvas size: bounding box of all match boxes + margin
         svg-w        (+ margin (apply max (map #(+ (:x (get layout [(:bracket %) (:number %)]))
                                                     box-w)
                                                all-matches)))
         svg-h        (+ margin (apply max (map #(+ (:y-center (get layout [(:bracket %) (:number %)]))
                                                     (/ box-h 2.0))
                                                all-matches)))]
     (str "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
          "<svg xmlns=\"http://www.w3.org/2000/svg\""
          " width=\""  (fmt svg-w) "\""
          " height=\"" (fmt svg-h) "\""
          " font-family=\"'Helvetica Neue', Arial, sans-serif\">\n"
          (svg-styles)
          ;; Connectors behind boxes
          (render-all-connectors tournament layout)
          ;; Section labels
          (render-section-labels tournament layout)
          ;; Match boxes
          (str/join (map (fn [m]
                           (let [pos (get layout [(:bracket m) (:number m)])]
                             (render-match-box m players dkey (:x pos) (:y-center pos))))
                         all-matches))
          "</svg>\n"))))

(defn save-svg
  "Render the tournament bracket to an SVG file at path.

   Args:
     tournament  - the full tournament map
     path        - output file path (e.g. \"tournament.svg\")
     display-key - (optional) player map key to show alongside seed.
                   Defaults to the first key in the player map."
  ([tournament path]
   (save-svg tournament path nil))
  ([tournament path display-key]
   (spit path (render-svg tournament display-key))))
