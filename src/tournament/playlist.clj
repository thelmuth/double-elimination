(ns tournament.playlist
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.xml :as xml])
  (:import [javax.xml.parsers SAXParserFactory]
           [org.xml.sax EntityResolver InputSource]
           [java.io StringReader]
           [java.net URLDecoder]))

;; ─────────────────────────────────────────────────────────────────────────────
;; iTunes XML parsing
;; ─────────────────────────────────────────────────────────────────────────────

(defn- startparse-no-dtd
  "SAX startparse fn with external DTD loading disabled, so the parser never
   makes network requests to fetch Apple's plist DTD."
  [source handler]
  (let [factory (doto (SAXParserFactory/newInstance)
                  (.setFeature "http://apache.org/xml/features/nonvalidating/load-external-dtd" false)
                  (.setFeature "http://xml.org/sax/features/validation" false))
        reader  (.. factory newSAXParser getXMLReader)]
    (.setEntityResolver reader
                        (reify EntityResolver
                          (resolveEntity [_ _ _]
                            (InputSource. (StringReader. "")))))
    (.parse reader source handler)))

(defn- element-children
  "Return only element children (maps) of an XML node, discarding whitespace text nodes."
  [node]
  (filter map? (:content node)))

(defn- dict-pairs
  "Convert a plist <dict> element into a lazy seq of [key-string value-element] pairs."
  [dict-element]
  (->> (element-children dict-element)
       (partition 2)
       (map (fn [[k v]] [(first (:content k)) v]))))

(defn- extract-track
  "Extract Name, Artist, Album, and Location string values from a plist track
   <dict> element.  Returns nil if the track has no Location (e.g. iCloud-only)."
  [track-dict]
  (let [wanted #{"Name" "Artist" "Album" "Location"}
        fields (into {}
                     (keep (fn [[k v]]
                             (when (and (contains? wanted k)
                                        (= :string (:tag v)))
                               [k (first (:content v))]))
                           (dict-pairs track-dict)))]
    (when (contains? fields "Location") fields)))

(defn parse-itunes-library
  "Parse an iTunes Music Library XML file and return a sequence of track maps
   with string keys 'Name', 'Artist', 'Album', 'Location'.
   Tracks without a Location are excluded.

   Args:
     xml-path - path to the iTunes Music Library XML file"
  [xml-path]
  (println (str "  Parsing " xml-path " ..."))
  (let [parsed      (xml/parse (io/file xml-path) startparse-no-dtd)
        top-dict    (first (element-children parsed))
        top-pairs   (dict-pairs top-dict)
        tracks-dict (->> top-pairs
                         (filter #(= "Tracks" (first %)))
                         first
                         second)]
    (->> (dict-pairs tracks-dict)
         (map (fn [[_ track-dict]] (extract-track track-dict)))
         (filter some?)
         doall)))

;; ─────────────────────────────────────────────────────────────────────────────
;; Track matching
;; ─────────────────────────────────────────────────────────────────────────────

(defn- normalize [s]
  (str/lower-case (str/trim (str s))))

(defn- track-key
  "Normalized [name artist album] key for an iTunes track map (string keys)."
  [track]
  [(normalize (get track "Name"))
   (normalize (get track "Artist"))
   (normalize (get track "Album"))])

(defn- player-key
  "Normalized [name artist album] key for a tournament player map (keyword keys)."
  [player]
  [(normalize (get player :name))
   (normalize (get player :artist))
   (normalize (get player :album))])

(defn build-track-index
  "Build a map from normalized [name artist album] → iTunes Location URL."
  [tracks]
  (into {} (map (fn [t] [(track-key t) (get t "Location")])) tracks))

;; ─────────────────────────────────────────────────────────────────────────────
;; M3U generation
;; ─────────────────────────────────────────────────────────────────────────────

(defn- decode-location
  "URL-decode an iTunes Location string (e.g. %20 → space) while keeping the
   file:// scheme intact so iTunes on Windows can open it."
  [location]
  (URLDecoder/decode location "UTF-8"))

(defn- matches-for-round
  "Return matches in the given bracket and round, sorted by match number."
  [tournament bracket round]
  (->> (get tournament bracket)
       (filter #(= round (:round %)))
       (sort-by :number)))

(defn playlist-path
  "Derive the M3U output path from an EDN save path, bracket, and round.
   Example: 'songs.edn', :WB, 1 → 'songs-WB-R1-playlist.m3u'"
  [edn-path bracket round]
  (str/replace edn-path #"\.edn$"
               (str "-" (name bracket) "-R" round "-playlist.m3u")))

(defn generate-playlist
  "Generate an M3U playlist for all matches in a bracket+round.

   Songs are ordered match-by-match: both songs for match 1, then match 2, etc.
   Within each match the left/A player comes first.

   Returns {:m3u string :unmatched [seed ...]} where :unmatched lists seeds
   that could not be found in the iTunes track index.

   Args:
     tournament   - the tournament map
     track-index  - map from build-track-index
     bracket      - :WB, :LB, or :GF
     round        - integer round number"
  [tournament track-index bracket round]
  (let [players   (:players tournament)
        matches   (matches-for-round tournament bracket round)
        lines     (transient ["#EXTM3U"])
        unmatched (transient [])]
    (doseq [match matches
            seed  (:players match)]
      (when (integer? seed)
        (let [player   (nth players seed)
              location (get track-index (player-key player))]
          (if location
            (do (conj! lines (str "#EXTINF:-1,"
                                  (get player :artist "") " - "
                                  (get player :name "")))
                (conj! lines (decode-location location)))
            (conj! unmatched seed)))))
    {:m3u       (str/join "\n" (persistent! lines))
     :unmatched (persistent! unmatched)}))

(defn save-playlist
  "Parse an iTunes library XML, generate an M3U playlist for the given
   bracket+round, and write it to output-path.

   Prints a summary and warns about any seeds not found in the library.

   Args:
     tournament  - the tournament map
     itunes-path - path to iTunes Music Library XML
     output-path - destination .m3u file path
     bracket     - :WB, :LB, or :GF
     round       - integer round number"
  [tournament itunes-path output-path bracket round]
  (let [tracks      (parse-itunes-library itunes-path)
        track-index (build-track-index tracks)
        matches     (matches-for-round tournament bracket round)]
    (when (empty? matches)
      (println (str "  No matches found for " (name bracket) " round " round "."))
      (System/exit 1))
    (let [{:keys [m3u unmatched]} (generate-playlist tournament track-index bracket round)]
      (with-open [writer (io/writer output-path :encoding "UTF-8")]
        (.write writer m3u))
      (println (str "  Saved: " output-path))
      (println (str "  Songs: " (- (* 2 (count matches)) (count unmatched))
                    " matched, " (count unmatched) " not found"))
      (when (seq unmatched)
        (let [players (:players tournament)]
          (println "  Could not match these seeds in the iTunes library:")
          (doseq [seed unmatched]
            (let [p (nth players seed)]
              (println (str "    Seed " seed ": "
                            (get p :name) " — "
                            (get p :artist) " — "
                            (get p :album))))))))))
