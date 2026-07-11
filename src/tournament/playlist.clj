(ns tournament.playlist
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.xml :as xml])
  (:import [javax.xml.parsers SAXParserFactory]
           [org.xml.sax EntityResolver InputSource XMLReader]
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
        ^XMLReader reader (.. factory newSAXParser getXMLReader)]
    ;; XMLReader.parse takes a single InputSource, so the handler and resolver
    ;; must be installed on the reader beforehand. (SAXParser's two-arg parse
    ;; would clobber the EntityResolver with the DefaultHandler.)
    (.setContentHandler reader handler)
    (.setEntityResolver reader
                        (reify EntityResolver
                          (resolveEntity [_ _ _]
                            (InputSource. (StringReader. "")))))
    (with-open [input-stream (io/input-stream source)]
      (.parse reader (InputSource. input-stream)))))

(defn- element-children
  "Return only element children (maps) of an XML node, discarding whitespace text nodes."
  [node]
  (filter map? (:content node)))

(defn- text-content
  "Return the full text of an XML node.  SAX splits text around entity
   references (e.g. &#38;) into multiple :content strings, so they must be
   rejoined rather than taking only the first."
  [node]
  (str/join (filter string? (:content node))))

(defn- dict-pairs
  "Convert a plist <dict> element into a lazy seq of [key-string value-element] pairs."
  [dict-element]
  (->> (element-children dict-element)
       (partition 2)
       (map (fn [[key-element value-element]]
              [(text-content key-element) value-element]))))

(defn- extract-track
  "Extract Name, Artist, Album, and Location string values from a plist track
   <dict> element.  Returns nil if the track has no Location (e.g. iCloud-only)."
  [track-dict]
  (let [wanted #{"Name" "Artist" "Album" "Location"}
        fields (into {}
                     (keep (fn [[field-name value-element]]
                             (when (and (contains? wanted field-name)
                                        (= :string (:tag value-element)))
                               [field-name (text-content value-element)]))
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

(defn- android-relative-path
  "Convert an iTunes Location URL into a path that AIMP on Android can resolve.

   The desktop stores absolute Windows/macOS file URIs (e.g.
   'file://localhost/F:/.../03 The Way I Feel.mp3') that mean nothing on a
   phone, so we discard everything but the filename, URL-decode it, and place
   it under the phone's music subfolder.  The resulting relative path is
   resolved by AIMP against the folder the .m3u itself lives in, so it works
   on internal storage or an SD card without a hard-coded /storage/... prefix.

   Args:
     location - iTunes Location URL string
     folder   - name of the phone folder holding the tracks (e.g. 'VGM');
                blank/nil produces a bare filename with no folder prefix
   Returns the relative path string, e.g. 'VGM/03 The Way I Feel.mp3'."
  [location folder]
  (let [decoded  (URLDecoder/decode location "UTF-8")
        filename (last (str/split decoded #"/"))]
    (if (str/blank? folder)
      filename
      (str folder "/" filename))))

(defn- bye-match?
  "True if either slot of the match is a :BYE, meaning no song is played."
  [match]
  (some #(= :BYE %) (:players match)))

(defn- matches-for-round
  "Return the playable matches in the given bracket and round, sorted by match
   number.  Matches decided by a bye are excluded, since no song is played."
  [tournament bracket round]
  (->> (get tournament bracket)
       (filter #(= round (:round %)))
       (remove bye-match?)
       (sort-by :number)))

(defn playlist-path
  "Derive the M3U output path from an EDN save path, bracket, and round.
   An optional label is appended before the extension to distinguish variants.
   Examples:
     'songs.edn', :WB, 1           → 'songs-WB-R1-playlist.m3u'
     'songs.edn', :WB, 1, \"aimp\"   → 'songs-WB-R1-playlist-aimp.m3u'"
  ([edn-path bracket round]
   (playlist-path edn-path bracket round nil))
  ([edn-path bracket round label]
   (str/replace edn-path #"\.edn$"
                (str "-" (name bracket) "-R" round "-playlist"
                     (when label (str "-" label)) ".m3u"))))

(defn generate-playlist
  "Generate an M3U playlist for all matches in a bracket+round.

   Songs are ordered match-by-match: both songs for match 1, then match 2, etc.
   Within each match the left/A player comes first.

   Returns {:m3u string :matched n :unmatched [seed ...]} where :unmatched lists
   seeds that could not be found in the iTunes track index.  Byes (matches with
   no opponent) contribute no songs to either count.

   The 4-arg arity writes decoded iTunes file:// locations (for iTunes on the
   desktop).  The 5-arg arity takes location->path, a function mapping an
   iTunes Location URL to the path string to write, so callers can target other
   players (e.g. AIMP on Android via android-relative-path).

   Args:
     tournament     - the tournament map
     track-index    - map from build-track-index
     bracket        - :WB, :LB, or :GF
     round          - integer round number
     location->path - fn from Location URL to the path string written to the M3U"
  ([tournament track-index bracket round]
   (generate-playlist tournament track-index bracket round decode-location))
  ([tournament track-index bracket round location->path]
   (let [players   (:players tournament)
         matches   (matches-for-round tournament bracket round)
         lines     (transient ["#EXTM3U"])
         matched   (volatile! 0)
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
                 (conj! lines (location->path location))
                 (vswap! matched inc))
             (conj! unmatched seed)))))
     {:m3u       (str/join "\n" (persistent! lines))
      :matched   @matched
      :unmatched (persistent! unmatched)})))

(defn- write-and-report
  "Write an M3U string to output-path (UTF-8) and print a summary, listing any
   seeds that could not be matched in the iTunes library.

   Args:
     tournament  - the tournament map (used to describe unmatched seeds)
     output-path - destination .m3u file path
     result      - a generate-playlist result {:m3u :matched :unmatched}"
  [tournament output-path {:keys [m3u matched unmatched]}]
  (with-open [writer (io/writer output-path :encoding "UTF-8")]
    (.write writer m3u))
  (println (str "  Saved: " output-path))
  (println (str "  Songs: " matched
                " matched, " (count unmatched) " not found"))
  (when (seq unmatched)
    (let [players (:players tournament)]
      (println "  Could not match these seeds in the iTunes library:")
      (doseq [seed unmatched]
        (let [p (nth players seed)]
          (println (str "    Seed " seed ": "
                        (get p :name) " — "
                        (get p :artist) " — "
                        (get p :album))))))))

(defn save-playlist
  "Parse an iTunes library XML, generate an M3U playlist for the given
   bracket+round, and write it to output-path.

   The paths written are the decoded iTunes file:// locations, so the playlist
   is meant for iTunes on the same computer.  For AIMP on Android see
   save-aimp-playlist.

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
    (write-and-report tournament output-path
                      (generate-playlist tournament track-index bracket round))))

(defn save-aimp-playlist
  "Parse an iTunes library XML, generate an M3U playlist for the given
   bracket+round, and write it to output-path using relative paths for AIMP on
   Android.

   Each entry is written as '<folder>/<filename>' (just the track's filename
   under the given phone folder), so the .m3u must be placed at the root of
   that folder's parent on the phone and every track must live directly inside
   <folder> with the same filename it has on the desktop.

   Prints a summary and warns about any seeds not found in the library.

   Args:
     tournament  - the tournament map
     itunes-path - path to iTunes Music Library XML
     output-path - destination .m3u file path
     bracket     - :WB, :LB, or :GF
     round       - integer round number
     folder      - phone folder holding the tracks (e.g. 'VGM')"
  [tournament itunes-path output-path bracket round folder]
  (let [tracks      (parse-itunes-library itunes-path)
        track-index (build-track-index tracks)
        matches     (matches-for-round tournament bracket round)]
    (when (empty? matches)
      (println (str "  No matches found for " (name bracket) " round " round "."))
      (System/exit 1))
    (write-and-report tournament output-path
                      (generate-playlist tournament track-index bracket round
                                         #(android-relative-path % folder)))))
