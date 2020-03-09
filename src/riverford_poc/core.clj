(ns riverford-poc.core
  (:require [clojure.spec.alpha :as spec]
            [me.raynes.fs :as fs]
            [cljcc]
            [clj-memory-meter.core :as mm]
            [clj-fuzzy.porter :as porter]
            [clj-fuzzy.phonetics :as pho]
            [clojure.core.reducers :as r]))

;; ---------
;; Read in and validate recipes
;;

(defn split-by-short [pred coll]
  "Splits a sequence by predicate but always splits on predicate success"
  (lazy-seq
   (when-let [s (seq coll)]
     (let [[x & xs] coll
           !pred (complement pred)]
       (let [skip (take-while !pred xs)
             others (drop-while !pred xs)]
         (cons (cons x skip)
               (split-by-short pred others)))))))

(defn keyword+ [s] (keyword "riverford-poc.core" s))

(defn normalised-keyword [s]
  (-> s
      clojure.string/lower-case
      (clojure.string/replace ":" "")
      keyword+))

(spec/def ::id int?)
(spec/def ::file-name string?)
(spec/def ::title string?)
(spec/def ::introduction string?)
(spec/def ::ingredients string?)
(spec/def ::method string?)
(spec/def ::recipe (spec/keys :req [::id ::file-name ::title ::introduction ::ingredients ::method]))


(defn split-recipe [recipe-seq]
  (split-by-short #(case % ("File-Name:" "Title:" "Introduction:"
                            "Ingredients:" "Method:") true false) recipe-seq))

;; Global id state
(def id (atom 0))
(def ids (atom []))
(def id->file (atom {}))

(defn recipe-seq-to-map [recipe-seq]
  (let [id (swap! id inc)
        recipe-map (into {::id id}
                         (map (fn [[x & y]] [(normalised-keyword x) (clojure.string/join "\n" y)]) (split-recipe recipe-seq)))]
    (if (not (spec/valid? ::recipe recipe-map))
      (do (spec/explain ::recipe recipe-map)
          nil)
      (do (swap! ids conj id)
          (swap! id->file assoc id (::file-name recipe-map))
          recipe-map))))

(defn read-recipe [recipe-file]
  (with-open [rdr (clojure.java.io/reader recipe-file)]
    (let [recipe-seq (concat ["File-Name:" (fs/base-name recipe-file)]
                             (conj (line-seq rdr) "Title:"))] ;; add Title and File-Name to seq
      (recipe-seq-to-map recipe-seq))))

(defn read-recipes [recipe-files]
  (let [n (int (Math/ceil (/ (count recipe-files) (.. Runtime getRuntime availableProcessors))))]
    (reset! id 0)
    (reset! ids [])
    (reset! id->file {})
    (into [] (r/fold
              n
              (r/monoid r/cat (constantly []))
              ((map read-recipe) conj)
              (into [] recipe-files)))))

;; --------
;; Tokenizer
;;

(def tokens
  #{{:name :word :pattern #"\w+" } ; match all strings
    {:ignore true :pattern #"\W+" }}) ; ignore everything else

(def lexical-tokenizer
  "Tokenize a string"
  (cljcc/make-lexer tokens))

(defn linguistic-tokenizer [matched-tokens]
  "This is where increased sophistication can come in but for now we
  keep it very simple, ignoring position, lower-casing and returning
  sorted map of frequency.

  We use the Porter stemmer
  https://www.cs.odu.edu/~jbollen/IR04/readings/readings5.pdf

  We also create the metaphone (phonetic representation), so bad
  spellings etc might work"

  (let [result (sorted-map)]
    (reduce (fn [acc {:keys [consumed postition token-name]}]
              (if (= token-name :word)
                (update acc (-> consumed porter/stem pho/metaphone) (fnil inc 0))
                acc))
            result matched-tokens)))

(defn combined-tokenizer [s]
  (-> (str s) lexical-tokenizer linguistic-tokenizer))

;; --------
;; Searches
;;

;; https://stackoverflow.com/questions/14160830/idiomatic-efficient-clojure-way-to-intersect-two-a-priori-sorted-vectors
(defn intersect-sorted-seq [x y]
  (loop [i 0, j 0, r (transient [])]
    (let [xi (nth x i nil), yj (nth y j nil)]
      (cond
        (not (and xi yj)) (persistent! r)
        (< xi yj) (recur (inc i) j r)
        (> xi yj) (recur i (inc j) r)
        :else (recur (inc i) (inc j) (conj! r xi))))))

(defn union-sorted-seq [x y]
  (loop [i 0, j 0, r (transient [])]
    (let [xi (nth x i nil), yj (nth y j nil)]
      (cond
        (and (nil? xi) (nil? yj)) (persistent! r)
        (nil? yj) (recur (inc i) j (conj! r xi)) ;; we could safely concat the remainder
        (nil? xi) (recur i (inc j) (conj! r yj)) ;; we could safely concat the remainder
        (< xi yj) (recur (inc i) j (conj! r xi))
        (> xi yj) (recur i (inc j) (conj! r yj))
        :else (recur (inc i) (inc j) (conj! r xi))))))

(defn subtraction-sorted-seq [x y]
  (loop [i 0, j 0, r (transient [])]
    (let [xi (nth x i nil), yj (nth y j nil)]
      (cond
        (and (nil? xi) (nil? yj)) (persistent! r)
        (nil? yj) (recur (inc i) j (conj! r xi))
        (nil? xi) (persistent! r)
        (< xi yj) (recur (inc i) j (conj! r xi))
        (> xi yj) (recur i (inc j) r)
        :else (recur (inc i) (inc j) r)))))

(defn normalise-terms [terms]
  (->> terms
       (map porter/stem)
       (map pho/metaphone)))

(defn match-keys [index terms]
  "We can increasing sophistication to how search words are matched to
  index keys here.

  This is alternative approach to adding words stems etc at index
  creation time"
  (select-keys index terms))

(defn relevance-identity [seq index-store index terms]
  "Does nothing returns the sequence in order"
  seq)

(defn get-freq [index-store index term id]
  (get-in index-store [id index term :f] 0))

(defn relevance-intersect-1st-term-freq [seq index-store index terms]
  "Uses the first term in the intersection and orders based on it's frequency

   This is a very naive implementation of relevance, used just to show
   off the capability we have.

   Literature search is required to see what algorithms exist in this
   space.  These fns could even be passed as part of the search DSL."
  (let [term (first terms)]
    (sort-by (partial get-freq index-store index term) #(compare %2 %1) seq)))

(defn reduce-by-freq [reducer relevancy-sorter]
  "This function for efficiency sorts the results on frequency: lowest
   frequency first so the least work is done on reduce operations.  It
   first selects the sorted vector of document ids before reducing
   over them."
  (fn [index-store index terms]
    (let [normalised-terms (normalise-terms terms)
          results (match-keys (get index-store index) normalised-terms)]
      (if (seq results)
        (-> (reduce reducer
                    (-> (into (sorted-map-by (fn [k1 k2]
                                               (compare [(get-in results [k1 :f]) k1]
                                                        [(get-in results [k2 :f]) k2])))
                              results) ;; sort lowest to highest based on sort frequency
                       (vals)
                       (->>(map :i))))
            (relevancy-sorter index-store index normalised-terms))
        []))))

;;--------
;; Logical operators

(def intersect (reduce-by-freq intersect-sorted-seq relevance-identity))
(def union (reduce-by-freq union-sorted-seq relevance-identity))

(def intersect-rel (reduce-by-freq intersect-sorted-seq relevance-intersect-1st-term-freq))
(def union-rel (reduce-by-freq union-sorted-seq relevance-intersect-1st-term-freq))

;; (defn not' [logical-operator index-store index terms]
;;   "If we create an index of ::ids then we can use it as an index of
;;   records.  However it requires a re-factor to use index-stores
;;   everywhere.  So for time reasons we cheat with an atom to record
;;   this info."
;;   (let [results (logical-operator index-store index terms)
;;         id-index (get index-store index)]
;;     (map #(Integer/parseInt %) (keys (apply dissoc id-index (map str results))))))

(defn not'
  "TODO: DO NOT USE IN CURRENT STATE
  We allow an optional set of ids to be passed so that we can easily
  test else we default to the atom containing all used ids.

  This is also problematic as what we really need is the list of all
  ids for the index of question."
  ([logical-operator index-store index terms]
   (not' logical-operator index-store index terms @ids))
  ([logical-operator index-store index terms ids]
   (let [results (logical-operator index-store index terms)]
     (subtraction-sorted-seq ids results))))

;; --------
;; Index Operations
;;

(defn merge-index-terms [t1 t2]
  (let [freq1 (or (:f t1) 0)
        freq2 (or (:f t2) 0)
        ids1  (or (:i t1) [])
        ids2  (or (:i t2) [])]
    {:f (+ freq1 freq2)
     :i (union-sorted-seq ids1 ids2)}))

(defn merge-index-pair [xs ys]
  (reduce (fn [acc [k m]] (update acc k #(merge-index-terms % m)))
          xs ys))

(defn merge-indexes [indexes]
  (reduce merge-index-pair indexes))

(defn merge-index-stores [is1 is2]
  (let [ks (distinct (concat (keys is1) (keys is2) [:all]))]
    (reduce (fn [acc k]
              (let [is1k (get is1 k)
                    is2k (get is2 k)]
                (cond
                  (and is1k is2k) (assoc acc k (merge-index-pair is1k is2k))
                  is1k (assoc acc k is1k)
                  is2k (assoc acc k is2k)
                  :else acc
                  ))) {} ks)))

(defn add-composite-index-to-index-store [index-store name indexes]
  (assoc index-store name (merge-indexes (-> (select-keys index-store indexes) vals))))

;; ---------
;; Reverse Index
;;

(defn sorted-insert [sorted-seq n]
  (loop [s sorted-seq, a []]
    (cond
      (nil? n)         s
      (empty? s)       (conj a n)
      (< (first s) n)  (recur (rest s) (conj a (first s)))
      (= (first s) n)  (recur (rest s) a)
      :else            (apply conj a n s))))

(defn- update-index-term [{:keys [f i]} freq-inc document-id]
  "Update an index term by adding the frequency and adding the
  document id to a sorted set.

  Term is the literature name for word"

  {:f ((fnil #(+ freq-inc %) 0) f)
   :i ((fnil #(sorted-insert % document-id) []) i)})

(defn add-to-index [index tokens document-id]
  "Add to index, a sorted map, the linguistic tokens (updating
  frequencies) resulting from parsing s and registering the document
  id"
  (reduce (fn [acc [token freq]]
            (update acc token #(update-index-term % freq document-id)))
          (or index (sorted-map)) tokens))

(defn add-to-index-store [index-store maps ks id-fn]
  "Build an index store (collection of indexes) from a sequence of
  maps with the specified keys as indexes. Used id-fn to get the doc
  id from the map.

  Now also stores an index per document added"
  (let [rmaps (into [] maps)
        n (int (Math/ceil (/ (count rmaps) (.. Runtime getRuntime availableProcessors))))]
    (r/fold n
     (r/monoid merge-index-stores (constantly index-store))
     (fn [acc m]
       (r/reduce (fn [a k]
                   (let [tokens (combined-tokenizer (k m))
                         id (id-fn m)]
                     (-> a
                         (update k #(add-to-index % tokens id)) ;; adds to section indexes
                         (update ::all #(add-to-index % tokens id)) ;; adds to a global :all index
                         (assoc-in [id k] (add-to-index nil tokens id)) ;; stores an index per document per section
                         (update-in [id ::all ] #(add-to-index % tokens id)) ;; stores an :all index per document
                         ))) acc ks))
     rmaps)))

;; -------
;; REPL helper functions
;;

(defmacro wrap-time [txt body]
  `(do (println ~txt)
       (time  ~body)))

(defn display [n results]
  (let [c (count results)]
    (println "\n" c "results:\n- "
             (clojure.string/join "\n-  " (take n (map #(get @id->file %) results)))
             (if (> c n) "..." )
             "\n")))

(defn display-extra [n results index-store index terms]
  (let [c (count results)
        normalised-term (first (normalise-terms terms))]
    (println "\n" c "results:\n- "
             (clojure.string/join "\n-  " (take n (map (juxt #(get @id->file %) #(get-freq index-store index normalised-term %)) results )))
             (if (> c n) "..." )
             "\n")))

(def d10 (partial display 10))

(defmacro d [body]
  `(time (d10 ~body)))

;; -------
;; Example time :-)
;;

(comment
  ;; C-x C-e to execute within a comment not C-c C-c

  ;; Read recipes
  ;; - dir location assumes unzipped dir of recipes
  (do (def recipe-dir-name "/home/matt/Downloads/recipes/")
      (def recipe-dir (clojure.java.io/file recipe-dir-name))
      (def recipe-files (filter #(.isFile %) (file-seq recipe-dir)))
      (def recipes (wrap-time "Reading files..." (read-recipes recipe-files))))

  ;; Build an index store
  ;; - add an index by recipe section
  ;; - this takes a little whilst as it's search that's optimised not ingestion
  ;; - on my machine it takes ~60 seconds
  (def index-store (wrap-time "Building index-store..."
                              (do
                                  (add-to-index-store {} recipes [::title ::introduction ::method ::ingredients] ::id))))

  ;; How big is the index-store?
  ;; ~150MB 15x roughly the size of the unzipped files on disk!
  ;; The storage format is not optimized!!
  ;; The file indexes can be made much more efficient
  ;; - some simple name key name changes reduced the size to 125MB 12x increase
  (mm/measure index-store)

  ;; An example search and expected output
  (d (intersect-rel index-store ::all ["riverford"]))
  ;; This example may not reflect the current implementation: it's
  ;; to show what to expect in the output.
  ;; 33 results:
  ;; -  sweet-chilli-chicken-wings-with-slaw.txt
  ;; -  lamb-and-mint-burgers-tzatziki-greek-sal.txt
  ;; -  turnips-caramelised-butter-wine.txt
  ;; -  asian-coleslaw-with-peanuts-chilli.txt
  ;; -  kale-and-chorizo-soup.txt
  ;; -  lemon-and-thyme-roasted-chicken.txt
  ;; -  courgette-fennel-kohlrabi-salad.txt
  ;; -  chinese-style-pulled-pork.txt
  ;; -  purple-sprouting-broccoli-with-hollandai.txt
  ;; -  country-pate.txt ...
  ;; "Elapsed time: 1.691325 msecs"

  ;; To search for a single term use an intersection with one term
  ;; To restrict to a recipe section, select the appropriate index.
  ;; - notice the difference between the relevance and non-relevance sorted results.
  (d (intersect index-store ::ingredients ["broccoli"]))
  (display-extra 10 (intersect-rel index-store ::ingredients ["broccoli"]) index-store ::ingredients ["broccoli"])
  (d (intersect-rel index-store ::ingredients ["broccoli"]))

  ;; Highlight all the different accepted spellings supported.
  (d (intersect-rel index-store ::ingredients ["broccoli"]))
  (d (intersect-rel index-store ::ingredients ["brocoli"]))
  (d (intersect-rel index-store ::ingredients ["brokolee"]))

  (d (intersect index-store ::ingredients ["chicken"]))
  (d (intersect-rel index-store ::ingredients ["chicken"]))
  (d (intersect-rel index-store ::ingredients ["spinach" "chicken" "garlic"]))
  (d (intersect-rel index-store ::ingredients ["spinitch" "chickon" "garlik"]))

  ;; To search everywhere in the recipe use the ::all index
  ;; Use intersect and a vector of terms to do an AND
  (d (intersect-rel index-store ::all ["broccoli" "stilton" "soup"]))

  ;; To search with an OR use union
  ;; With 1st term relevance
  (d (union-rel index-store ::method  ["fry" "heat" "cook"]))
  (d (union-rel index-store ::method  ["broccoli" "stilton" "soup"]))

  ;; We can use the NOT operator only with the :all index at present
  ;; It's also limited to non-relevance search results.
  (d (not' intersect index-store ::all  ["and" "the" "if" "or" "a"] ))

  ;; The most taxing searches will involve intersections of common
  ;; words across the ::all index
  ;; - on average these all seem to be <5ms
  (d (intersect index-store ::all ["i" "and" "the" "if" "or" "a"]))
  (d (union index-store ::all ["i" "and" "the" "if" "or" "a"]))

  ;; Complex queries can be written
  ;; We want all recipes that have cheese as an ingredient but not
  ;; Stilton and have baked or fry in the method.
  ;; This is the longest query time wise at ~1.4ms
  ;; - relevancy is hard to manage here as the logic operators require
  ;;   the ordering to exist to work properly. To fix more general
  ;;   intersection functions (i.e, ones that don't rely on ordering) can
  ;;   used instead.

  (d (intersect-sorted-seq (subtraction-sorted-seq (intersect index-store ::ingredients ["cheese"])
                                                   (intersect index-store ::ingredients  ["stilton"]))
                           (union index-store ::method ["baked" "fry"])))

  ;; Add a new file to the store, creates a new index-store.
  ;; - maybe index-store should be atomic

  (add-to-index-store index-store [(read-recipe JAVA_FILE_OBJECT)] [::title ::introduction ::method ::ingredients] ::id)
  )
