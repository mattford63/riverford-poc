(ns riverford-poc.core
  (:require [clojure.spec.alpha :as spec]
            [me.raynes.fs :as fs]
            [clucie.core :as core]
            [clucie.store :as store]
            [clucie.analysis :as analysis]
            [cljcc]))

;; ---------
;; Read in and validate recipes
;;

(def recipe-dir-name "/Users/matt/Downloads/recipes/") ;; unzipped dir of recipes

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

(defn keyword+ [s] (keyword (str *ns*) s))

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

(def id (atom 0))

(defn recipe-seq-to-map [recipe-seq]
  (let [recipe-map (into {::id (swap! id inc)} (map (fn [[x & y]] [(normalised-keyword x) (clojure.string/join "\n" y)]) (split-recipe recipe-seq)))]
    (if (not (spec/valid? ::recipe recipe-map))
      (do (spec/explain ::recipe recipe-map)
          nil)
      recipe-map)))

(defn read-recipe [recipe-file]
  (with-open [rdr (clojure.java.io/reader recipe-file)]
    (let [recipe-seq (concat ["File-Name:" (fs/base-name recipe-file)]
                             (conj (line-seq rdr) "Title:"))] ;; add Title and File-Name to seq
      (recipe-seq-to-map recipe-seq))))

(def recipe-dir (clojure.java.io/file recipe-dir-name))
(def recipe-files (filter #(.isFile %) (file-seq recipe-dir)))
(def recipes (map read-recipe recipe-files))

;; View invalid ingredients
(comment (dorun (map read-recipe recipe-files)))

;; This is the only file that doesn't conform to the spec.
;; "File: /Users/matt/Downloads/recipes/blueberry-friands.txt INVALID!!"
;; #:riverford-poc.core{:id "blueberry-friands.txt", :title "Blueberry friands", :introduction "Friands are little cakes made with ground nuts – either almonds or hazelnuts. Usually baked in an oval shape, they're suitable for a muffin tin too. You can add different flavours to them. Here we've used blueberries from our grower on Dartmoor. You could sprinkle over other berries or even a few chopped plums, when in season."} - failed: (contains? % :riverford-poc.core/ingredients) spec: :riverford-poc.core/recipe
;; #:riverford-poc.core{:id "blueberry-friands.txt", :title "Blueberry friands", :introduction "Friands are little cakes made with ground nuts – either almonds or hazelnuts. Usually baked in an oval shape, they're suitable for a muffin tin too. You can add different flavours to them. Here we've used blueberries from our grower on Dartmoor. You could sprinkle over other berries or even a few chopped plums, when in season."} - failed: (contains? % :riverford-poc.core/method) spec: :riverford-poc.core/recipe

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

  Sophisticated things are \"stem\" words, hyphenation, names,
  file-names, phrases etc."
  (let [result (sorted-map)]
    (reduce (fn [acc {:keys [consumed postition token-name]}]
              (if (= token-name :word)
                (update acc (clojure.string/lower-case consumed) (fnil inc 0))
                acc)) result matched-tokens)))

(defn combined-tokenizer [s]
  (-> s lexical-tokenizer linguistic-tokenizer))

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

(defn- update-index-term [{:keys [freq document-ids]} freq-inc document-id]
  "Update an index term by adding the frequency and adding the
  document id to a sorted set.

  Term is the literature name for word"

  {:freq ((fnil #(+ freq-inc %) 0) freq)
   :document-ids ((fnil #(sorted-insert % document-id) []) document-ids)})

(defn add-to-index [index s document-id]
  "Add to index, a sorted map, the linguistic tokens (updating
  frequencies) resulting from parsing s and registering the document
  id"
  (reduce (fn [acc [token freq]]
            (update acc token #(update-index-term % freq document-id)))
          (or index (sorted-map)) (combined-tokenizer s)))

(defn add-to-index-store [index-store maps ks id-fn]
  "Build an index store (collection of indexes) from a sequence of
  maps with the specified keys as indexes. Used id-fn to get the doc
  id from the map."
  (reduce (fn [acc m]
            (reduce (fn [a k]
                      (update a k #(add-to-index % (k m) (id-fn m)))) acc ks))
          index-store maps))

(comment
  "Simple one word searches in an index in a index-store"
  (def custom-index-store (add-to-index-store {} recipes [::file-name ::title ::introduction ::ingredients ::method] ::id ))
  (get (::file-name custom-index-store) "brussels")
  (get (::ingredients custom-index-store) "sauce"))

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

(defn intersect [index terms]
  (let [results (select-keys index terms)]
    (if (seq results)
      (reduce intersect-sorted-seq
              (-> (into (sorted-map-by (fn [k1 k2]
                                         (compare [(get-in results [k1 :freq]) k1]
                                                  [(get-in results [k2 :freq]) k2])))
                        results) ;; sort lowest to highest based on sort frequency
                  (vals)
                  (->>(map :document-ids))))
      [])))

(comment
  "Intersect searches over common words"
  (def ingredients (::ingredients custom-index-store))
  (time (intersect ingredients ["and" "the" "of"])) ;; ~50ms ["afelia.txt" "baked-peppers-with-mozzarella-pesto-ross.txt" "boulangere-potatoes.txt" "chicken-kiev.txt" "grilled-leg-of-lamb-with-swiss-chard-and.txt" "lemony-chicken-and-spinach-curry.txt" "slow-cooked-black-kale-bruschetta.txt" "swede-leek-bacon-gratin.txt" "vietnamese-style-carrot-cabbage-slaw.txt"]
  (time (intersect ingredients ["and" "the" "of"])) ;; ~0.43ms [73 343 363 553 827 1123 1231 1690 1967]
  (time (intersect ingredients ["broccoli" "stilton"])) ;; ~0.12ms [1299 1518]
)

;; --------
;; Add to Clucie
;;

(def analyzer (analysis/standard-analyzer))
(def index-store (store/memory-store))
(core/add! index-store recipes [::file-name ::title ::introduction ::ingredients ::method] analyzer)


(defn get-ids [s] (time (map ::file-name s))) ;; "Elapsed time: 0.001128 msecs"

(comment (get-ids  (core/search index-store
                                {::title "broccoli stilton soup"}
                                10
                                analyzer
                                0
                                10)) ;; ("broccoli-soup-with-stilton.txt" "cauliflower-stilton-soup.txt" "broccoli-bulghur-stilton-grapes.txt" "broccoli-soup-with-gorgonzola.txt" "broccoli-bean-pasta-soup.txt" "squash-chard-stilton-pithivier.txt" "kale-potato-28-celeriac-29-stilton-pie.txt" "purple-sprouting-broccoli-bean-and-pasta.txt" "fillet-steak-with-stilton-herb-roasted-v.txt" "squash-kale-stilton-pie.txt")
         (get-ids  (core/search index-store
                                {::ingredients "and the of"}
                                10
                                analyzer
                                0
                                10)) ;; ("broccoli-bulghur-stilton-grapes.txt" "broccoli-soup-with-stilton.txt" "cauliflower-stilton-soup.txt" "pork-shoulder-steaks-with-pear-stilton.txt" "pork-spare-rib-steaks-with-pear-stilton.txt" "spinach-baked-potatoes.txt" "squash-leek-blue-cheese-risotto.txt" "brussels-sprouts-red-onion-blue-cheese.txt" "purple-sprouting-broccoli-easy-ideas.txt" "easy-ideas-for-purple-sprouting-broccoli.txt")
         )
