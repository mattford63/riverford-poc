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

(spec/def ::file-name string?)
(spec/def ::title string?)
(spec/def ::introduction string?)
(spec/def ::ingredients string?)
(spec/def ::method string?)
(spec/def ::recipe (spec/keys :req [::file-name ::title ::introduction ::ingredients ::method]))


(defn split-recipe [recipe-seq]
  (split-by-short #(case % ("File-Name:" "Title:" "Introduction:"
                            "Ingredients:" "Method:") true false) recipe-seq))

(defn recipe-seq-to-map [recipe-seq]
  (let [recipe-map (into {} (map (fn [[x & y]] [(normalised-keyword x) (clojure.string/join "\n" y)]) (split-recipe recipe-seq)))]
    (if (not (spec/valid? ::recipe recipe-map))
      (do (spec/explain ::recipe recipe-map)
          nil)
      recipe-map)))

(defn read-recipe [recipe-file]
  (with-open [rdr (clojure.java.io/reader recipe-file)]
    (let [recipe-seq (concat ["File-Name:" (fs/base-name recipe-file)] (conj (line-seq rdr) "Title:"))] ;; add Title and File-Name to seq
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
;; Add to Clucie
;;

(def analyzer (analysis/standard-analyzer))
(def index-store (store/memory-store))
(core/add! index-store recipes [::file-name ::title ::introduction ::ingredients ::method] analyzer)

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

  Sophisticated things are \"stem\" words, hyphenation, names, phrases etc."
  (let [result (sorted-map)]
    (reduce (fn [acc {:keys [consumed postition token-name]}]
              (if (= token-name :word)
                (update acc (clojure.string/lower-case consumed) (fnil inc 0))
                acc)) result matched-tokens)))


;; --------
;; Do some searches
;;

(defn get-ids [s] (time (map ::file-name s))) ;; "Elapsed time: 0.001128 msecs"

(comment (get-ids  (core/search index-store
                                {::title "broccoli stilton soup"}
                                10
                                analyzer
                                0
                                10)) ;; ("broccoli-soup-with-stilton.txt" "cauliflower-stilton-soup.txt" "broccoli-bulghur-stilton-grapes.txt" "broccoli-soup-with-gorgonzola.txt" "broccoli-bean-pasta-soup.txt" "squash-chard-stilton-pithivier.txt" "kale-potato-28-celeriac-29-stilton-pie.txt" "purple-sprouting-broccoli-bean-and-pasta.txt" "fillet-steak-with-stilton-herb-roasted-v.txt" "squash-kale-stilton-pie.txt")
         (get-ids  (core/search index-store
                                {::ingredients "broccoli stilton"}
                                10
                                analyzer
                                0
                                10)) ;; ("broccoli-bulghur-stilton-grapes.txt" "broccoli-soup-with-stilton.txt" "cauliflower-stilton-soup.txt" "pork-shoulder-steaks-with-pear-stilton.txt" "pork-spare-rib-steaks-with-pear-stilton.txt" "spinach-baked-potatoes.txt" "squash-leek-blue-cheese-risotto.txt" "brussels-sprouts-red-onion-blue-cheese.txt" "purple-sprouting-broccoli-easy-ideas.txt" "easy-ideas-for-purple-sprouting-broccoli.txt")
         )
