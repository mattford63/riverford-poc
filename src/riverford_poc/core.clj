(ns riverford-poc.core
  (:require [clojure.spec.alpha :as spec]
            [me.raynes.fs :as fs]
            [clucie.core :as core]
            [clucie.store :as store]
            [clucie.analysis :as analysis]))

;; ---------
;; Read in and validate recipes
;;

(def recipe-dir-name "/Users/matt/Downloads/recipes/") ;; unzipped dir of recipes

;; https://clojuredocs.org/clojure.core/split-with#example-5e48288ce4b0ca44402ef839
(defn split-by [pred coll]
  (lazy-seq
    (when-let [s (seq coll)]
      (let [!pred (complement pred)
            [xs ys] (split-with !pred s)]
        (if (seq xs)
          (cons xs (split-by pred ys))
          (let [skip (take-while pred s)
                others (drop-while pred s)
                [xs ys] (split-with !pred others)]
            (cons (concat skip xs)
                  (split-by pred ys))))))))

(defn keyword+ [s] (keyword (str *ns*) s))

(defn normalised-keyword [s]
  (-> s
      clojure.string/lower-case
      (clojure.string/replace ":" "")
      keyword+))

(spec/def ::id string?)
(spec/def ::title string?)
(spec/def ::introduction string?)
(spec/def ::ingredients string?)
(spec/def ::method string?)
(spec/def ::recipe (spec/keys :req [::id ::title ::introduction ::ingredients ::method]))

(defn recipe-to-map [recipe-seq]
  (let [split-recipe (split-by #(case % ("Id:" "Title:" "Introduction:"
                                         "Ingredients:" "Method:") true
                                      false) recipe-seq)]
    (into {} (map (fn [[x & y]] [(normalised-keyword x) (clojure.string/join y)]) split-recipe))))

(defn read-recipe [recipe-file]
  (with-open [rdr (clojure.java.io/reader recipe-file)]
    (let [recipe-seq (concat [:id (fs/base-name recipe-file)] (conj (line-seq rdr) "Title:")) ;; add Title and Id to seq
          recipe (recipe-to-map recipe-seq)]
      (if (not (spec/valid? ::recipe recipe))
        (do (prn (str "File: " recipe-file " INVALID!!"))
            (spec/explain ::recipe recipe))
        recipe))))

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
(core/add! index-store recipes [::id ::title ::introduction ::ingredients ::method] analyzer)

;; --------
;; Do some searches
;;

(defn get-ids [s] (time (map ::id s))) ;; "Elapsed time: 0.001128 msecs"

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
