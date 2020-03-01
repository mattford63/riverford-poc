(ns riverford-poc.scratch)

(comment
  "Intersect searches over common words"
  (def ingredients (::ingredients custom-index-store))
  (time (intersect ingredients ["and" "the" "of"])) ;; ~50ms ["afelia.txt" "baked-peppers-with-mozzarella-pesto-ross.txt" "boulangere-potatoes.txt" "chicken-kiev.txt" "grilled-leg-of-lamb-with-swiss-chard-and.txt" "lemony-chicken-and-spinach-curry.txt" "slow-cooked-black-kale-bruschetta.txt" "swede-leek-bacon-gratin.txt" "vietnamese-style-carrot-cabbage-slaw.txt"]
  (time (intersect ingredients ["and" "the" "of"])) ;; ~0.43ms [73 343 363 553 827 1123 1231 1690 1967]
  (time (intersect ingredients ["broccoli" "stilton"])) ;; ~0.12ms [1299 1518]
  )


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


(defn get-ids [s] (time (map ::file-name s))) ;; "Elapsed time: 0.001128 msecs"
