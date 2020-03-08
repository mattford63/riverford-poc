(ns riverford-poc.core-test
  (:require [clojure.test :refer :all]
            [riverford-poc.core :refer :all]))

;; --------
;; Import
;;

(deftest test-split-recipe
  (testing "Splitting a recipe line sequence:"
    (testing "no separation of sections"
      (let [recipe-seq ["File-Name:"
                        "Title:"
                        "Introduction:"
                        "Ingredients:"
                        "Method:"]]
        (is (= '(("File-Name:")
                 ("Title:")
                 ("Introduction:")
                 ("Ingredients:")
                 ("Method:")) (split-recipe recipe-seq)))))
    (testing "partial mix between sections"
      (let [recipe-seq ["File-Name:"
                        "foo.txt"
                        ""
                        "Title:"
                        ""
                        "Introduction:"
                        "welcome"
                        "Ingredients:"
                        "Method:"]]
        (is (= '(("File-Name:" "foo.txt" "")
                 ("Title:" "")
                 ("Introduction:" "welcome")
                 ("Ingredients:")
                 ("Method:")) (split-recipe recipe-seq)))))))

(deftest test-recipe-seq-to-map
  (testing "Conversion of recipe seq to a map:"
    (testing "standard recipe sequence"
      (let [recipe-seq ["File-Name:"
                        "foo.txt"
                        "Title:"
                        "Little Red Riding hood"
                        "Introduction:"
                        "A lot of fairy tales"
                        "Ingredients:"
                        "Jack and Jill went up the hill"
                        "Method:"
                        "All the kings horses"]
            recipe-map #:riverford-poc.core{:file-name "foo.txt"
                                            :title "Little Red Riding hood"
                                            :introduction "A lot of fairy tales"
                                            :ingredients "Jack and Jill went up the hill"
                                            :method "All the kings horses"}]
        (is (= recipe-map (dissoc (recipe-seq-to-map recipe-seq) :riverford-poc.core/id)))))
    (testing "more than one line in sections"
      (let [recipe-seq ["File-Name:"
                        "foo.txt"
                        "Title:"
                        "Little Red Riding hood"
                        ""
                        "Goldilocks"
                        "Introduction:"
                        "A lot of fairy tales"
                        "Make our world great"
                        "Ingredients:"
                        "Jack and Jill went up the hill"
                        ""
                        "To fetch a pale of water"
                        "Method:"
                        "All the kings horses"
                        "Humpty!"]
            recipe-map #:riverford-poc.core{:file-name "foo.txt"
                                            :title "Little Red Riding hood\n\nGoldilocks"
                                            :introduction "A lot of fairy tales\nMake our world great"
                                            :ingredients "Jack and Jill went up the hill\n\nTo fetch a pale of water"
                                            :method "All the kings horses\nHumpty!"}]
        (is (= recipe-map (dissoc (recipe-seq-to-map recipe-seq) :riverford-poc.core/id)))))
    (testing "empty recipe"
      (let [recipe-seq ["File-Name:"
                        "Title:"
                        "Introduction:"
                        "Ingredients:"
                        "Method:"]
            recipe-map #:riverford-poc.core{:file-name ""
                                            :title ""
                                            :introduction ""
                                            :ingredients ""
                                            :method ""}]
        (is (= recipe-map (dissoc (recipe-seq-to-map recipe-seq) :riverford-poc.core/id)))))))

;; Tokenizer

(deftest test-lexical-tokenizer
  (testing "Convert strings to tokens:"
    (testing "simple chars"
      (let [s "a b c d e f"]
        (is (= ["a" "b" "c" "d" "e" "f" nil] (map :consumed (lexical-tokenizer s))))))
    (testing "empty string"
      (let [s ""]
        (is (= [nil] (map :consumed (lexical-tokenizer s))))))
    (testing "simple words"
      (let [s "the quick brown fox jumped over the lazy dog"
            matched-tokens nil ]
        (is (= ["the" "quick" "brown" "fox" "jumped" "over" "the" "lazy" "dog" nil]
               (map :consumed (lexical-tokenizer s))))))
    (testing "grammar and new lines"
      (let [s "What; can ! you\n do \\n @folly know's now bound."]
        (is (= ["What" "can" "you" "do" "n" "folly" "know" "s" "now" "bound" nil]
               (map :consumed (lexical-tokenizer s))))))))

(deftest test-linguistic-tokenizer
  (testing "Convert matched tokens to linguistic tokens with frequency:"
    (testing "simple case"
      (let [s "Bob, Dave, Carl, Anne."
            linguistic-tokens (sorted-map "anne" 1 "bob" 1 "carl" 1 "dave" 1)]
        (is (= {"AN" 1, "BB" 1, "KRL" 1, "TF" 1} (combined-tokenizer s)))
        (is (not (= ["bob" "dave" "carl" "anne"] (combined-tokenizer s))))))
    (testing "real recipe"
      (let [s "It may sound like a glass of superfood buzzwords, but it works splendidly. To extract as much juice as possible from leafy veg, roll them together tightly before putting them through the machine. This should yield about 300ml of juice, but will vary depending on the oomph of your juicer. The trick to getting maximum flavour is to start with the more fibrous and difficult-to-juice items, and finish with the juicier and higher-yield items; they will flush through all the preceding flavour. Ingredients are listed in the best order in which to plunge."]
        (is (= (sorted-map "0" 9,
           "0M" 2,
           "0RKH" 2,
           "A" 1,
           "ABT" 1,
           "AL" 1,
           "ANT" 3,
           "AR" 1,
           "AS" 2,
           "BFR" 1,
           "BST" 1,
           "BSWRT" 1,
           "BT" 2,
           "EXTRKT" 1,
           "FBR" 1,
           "FK" 1,
           "FLFR" 2,
           "FLX" 1,
           "FNX" 1,
           "FR" 1,
           "FRM" 1,
           "HKHR" 1,
           "IN" 2,
           "INKRT" 1,
           "IS" 1,
           "IT" 2,
           "ITM" 2,
           "JK" 3,
           "JSR" 2,
           "JT" 1,
           "KLS" 1,
           "LF" 1,
           "LK" 1,
           "LST" 1,
           "M" 1,
           "ML" 1,
           "MR" 1,
           "MX" 1,
           "MXMM" 1,
           "MXN" 1,
           "OF" 3,
           "OMF" 1,
           "ON" 1,
           "ORTR" 1,
           "PLNK" 1,
           "PRST" 1,
           "PSBL" 1,
           "PT" 1,
           "RL" 1,
           "SNT" 1,
           "SPLNTTL" 1,
           "SPRFT" 1,
           "STRT" 1,
           "T" 5,
           "TFKLT" 1,
           "TJ0" 1,
           "TPNT" 1,
           "TRKK" 1,
           "TTL" 1,
           "W0" 2,
           "WL" 2,
           "WRK" 1,
           "WX" 1,
           "XLT" 1,
           "YLT" 2,
           "YR" 1)
               (combined-tokenizer s)))))))

(deftest test-add-to-index
  (testing "Add documents to an index:"
    (testing "simple case"
      (let [s "Bob, Dave, Carl, Anne"
            id 1]
        (is (= {"AN" {:f 1, :i [1]},
                "BB" {:f 1, :i [1]},
                "KRL" {:f 1, :i [1]},
                "TF" {:f 1, :i [1]}}
               (add-to-index nil (combined-tokenizer s) id)))))
    (testing "multiple strings and file names"
      (let [s1 "Bob, Dave, Carl, Anne"
            s2 "Anne and Bob are married."
            id1 1
            id2 2]
        (is (= {"AN" {:f 2, :i [1 2]},
                "ANT" {:f 1, :i [2]},
                "AR" {:f 1, :i [2]},
                "BB" {:f 2, :i [1 2]},
                "KRL" {:f 1, :i [1]},
                "MR" {:f 1, :i [2]},
                "TF" {:f 1, :i [1]}}
                (-> nil
                    (add-to-index (combined-tokenizer s1) id1)
                    (add-to-index (combined-tokenizer s2) id2))))))))


(deftest test-add-to-index-store
  (testing "Add sequence of maps to an index store:"
    (testing "simple case"
      (let [maps [{:a "hello there"
                   :b "hello back to you"
                   :id 1}
                  {:a "Another one!"
                   :b "back at you"
                   :id 2}]]
        (is (= {:a
           {"0R" {:f 1, :i [1]},
            "AN0" {:f 1, :i [2]},
            "HL" {:f 1, :i [1]},
            "ON" {:f 1, :i [2]}},
           :riverford-poc.core/all
           {"0R" {:f 1, :i [1]},
            "AN0" {:f 1, :i [2]},
            "AT" {:f 1, :i [2]},
            "BKK" {:f 2, :i [1 2]},
            "HL" {:f 2, :i [1]},
            "ON" {:f 1, :i [2]},
            "T" {:f 1, :i [1]},
            "Y" {:f 2, :i [1 2]}},
           1
           {:a {"0R" {:f 1, :i [1]}, "HL" {:f 1, :i [1]}},
            :riverford-poc.core/all
            {"0R" {:f 1, :i [1]},
             "BKK" {:f 1, :i [1]},
             "HL" {:f 2, :i [1]},
             "T" {:f 1, :i [1]},
             "Y" {:f 1, :i [1]}},
            :b
            {"BKK" {:f 1, :i [1]},
             "HL" {:f 1, :i [1]},
             "T" {:f 1, :i [1]},
             "Y" {:f 1, :i [1]}}},
           :b
           {"AT" {:f 1, :i [2]},
            "BKK" {:f 2, :i [1 2]},
            "HL" {:f 1, :i [1]},
            "T" {:f 1, :i [1]},
            "Y" {:f 2, :i [1 2]}},
           2
           {:a {"AN0" {:f 1, :i [2]}, "ON" {:f 1, :i [2]}},
            :riverford-poc.core/all
            {"AN0" {:f 1, :i [2]},
             "AT" {:f 1, :i [2]},
             "BKK" {:f 1, :i [2]},
             "ON" {:f 1, :i [2]},
             "Y" {:f 1, :i [2]}},
            :b {"AT" {:f 1, :i [2]}, "BKK" {:f 1, :i [2]}, "Y" {:f 1, :i [2]}}}}
               (add-to-index-store {} maps [:a :b] :id)))))))

(deftest test-intersect-sorted-seq
  (testing "Sorted intersect:"
    (testing "simple cases"
      (is (= [2 3] (intersect-sorted-seq [1 2 3] [2 3 4])))
      (is (= [2 3] (intersect-sorted-seq [2 3 4] [2 3])))
      (is (= [] (intersect-sorted-seq [] [2 3 4])))
      (is (= [] (intersect-sorted-seq [] [])))
      (is (= [] (intersect-sorted-seq [2 3 4] [])))
      (is (= [] (intersect-sorted-seq [1 2 3] [4 5 6]))))))

(deftest test-union-sorted-seq
  (testing "Sorted intersect:"
    (testing "simple cases"
      (is (= [1 2 3 4] (union-sorted-seq [1 2 3] [2 3 4])))
      (is (= [2 3 4] (union-sorted-seq [2 3 4] [2 3])))
      (is (= [2 3 4] (union-sorted-seq [] [2 3 4])))
      (is (= [] (union-sorted-seq [] [])))
      (is (= [2 3 4] (union-sorted-seq [2 3 4] [])))
      (is (= [1 2 3 4 5 6] (union-sorted-seq [1 2 3] [4 5 6])))
      (is (= [1 2 3 4 5 6] (union-sorted-seq [4 5 6] [1 2 3]))))))

(deftest test-subtraction-sorted-seq
  (testing "Sorted intersect:"
    (testing "simple cases"
      (is (= [1] (subtraction-sorted-seq [1 2 3] [2 3 4])))
      (is (= [4] (subtraction-sorted-seq [2 3 4] [2 3])))
      (is (= [] (subtraction-sorted-seq [] [2 3 4])))
      (is (= [] (subtraction-sorted-seq [] [])))
      (is (= [2 3 4] (subtraction-sorted-seq [2 3 4] [])))
      (is (= [1 2 3] (subtraction-sorted-seq [1 2 3] [4 5 6])))
      (is (= [4 5 6] (subtraction-sorted-seq [4 5 6] [1 2 3]))))))

(deftest test-sorted-insert
  (testing "Sorted inserts"
    (testing "simple cases"
      (is (= [1 2 3] (sorted-insert [1 3] 2)))
      (is (= [1 3]   (sorted-insert [1 3] nil)))
      (is (= [1 2 3] (sorted-insert [2 3] 1)))
      (is (= [1 2 3] (sorted-insert [1 2] 3))))))

(deftest test-intersect
  (testing "Intersects over indexes"
    (testing "simple cases"
      (let [data [["Bob, Dave, Carl, Anne" 1]
                  ["Anne and Bob are married." 2]
                  ["Carl and Anne met for lunch" 3]]
            index-store {:test (reduce (fn [acc [s id]] (add-to-index acc (combined-tokenizer s) id)) (sorted-map) data)}]
        (is (= [1 2 3] (intersect index-store :test ["anne"])))
        (is (= [1 2 3] (intersect index-store :test ["AnNe"])))
        (is (= [2 3] (intersect index-store :test ["and"])))
        (is (= [] (intersect index-store :test ["moonshine"])))
        (is (= [] (intersect index-store :test [""])))
        (is (= [2 3] (intersect index-store :test ["and" "anne"])))
        (is (= [2] (intersect index-store :test ["and" "bob" "married"])))))))

(deftest test-union
  (testing "Unions over indexes"
    (testing "simple cases"
      (let [data [["Bob, Dave, Carl, Anne" 1]
                  ["Anne and Bob are married." 2]
                  ["Carl and Anne met for lunch" 3]]
            index-store {:test (reduce (fn [acc [s id]] (add-to-index acc (combined-tokenizer s) id)) (sorted-map) data)}]
        (is (= [1 2 3] (union index-store :test ["anne"])))
        (is (= [1 2 3] (union index-store :test ["AnNe"])))
        (is (= [2 3] (union index-store :test ["and"])))
        (is (= [2] (union index-store :test ["are"])))
        (is (= [] (union index-store :test ["moonshine"])))
        (is (= [] (union index-store :test [""])))
        (is (= [1 2 3] (union index-store :test ["and" "anne"])))
        (is (= [1 2 3] (union index-store :test ["and" "bob" "married"])))))))

(deftest test-not
  (testing "Negation of selection:"
    (testing "over intersection"
      (let [data [["Bob, Dave, Carl, Anne" 1]
                  ["Anne and Bob are married." 2]
                  ["Carl and Anne met for lunch" 3]]
            index-store {:test (reduce (fn [acc [s id]] (add-to-index acc (combined-tokenizer s) id)) (sorted-map) data)}
            ids   [1 2 3]]
        (is (= [] (not' intersect index-store :test ["anne"] ids)))
        (is (= [] (not' intersect index-store :test ["AnNe"] ids)))
        (is (= [1] (not' intersect index-store :test ["and"] ids)))
        (is (= [1 2 3] (not' intersect index-store :test ["moonshine"] ids)))
        (is (= [1 2 3] (not' intersect index-store :test [""] ids)))
        (is (= [1] (not' intersect index-store :test ["and" "anne"] ids)))
        (is (= [1 3] (not' intersect index-store :test ["and" "bob" "married"] ids)))
        ))
    (testing "over union"
      (let [data [["Bob, Dave, Carl, Anne" 1]
                  ["Anne and Bob are married." 2]
                  ["Carl and Anne met for lunch" 3]]
            index-store {:test (reduce (fn [acc [s id]] (add-to-index acc (combined-tokenizer s) id)) (sorted-map) data)}
            ids [ 1 2 3]]
        (is (= [] (not' union index-store :test ["anne"] ids)))
        (is (= [] (not' union index-store :test ["AnNe"] ids)))
        (is (= [1] (not' union index-store :test ["and"] ids)))
        (is (= [1 2 3] (not' union index-store :test ["moonshine"] ids)))
        (is (= [1 2 3] (not' union index-store :test [""] ids)))
        (is (= [] (not' union index-store :test ["and" "anne"] ids)))
        (is (= [] (not' union index-store :test ["and" "bob" "married"] ids)))
        ))))

(deftest test-merge-index-terms
  (testing "Merge index terms: "
    (testing "simple case"
      (let [term1 {:f 3 :i [1 2 3]}
            term2 {:f 2 :i [1 5]}]
        (is (= {:f 5 :i [1 2 3 5]}
               (merge-index-terms term1 term2)))
        (is (= {:f 3 :i [1 2 3]}
               (merge-index-terms term1 nil)))
        (is (= {:f 3 :i [1 2 3]}
               (merge-index-terms nil term1)))))))

(deftest test-merge-index-pair
  (testing "Add sequence of maps to an index store:"
    (testing "simple case"
      (let [maps [{:a "hello there"
                   :b "hello back to you"
                   :id 1}
                  {:a "Another one!"
                   :b "back at you"
                   :id 2}]
            index-store (add-to-index-store {} maps [:a :b] :id)]
        (is (= {"0R" {:f 1, :i [1]},
                "AN0" {:f 1, :i [2]},
                "AT" {:f 1, :i [2]},
                "BKK" {:f 2, :i [1 2]},
                "HL" {:f 2, :i [1]},
                "ON" {:f 1, :i [2]},
                "T" {:f 1, :i [1]},
                "Y" {:f 2, :i [1 2]}}
               (merge-index-pair (:a index-store) (:b index-store))))))))

(deftest test-merge-indexes
  (testing "Add sequence of maps to an index store:"
    (testing "simple case"
      (let [maps [{:a "hello there"
                   :b "hello back to you"
                   :id 1}
                  {:a "Another one!"
                   :b "back at you"
                   :id 2}
                  {:a "harry"
                   :c "amelia"
                   :id 3}]
            index-store (add-to-index-store {} maps [:a :b :c] :id)]
        (is (= {"0R" {:f 1, :i [1]},
                "AML" {:f 1, :i [3]},
                "AN0" {:f 1, :i [2]},
                "AT" {:f 1, :i [2]},
                "BKK" {:f 2, :i [1 2]},
                "HL" {:f 2, :i [1]},
                "HR" {:f 1, :i [3]},
                "ON" {:f 1, :i [2]},
                "T" {:f 1, :i [1]},
                "Y" {:f 2, :i [1 2]}}
               (merge-indexes [(:a index-store) (:b index-store) (:c index-store)])))))))

(deftest test-add-composite-index-to-index-store
  (testing "Add sequence of maps to an index store:"
    (testing "simple case"
      (let [maps [{:a "hello there"
                   :b "hello back to you"
                   :id 1}
                  {:a "Another one!"
                   :b "back at you"
                   :id 2}
                  {:a "harry"
                   :c "amelia"
                   :id 3}]
            index-store (add-to-index-store {} maps [:a :b :c] :id)]
        (is (= {"0R" {:f 1, :i [1]},
                "AML" {:f 1, :i [3]},
                "AN0" {:f 1, :i [2]},
                "AT" {:f 1, :i [2]},
                "BKK" {:f 2, :i [1 2]},
                "HL" {:f 2, :i [1]},
                "HR" {:f 1, :i [3]},
                "ON" {:f 1, :i [2]},
                "T" {:f 1, :i [1]},
                "Y" {:f 2, :i [1 2]}}
               (:all (add-composite-index-to-index-store index-store :all [:a :b :c]))))))))


(deftest test-merge-index-stores
  (testing "Merge two index stores:"
    (testing "simple case"
      (let [maps1 [{:a "hello there"
                    :b "hello back to you"
                    :id 1}
                   {:a "Another one!"
                    :b "back at you"
                    :id 2}
                   {:a "harry"
                    :c "amelia"
                    :id 3}]
            maps2 [{:a "hello there"
                    :b "hello back to you"
                    :id 4}
                   {:a "Another one!"
                    :b "back at you"
                    :id 5}
                   {:a "harry"
                    :c "amelia"
                    :id 6}]
            index-store1 (add-to-index-store {} maps1 [:a :b :c] :id)
            index-store2 (add-to-index-store {} maps2 [:a :b :c] :id)]
        (is (= {1
           {:a {"0R" {:f 1, :i [1]}, "HL" {:f 1, :i [1]}},
            :riverford-poc.core/all
            {"0R" {:f 1, :i [1]},
             "BKK" {:f 1, :i [1]},
             "HL" {:f 2, :i [1]},
             "T" {:f 1, :i [1]},
             "Y" {:f 1, :i [1]}},
            :b
            {"BKK" {:f 1, :i [1]},
             "HL" {:f 1, :i [1]},
             "T" {:f 1, :i [1]},
             "Y" {:f 1, :i [1]}},
            :c {}},
           4
           {:a {"0R" {:f 1, :i [4]}, "HL" {:f 1, :i [4]}},
            :riverford-poc.core/all
            {"0R" {:f 1, :i [4]},
             "BKK" {:f 1, :i [4]},
             "HL" {:f 2, :i [4]},
             "T" {:f 1, :i [4]},
             "Y" {:f 1, :i [4]}},
            :b
            {"BKK" {:f 1, :i [4]},
             "HL" {:f 1, :i [4]},
             "T" {:f 1, :i [4]},
             "Y" {:f 1, :i [4]}},
            :c {}},
           6
           {:a {"HR" {:f 1, :i [6]}},
            :riverford-poc.core/all {"AML" {:f 1, :i [6]}, "HR" {:f 1, :i [6]}},
            :b {},
            :c {"AML" {:f 1, :i [6]}}},
           :c {"AML" {:f 2, :i [3 6]}},
           3
           {:a {"HR" {:f 1, :i [3]}},
            :riverford-poc.core/all {"AML" {:f 1, :i [3]}, "HR" {:f 1, :i [3]}},
            :b {},
            :c {"AML" {:f 1, :i [3]}}},
           2
           {:a {"AN0" {:f 1, :i [2]}, "ON" {:f 1, :i [2]}},
            :riverford-poc.core/all
            {"AN0" {:f 1, :i [2]},
             "AT" {:f 1, :i [2]},
             "BKK" {:f 1, :i [2]},
             "ON" {:f 1, :i [2]},
             "Y" {:f 1, :i [2]}},
            :b {"AT" {:f 1, :i [2]}, "BKK" {:f 1, :i [2]}, "Y" {:f 1, :i [2]}},
            :c {}},
           :b
           {"AT" {:f 2, :i [2 5]},
            "BKK" {:f 4, :i [1 2 4 5]},
            "HL" {:f 2, :i [1 4]},
            "T" {:f 2, :i [1 4]},
            "Y" {:f 4, :i [1 2 4 5]}},
           5
           {:a {"AN0" {:f 1, :i [5]}, "ON" {:f 1, :i [5]}},
            :riverford-poc.core/all
            {"AN0" {:f 1, :i [5]},
             "AT" {:f 1, :i [5]},
             "BKK" {:f 1, :i [5]},
             "ON" {:f 1, :i [5]},
             "Y" {:f 1, :i [5]}},
            :b {"AT" {:f 1, :i [5]}, "BKK" {:f 1, :i [5]}, "Y" {:f 1, :i [5]}},
            :c {}},
           :riverford-poc.core/all
           {"0R" {:f 2, :i [1 4]},
            "AML" {:f 2, :i [3 6]},
            "AN0" {:f 2, :i [2 5]},
            "AT" {:f 2, :i [2 5]},
            "BKK" {:f 4, :i [1 2 4 5]},
            "HL" {:f 4, :i [1 4]},
            "HR" {:f 2, :i [3 6]},
            "ON" {:f 2, :i [2 5]},
            "T" {:f 2, :i [1 4]},
            "Y" {:f 4, :i [1 2 4 5]}},
           :a
           {"0R" {:f 2, :i [1 4]},
            "AN0" {:f 2, :i [2 5]},
            "HL" {:f 2, :i [1 4]},
            "HR" {:f 2, :i [3 6]},
            "ON" {:f 2, :i [2 5]}}}
               (merge-index-stores index-store1 index-store2)))))))
