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
        (is (= recipe-map (recipe-seq-to-map recipe-seq)))))
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
        (is (= recipe-map (recipe-seq-to-map recipe-seq)))))
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
        (is (= recipe-map (recipe-seq-to-map recipe-seq)))))))

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
        (is (= linguistic-tokens (-> s lexical-tokenizer linguistic-tokenizer)))
        (is (= (keys linguistic-tokens) (keys (-> s lexical-tokenizer linguistic-tokenizer))))
        (is (not (= ["bob" "dave" "carl" "anne"] (keys (-> s lexical-tokenizer linguistic-tokenizer)))))))
    (testing "real recipe"
      (let [s "It may sound like a glass of superfood buzzwords, but it works splendidly. To extract as much juice as possible from leafy veg, roll them together tightly before putting them through the machine. This should yield about 300ml of juice, but will vary depending on the oomph of your juicer. The trick to getting maximum flavour is to start with the more fibrous and difficult-to-juice items, and finish with the juicier and higher-yield items; they will flush through all the preceding flavour. Ingredients are listed in the best order in which to plunge."]
        (is (= (sorted-map "300ml" 1 "a" 1 "about" 1 "all" 1 "and" 3 "are" 1 "as" 2 "before" 1
                           "best" 1 "but" 2 "buzzwords" 1 "depending" 1 "difficult" 1 "extract" 1
                           "fibrous" 1 "finish" 1 "flavour" 2 "flush" 1
                           "from" 1 "getting" 1 "glass" 1 "higher" 1 "in" 2 "ingredients" 1 "is" 1 "it" 2
                           "items" 2 "juice" 3 "juicer" 1 "juicier" 1 "leafy" 1 "like" 1 "listed" 1
                           "machine" 1 "maximum" 1 "may" 1 "more" 1 "much" 1 "of" 3 "on" 1 "oomph" 1
                           "order" 1 "plunge" 1 "possible" 1 "preceding" 1 "putting" 1 "roll" 1 "should" 1
                           "sound" 1 "splendidly" 1 "start" 1 "superfood" 1 "the" 7 "them" 2 "they" 1
                           "this" 1 "through" 2 "tightly" 1 "to" 5 "together" 1 "trick" 1 "vary" 1
                           "veg" 1 "which" 1 "will" 2 "with" 2 "works" 1 "yield" 2 "your" 1)
               (-> s lexical-tokenizer linguistic-tokenizer)))))))
