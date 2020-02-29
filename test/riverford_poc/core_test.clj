(ns riverford-poc.core-test
  (:require [clojure.test :refer :all]
            [riverford-poc.core :refer :all]))

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
