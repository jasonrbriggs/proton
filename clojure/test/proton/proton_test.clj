(ns proton.proton-test
  (:require [clojure.test :refer :all]
            [proton.proton :refer :all]
            [clojure.string :as str]))

(def tmps (templates "test/resources"))

(defn compare-result [tmp resultname]
    (let [output (str/replace (. tmp toString) #" |\t|\n|\r" "")]
        (let [result (str/replace (slurp resultname) #" |\t|\n|\r" "")]
            (is (= result output)))))

(deftest basicTest
    (let [tmp (get-template tmps "basic.xhtml")]
        (set-value tmp "title" "Basic Xhtml Page")
        (set-value tmp "content" "Content goes here")
        (set-value tmp "link" "Link goes here")
        (set-attribute tmp "link" "href" "http://www.google.com")
        (compare-result tmp "test/resources/basic-result.xhtml")))