(ns proton.proton-test
    (:require clojure.string)
    (:use clojure.test proton.core)
    )


(def tmps (templates "test/resources"))


(defn compare-result [tmp resultname]
    (let [output (clojure.string/replace (. tmp toString) #" |\t|\n|\r" "")]
        (let [result (clojure.string/replace (slurp resultname) #" |\t|\n|\r" "")]
            (is (= result output)))))


(deftest basicTest
    (let [tmp (get-template tmps "basic.xhtml")]
        (set-value tmp "title" "Basic Xhtml Page")
        (set-value tmp "content" "Content goes here")
        (set-value tmp "link" "Link goes here")
        (set-attribute tmp "link" "href" "http://www.duckduckgo.com")
        (compare-result tmp "test/resources/basic-result.xhtml")))

        
(deftest repeatTest
    (let [tmp (get-template tmps "repeat.xhtml")]
        (set-value tmp "title" "Repeating Xhtml Page")
        (set-value tmp "link" "This is a link to DuckDuckGo")
        (set-attribute tmp "link" "href" "http://www.duckduckgo.com")
        (repeat-element tmp "list-item" 5)
        (set-value tmp "list-item" "test0" 0)
        (set-value tmp "list-item" "test1" 1)
        (set-value tmp "list-item" "test2" 2)
        (set-value tmp "list-item" "test3" 3)
        (set-value tmp "list-item" "test4" 4)
        (compare-result tmp "test/resources/repeat-result.xhtml")))
        
        
(deftest hideTest
    (let [tmp (get-template tmps "hiding.xhtml")]
        (set-value tmp "title" "Hiding Xhtml Page")
        (hide-element tmp "hidden-element")
        (compare-result tmp "test/resources/hiding-result.xhtml")))


(deftest includeTest
    (let [tmp (get-template tmps "include1.xhtml")]
        (set-value tmp "title" "Page Title")
        (include tmp "include-content" "include2.xhtml")
        (set-value tmp "para1" "First paragraph of text")
        (set-value tmp "para2" "Second paragraph of text")
        (compare-result tmp "test/resources/include-result.xhtml")))