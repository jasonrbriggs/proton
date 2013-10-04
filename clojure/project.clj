(defproject proton "0.1.0-SNAPSHOT"
    :description "Proton Clojure Wrapper"
    :url "http://example.com/FIXME"
    :license {
            :name "Apache License, Version 2.0"
            :url "http://www.apache.org/licenses/LICENSE-2.0.html"
    }
    :javac-options ["-source" "1.6" "-target" "1.6" "-g"]
    :java-source-paths ["src/java"]        

    :dependencies [[org.clojure/clojure "1.5.1"]
                    [xom/xom "1.2.5"]
                    [org.clojure/tools.trace "0.7.5"]
                    [clojure-debug-build "1.3.0-SNAPSHOT"]])
