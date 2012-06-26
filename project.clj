(defproject jlk/math "0.1-SNAPSHOT"
  :description "wrappers around apache commons math"
  :url "https://github.com/taprisiot/jlk.repl"
  :license {:name "BSD"}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.apache.commons/commons-math3 "3.0"]
                 [org.clojure/data.finger-tree "0.0.1"]
                 [jlk/utility "0.1-SNAPSHOT"]
                 [jlk/log "0.2-SNAPSHOT"]
                 [org.clojure/core.match "0.2.0-alpha9"]]
  :plugins [[lein-swank "1.4.0"]])
