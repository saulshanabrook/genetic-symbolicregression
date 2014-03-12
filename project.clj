(defproject genetic-symbolicregression "0.1.0-SNAPSHOT"
  :description "Data fitting using symblic regression through GP"
  :url "https://github.com/saulshanabrook/genetic-symbolicregression"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [genetic "0.1.0"]
                 [roul "0.2.0"]]
  :main ^:skip-aot genetic.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}
             :test {:plugins [[jonase/eastwood "0.1.0"]]}})
