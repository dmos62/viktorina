(defproject viktorina "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [
                 [org.clojure/clojure "1.8.0"]
                 ;[org.clojure/clojure "1.9.0-alpha14"]
                 [hiccup "1.0.5"]
                 [markdown-clj "0.9.90"]
                 ]
  :main ^:skip-aot viktorina.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
