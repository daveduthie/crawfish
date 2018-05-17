(defproject crawfish "0.1.0-SNAPSHOT"
  :description "A simple web crawler"
  :url "http://github.com/daveduthie/crawfish"
  :license {:name "Eclipse Public License"
            :url  "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[enlive "1.1.6"]
                 [http-kit "2.2.0"]
                 [org.clojure/clojure "1.9.0"]
                 [org.clojure/core.async "0.4.474"]
                 [org.clojure/tools.cli "0.3.7"]]
  :main ^:skip-aot crawfish.core
  :plugins [[lein-marginalia "0.9.1"]]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}
             :dev     {:jvm-opts ["-Dclojure.server.repl={:port 5555 :accept clojure.core.server/repl}"]}})
