(defproject crawfish "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[enlive "1.1.6"]
                 [http-kit "2.2.0"]
                 [org.clojure/clojure "1.9.0"]
                 [org.clojure/core.async "0.4.474"]]
  :main ^:skip-aot crawfish.core
  :jvm-opts ["-Dclojure.server.repl={:port 5555 :accept clojure.core.server/repl}"]
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
