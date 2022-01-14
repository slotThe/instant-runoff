(defproject instant-runoff "0.1.0-SNAPSHOT"
  :description "Instant runoff implementation, for the purpose of
                evaluating the XMonad logo contest."
  :url "https://github.com/slotThe/instant-runoff"
  :license {:name "AGPL-3.0"
            :url "https://www.gnu.org/licenses/agpl-3.0.en.html"}
  :dependencies [[org.clojure/clojure "1.10.1"]]
  :main ^:skip-aot instant-runoff.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
