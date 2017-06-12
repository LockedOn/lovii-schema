(defproject lockedon/lovii-schema "0.3.7"
  :description "Describe your application schema using data."
  :url "https://github.com/LockedOn/lovii-schema"
  :license {:name "MIT"
            :url "https://opensource.org/licenses/MIT"}
  :profiles {:dev {:dependencies [[org.clojure/clojure "1.9.0-alpha13"]
                                  [cheshire "5.5.0"]
                                  [prismatic/schema "1.1.3"]
                                  [clj-time "0.13.0"]
                                  [com.datomic/datomic-free "0.9.5201" :exclusions [joda-time]]]
                   :plugins [[com.jakemccrary/lein-test-refresh "0.10.0"]]
                   :main lovii-schema.core}}
  :plugins [[lein-cljfmt "0.5.1"]]
  :deploy-repositories [["releases" :clojars]])
