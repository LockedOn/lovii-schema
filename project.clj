(defproject lovii-schema "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :profiles {:dev {:dependencies [[org.clojure/clojure "1.8.0"]
  								  [cheshire "5.5.0"]
				  				  [com.datomic/datomic-free "0.9.5201" :exclusions [joda-time]]]
  				   :plugins [[com.jakemccrary/lein-test-refresh "0.10.0"]]
				   :main lovii-schema.core}})
