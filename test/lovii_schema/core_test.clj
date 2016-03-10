(ns lovii-schema.core-test
  (:require [clojure.test :refer :all]
            [lovii-schema.core :refer :all]
			[cheshire.core :as json]
			[datomic.api :as d]
			[lovii-schema.datomic :as lodatomic]
			[lovii-schema.util :as util]
			[lovii-schema.data :as lodata]
			[lovii-schema.schema :as loschema]))

(defn create-connection
  "Create a connection to an anonymous, in-memory database."
  [schema]
  (let [uri (str "datomic:mem://" (d/squuid))]
    (d/delete-database uri)
    (d/create-database uri)
    (let [conn (d/connect uri)
    	  schema-data (lodatomic/schema schema d/tempid)]
    	{:conn conn
    	 :schema-data schema-data
    	 :tx @(d/transact conn schema-data)})))

(def enum-test-data
    {:enumns {:schema/variant :enumns 
              :option {:type :enum :values {:one "One" :two "Two" :staticns/three "Three"} :default-value :one}}})

(def test-schema 
	{:testns {:schema/variant :testns
		   	  :uuid {:type :uuid :unique :identity :required true}
			  :counter {:type :double :required true}
			  :second {:type :ref :variants [:secondns]}}
	 :secondns {:schema/variant :secondns
				:uuid {:type :uuid :unique :identity :required true}
				:counter {:type :double :required true :cardinality :has-many}}})

(def test-data 
	{:schema/variant :testns
	 :testns/uuid #uuid "297d5fb9-13b9-47f9-a4d6-30ce7cdcec19"
	 :testns/counter 5.0
	 :testns/second {:schema/variant :secondns
	 				 :secondns/uuid #uuid  "123e4567-e89b-12d3-a456-426655440800"
	 				 :secondns/counter [3.1 9.0]}})

(def lo-schema (loschema/parse-schema test-schema))

(defn countdown
	[]
	(let [a (atom 0)]
		(fn [part] 
			(d/tempid part (swap! a dec)))))

(deftest schema-test
  (testing "Parse Schema"
    (is (= lo-schema 
           [{:schema/variant :testns, 
             :testns/uuid {:type :uuid, :unique :identity, :required true, :cardinality :one}, 
             :testns/counter {:type :double, :required true, :cardinality :one}, 
             :testns/second {:type :ref, :cardinality :one :variants [:secondns]}, 
             :schema/abstract :testns}
            {:schema/variant :secondns, 
             :secondns/uuid {:type :uuid, :unique :identity, :required true, :cardinality :one}, 
             :secondns/counter {:type :double, :required true, :cardinality :has-many}, 
             :schema/abstract :secondns}])))
  (testing "Parse Expand Namespace"
    (is (= (loschema/parse-schema enum-test-data) 
    	   [{:schema/variant :enumns
             :schema/abstract :enumns 
             :enumns/option {:type :enum 
                             :values {:enumns/one "One" 
                                      :enumns/two "Two" 
                                      :staticns/three "Three"} 
                             :default-value :enumns/one 
                             :cardinality :one}}])))
  (testing "Datomic Schema"
    (is (= (lodatomic/schema lo-schema (countdown))
    	   [{:db/index false,
    	   	 :db/valueType :db.type/ref,
			 :db/noHistory false,
			 :db/isComponent false,
			 :db.install/_attribute :db.part/db,
			 :db/fulltext false,
			 :db/cardinality :db.cardinality/one,
			 :db/doc "",
			 :db/id #db/id[:db.part/db -3],
			 :db/ident :schema/variant}
    	    {:db/index false,
    	     :db/unique :db.unique/identity,
    	     :db/valueType :db.type/uuid,
    	     :db/noHistory false,
    	     :db.install/_attribute :db.part/db,
    	     :db/fulltext false,
    	     :db/cardinality :db.cardinality/one,
    	     :db/doc "",
    	     :db/id #db/id[:db.part/db -4],
    	     :db/ident :testns/uuid}
    	    {:db/id #db/id[:db.part/db -5],
    	     :db.install/_attribute :db.part/db,
    	     :db/doc "",
    	     :db/valueType :db.type/double,
    	     :db/index false,
    	     :db/fulltext false,
    	     :db/noHistory false,
    	     :db/cardinality :db.cardinality/one,
    	     :db/ident :testns/counter}
    	    {:db/index false, 
    	     :db/valueType :db.type/ref, 
    	     :db/noHistory false, 
    	     :db/isComponent false, 
    	     :db.install/_attribute :db.part/db, 
    	     :db/fulltext false, 
    	     :db/cardinality :db.cardinality/one, 
    	     :db/doc "", 
    	     :db/id #db/id[:db.part/db -6], 
    	     :db/ident :testns/second}
    	    {:db/index false,
    	     :db/unique :db.unique/identity,
    	     :db/valueType :db.type/uuid,
    	     :db/noHistory false,
    	     :db.install/_attribute :db.part/db,
    	     :db/fulltext false,
    	     :db/cardinality :db.cardinality/one,
    	     :db/doc "",
    	     :db/id #db/id[:db.part/db -7],
    	     :db/ident :secondns/uuid}
    	    {:db/id #db/id[:db.part/db -8],
    	     :db.install/_attribute :db.part/db,
    	     :db/doc "",
    	     :db/valueType :db.type/double,
    	     :db/index false,
    	     :db/fulltext false,
    	     :db/noHistory false,
    	     :db/cardinality :db.cardinality/many,
    	     :db/ident :secondns/counter}
    	    {:db/id #db/id[:db.part/user -1],
    	     :db/ident :testns}
    	    {:db/id #db/id[:db.part/user -2],
    	     :db/ident :secondns}])))
	(testing "Create Datomic DB"
	    (is (:conn (create-connection lo-schema))))
	(testing "Create Datomic DB"
	    (is (= (lodatomic/data->datoms lo-schema (countdown) test-data)
	    	   [{:db/id #db/id[:db.part/user -1],
	    	   	 :schema/variant :testns,
	    	     :testns/uuid #uuid "297d5fb9-13b9-47f9-a4d6-30ce7cdcec19",
	    	     :testns/counter 5.0}
	    	    {:testns/_second #db/id[:db.part/user -1],
	    	   	 :db/id #db/id[:db.part/user -2],
	    	     :schema/variant :secondns,
	    	     :secondns/uuid #uuid "123e4567-e89b-12d3-a456-426655440800",
	    	     :secondns/counter [3.1 9.0]}])))
	(testing "Transact Data"
	    (is @(d/transact (:conn (create-connection lo-schema)) (lodatomic/data->datoms lo-schema d/tempid test-data))))
	)
