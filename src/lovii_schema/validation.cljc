(ns lovii-schema.validation
  (:require [lovii-schema.core :refer :all]
            [lovii-schema.schema :as loschema]
            [schema.coerce]
            [schema.core :as s]))

(def Edn
  (s/pred #(try (clojure.edn/read-string %)
                true
                (catch Throwable t
                  false))))

(defn type-schema [{:keys [type values regex] :as s}]
  (case type
    :enum (apply s/enum (keys values))
    :double (s/pred float?)
    :decimal (s/pred float?)
    :edn Edn
    :ref s/Any
    :uuid s/Uuid
    :string (cond regex 
                  (s/both s/Str (re-pattern regex))
                  :else
                  s/Str)
    :string-large s/Str
    :boolean s/Bool
    :int s/Int
    :long s/Int
    :integer s/Int
    :date-time s/Inst
    ; Is datetime an acceptable validation for date?
    :date s/Inst
    :instant s/Inst
    :keyword s/Keyword
    (throw (Exception. (format "Could not find validator for type %s." type))))) 

(defn select-abstract-keys [schema]
  (->> schema 
       (filter (fn [[k v]] 
                 (= (name (:abstract (:schema/abstract schema))) (namespace k))))
       (into {})))

(defn constrain-length [{:keys [min-length max-length]} current-schema]
  (cond-> current-schema
    min-length (s/constrained (fn [v] (>= (count v) min-length)) (format "min length of %s" min-length))
    max-length (s/constrained (fn [v] (<= (count v) max-length)) (format "max length of %s" min-length))))

(defn build-all-or-none-keys [schema ks]
  (s/constrained 
    schema 
    (fn [v]
      (let [values (map v ks)] 
        (or (every? nil? values)
            (not (some nil? values)))))
    [:error :all-or-none ks]))

(defn build-one-or-more-keys [schema ks]
  (s/constrained 
    schema 
    (fn [v]
      (let [values (map v ks)] 
        (some (complement nil?) values)))
    [:error :one-or-more ks]))

(defn apply-cardinality [v validation]
  (case (:cardinality v)
    :one validation
    :has-many [validation]))

(defn create-kv-validator [validation-var [k v]]
  (let [key-schema (if (true? (:required v))
                     k
                     (s/optional-key k))
        value-schema (if (= :ref (:type v))
                       (let [variant-set (set (:variants v))] 
                         (s/conditional #(get variant-set 
                                              (:schema/variant %)) 
                                        (s/recursive validation-var)))
                       (constrain-length v (type-schema v)))]
    [key-schema (apply-cardinality v value-schema)]))

(def DbId s/Any)

(defn build-validator 
  [validation-var variant-schema]
  (let [variant (:variant (:schema/variant variant-schema))] 
    (into {(s/optional-key :db/id) DbId
           :schema/variant (s/eq variant)}
          (map #(create-kv-validator validation-var %) 
               (select-abstract-keys variant-schema)))))

(defn schemas->validators 
  [validation-var parsed-schema]
  (let [keyed-schemas (into {} 
                            (map (fn [s] [(:variant (:schema/variant s)) s]) 
                                 parsed-schema))
        schemas (map (fn [[k v]]
                       [k (build-validator validation-var v)])
                     keyed-schemas)
        full-schema (apply s/conditional 
                           (mapcat (fn [[variant schema]]
                                     [#(= variant (:schema/variant %))
                                      schema]) 
                                   schemas))]
    (apply s/conditional 
           (mapcat (fn [[variant schema]]
                     [#(= variant (:schema/variant %))
                      schema]) 
                   schemas))))


(defmacro defrecursive-schema 
  [symbol-name parsed-schemas]
  `(let [vr# (def ~symbol-name nil)]
     (alter-var-root vr# (constantly (v/schemas->validators vr# ~parsed-schemas)))
     vr#))
