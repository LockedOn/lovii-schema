(ns lovii-schema.datomic
  (:require [lovii-schema.util :refer [flatten-schema concat-eager]]))

(defn- datomic-base
  [tempid m install-alter-options]
  (let [install-key (cond
                      (and (:db.alter m)
                           (= install-alter-options :alter))
                      :db.alter/_attribute

                      :else
                      :db.install/_attribute)]
    {:db/id (tempid :db.part/db)
     install-key :db.part/db}))

(defn- datomic-enum-values
  [flat-schema tempid]
  (->> flat-schema
       (reduce (fn [res [k v]]
                 (if (and (= :enum (:type v))
                          (map? (:values v)))
                   (concat-eager res (keys (:values v)))
                   res))
               [])
       (mapv (fn [k]
               {:db/id (tempid :db.part/user)
                :db/ident k}))))

(defn- schema->datomic-types
  [v type-map]
  (let [t (get v :type nil)]
    (if (contains? #{:keyword :string :boolean :long :bigint :float :double :bigdec :ref :instant :uuid :uri :bytes} t)
      v
      (assoc v :type (get (merge {:date :instant
                                  :date-time :instant
                                  :enum :ref}
                                 type-map)
                          t)))))

(defn- datomic-fulltext
  [d m]
  (assoc d :db/fulltext (if (true? (get m :fulltext)) true false)))

(defn- datomic-index
  [d m]
  (assoc d :db/index (if (true? (get m :index)) true false)))

(defn- datomic-isComponent
  [d m]
  (if (= :ref (get m :type))
    (assoc d :db/isComponent (if (true? (get m :isComponent)) true false))
    d))

(defn- datomic-noHistory
  [d m]
  (assoc d :db/noHistory (if (true? (get m :noHistory)) true false)))

(defn- datomic-doc
  [d m]
  (assoc d :db/doc (str (get m :comment ""))))

(defn- datomic-unique
  [d m]
  (if-let [unique (get {:value :db.unique/value
                        :identity :db.unique/identity}
                       (get m :unique))]
    (assoc d :db/unique unique)
    d))

(defn- datomic-cadinality
  [d m]
  (->> (get {:one :db.cardinality/one
             :has-many :db.cardinality/many}
            (get m :cardinality))
       (assoc d :db/cardinality)))

(defn- datomic-ident
  [d m]
  (->> (:attribute m)
       (assoc d :db/ident)))

(defn- datomic-type
  [d m]
  (->> (get {:keyword :db.type/keyword
             :string :db.type/string
             :boolean :db.type/boolean
             :long :db.type/long
             :bigint :db.type/bigint
             :float :db.type/float
             :double :db.type/double
             :bigdec :db.type/bigdec
             :ref :db.type/ref
             :instant :db.type/instant
             :uuid :db.type/uuid
             :uri :db.type/uri
             :bytes :db.type/bytes}
            (get m :type))
       (assoc d :db/valueType)))

(defn schema
  [raw-schema tempid type-map install-alter-options]
  (let [flat-schema (flatten-schema raw-schema)
        db (map (fn [[k v]]
                  (let [m (schema->datomic-types (assoc v :attribute k) type-map)]
                    (-> (datomic-base tempid m install-alter-options)
                        (datomic-doc m)
                        (datomic-type m)
                        (datomic-index m)
                        (datomic-fulltext m)
                        (datomic-isComponent m)
                        (datomic-noHistory m)
                        (datomic-unique m)
                        (datomic-cadinality m)
                        (datomic-ident m)))) flat-schema)
        idents (datomic-enum-values flat-schema tempid)]
    (vec (concat-eager db idents))))

(declare data->datoms-flat)

(defn- datom-values
  [flat-schema tempid attr value parent-id type-map]
  (let [descriptor (get flat-schema attr)
        type (:type descriptor)
        d-type (:type (schema->datomic-types descriptor type-map))]
    (cond (and (= (:cardinality descriptor :has-many))
               (vector? value))
          (->> value
               (mapv #(datom-values flat-schema tempid attr % parent-id type-map))
               (reduce (fn [[res parts] [v* more-parts]]
                         [(if (some? v*)
                            (vec (concat-eager res [v*]))
                            res)
                          (if more-parts
                            (concat-eager parts more-parts)
                            parts)])
                       [nil []]))

          (and (map? value)
               (= type :ref))
          [nil (data->datoms-flat flat-schema tempid value {(keyword (namespace attr) (str "_" (name attr))) parent-id} type-map)]

          :else
          [value []])))

(defn data->datoms-flat
  [flat-schema tempid data base type-map]
  (if (vector? data)
    (mapv #(data->datoms-flat flat-schema tempid % base type-map) data)
    (let [parent-id (tempid :db.part/user)
          [cleaned parts] (reduce (fn [[res m-parts] [a v]]
                                    (let [[v* more-parts] (datom-values flat-schema tempid a v parent-id type-map)]
                                      [(if (some? v*)
                                         (assoc res a v*)
                                         res)
                                       (if more-parts
                                         (concat-eager m-parts more-parts)
                                         m-parts)]))
                                  [{} []] data)]
      (-> base
          (assoc :db/id parent-id)
          (merge cleaned)
          (vector)
          (concat-eager parts)
          (vec)))))

(defn data->datoms
  ([schema tempid data]
   (data->datoms schema tempid data {}))

  ([schema tempid data type-map]
   (data->datoms-flat (flatten-schema schema) tempid data {} type-map)))
