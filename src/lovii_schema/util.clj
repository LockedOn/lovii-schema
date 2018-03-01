(ns lovii-schema.util
  (:require [clojure.set]))

(defn concat-eager
  [& colls]
  (seq (reduce into [] colls)))

(defn- all-variants
  [schema]
  (reduce (fn [res m]
            (conj res (-> m :schema/variant :variant)))
          [] schema))

(defn assert-schemas-compatible [s1 s2]
  (let [compare-keys [:type :index :cardinality]]
    (let [s1cmp (select-keys s1 compare-keys)
          s2cmp (select-keys s2 compare-keys)]
      (when (and s1 s2 (not= s1cmp s2cmp))
        (throw (ex-info "Variant schemas differ in an incompatible way"
                        {:schema1 s1cmp
                         :schema2 s2cmp}))))))

(defn combine [vs1 vs2]
  (assert (or (and (sequential? vs1) (sequential? vs2))
              (and (map? vs1) (map? vs2))))
  (into vs1 vs2))

(defn merge-schemas [m1 m2]
  (assert-schemas-compatible m1 m2)
  (cond-> (merge m1 m2)
    (or (= :enum (:type m1))
        (= :enum (:enum m2)))
    (update :values combine (:values m1))))

(defn enum-values-also-variants [flat-schema]
  (let [values-enums (->> (dissoc flat-schema :schema/variant)
                          (keep (comp :values val))
                          (reduce into [])
                          (map first)
                          (set))
        variants-enums (set (keys (:values (:schema/variant flat-schema))))]
    (clojure.set/intersection values-enums variants-enums)))

(defn variants-also-attributes [flat-schema]
  (clojure.set/intersection (set (keys (:values (:schema/variant flat-schema))))
                            (set (keys flat-schema))))

(defn flatten-schema-unmemoized [schema]
  (let [flat-schema (->> schema
                         (reduce (fn [res m]
                                   (-> m
                                       (dissoc :schema/variant)
                                       (dissoc :schema/abstract)
                                       (->> (merge-with merge-schemas res))))
                                 {})
                         (merge {:schema/variant {:type :enum
                                                  :cardinality :one
                                                  :values (reduce (fn [res ke]
                                                                    (assoc res ke (str ke)))
                                                                  {}
                                                                  (all-variants schema))}}))
        enums-variants (enum-values-also-variants flat-schema)
        variants-also-attributes (variants-also-attributes flat-schema)]
    (when-not (empty? enums-variants)
      (throw (ex-info "Detected schema enums that have the same name as schema variants" {:intersection enums-variants})))
    (when-not (empty? variants-also-attributes)
      (throw (ex-info "Detected schema variants that have the same name as schema attributes" {:intersection variants-also-attributes})))
    flat-schema))

(def flatten-schema
  (memoize flatten-schema-unmemoized))

(defn get-abstract
  [variant]
  (when (keyword? variant)
    (keyword (or (namespace variant)
                 (name variant)))))

(defn ref?
  [kw]
  (boolean
   (and (keyword? kw)
        (namespace kw))))

(defn back-ref?
  [kw]
  (and
   (ref? kw)
   (-> kw
       name
       (subs 0 1)
       (= "_"))))

(defn back-ref
  [kw]
  (cond (back-ref? kw)
        kw

        (ref? kw)
        (keyword
         (namespace kw)
         (str "_" (name kw)))))

(defn forward-ref?
  [kw]
  (and (ref? kw)
       (not (back-ref? kw))))

(defn forward-ref
  [kw]
  (cond (forward-ref? kw)
        kw

        (back-ref? kw)
        (keyword
         (namespace kw)
         (subs (name kw) 1))))
