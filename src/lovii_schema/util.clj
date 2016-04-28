(ns lovii-schema.util)

(defn- all-variants
  [schema]
  (reduce (fn [res m]
            (conj res (-> m :schema/variant :variant)))
          [] schema))

(def flatten-schema
  (memoize
   (fn [schema]
     (->> schema
          (reduce (fn [res m]
                    (-> m
                        (dissoc :schema/variant)
                        (dissoc :schema/abstract)
                        (->> (merge res))))
                  {})
          (merge {:schema/variant {:type :enum
                                   :cardinality :one
                                   :values (reduce (fn [res ke]
                                                     (assoc res ke (str ke)))
                                                   {}
                                                   (all-variants schema))}})))))
