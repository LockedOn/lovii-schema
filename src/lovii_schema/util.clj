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
