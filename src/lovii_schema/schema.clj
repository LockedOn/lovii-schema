(ns lovii-schema.schema)

(defn- add-namespace
  [k n]
  (if-let [n (namespace k)]
    k
    (keyword (name n) (name k))))

(defn- add-namespace-map
  [m n]
  (reduce (fn [res [k v]]
            (assoc res (add-namespace k n) v))
          {} m))

(defn- expand-attribute-enums
  [n]
  (fn [v]
    (if (and (= :enum (:type v))
             (map? (:values v)))
      (->> (add-namespace-map (:values v) n)
           (assoc v :values))
      v)))

(defn- expand-default-values
  [n]
  (fn [v]
    (if (keyword? (:default-value v))
      (->> (add-namespace (:default-value v) n)
           (assoc v :default-value))
      v)))

(defn- add-namespace-required
  [v n]
  (cond (vector? v)
        (mapv #(add-namespace-required % n) v)

        (set? v)
        (reduce (fn [res v*]
                  (conj res (add-namespace-required v* n)))
                #{} v)

        (keyword? v)
        (add-namespace v n)

        :else
        v))

(defn- expand-attribute-required
  [n]
  (fn [v]
    (if (:required v)
      (assoc v :required (add-namespace-required (:required v) n))
      v)))

(defn- apply-fns
  [m fns]
  (reduce (fn [m* f]
            (reduce (fn [res [k v]]
                      (if (:type v)
                        (assoc res k (f v))
                        (assoc res k v)))
                    {} m*))
          m fns))

(defn- cardinality-one
  [v]
  (assoc v :cardinality (get v :cardinality :one)))

(defn- expand-variant-keys
  [lo-schema fns]
  (fn [m]
    (when-some [n (-> m :schema/abstract :abstract)]
      (some-> m
              (add-namespace-map n)
              (apply-fns [(expand-default-values n) (expand-attribute-enums n) (expand-attribute-required n)])
              (apply-fns [cardinality-one])
              (apply-fns fns)))))

(defn- expand-schema-variant
	[m]
	(let [variant (-> m :schema/variant)
		  abstract (-> m :schema/abstract)
		  variant-map (if (keyword? variant)
		  				  {:schema/variant {:variant variant}}
		  				  {})
		  abstract-map (if (keyword? abstract)
		  					{:schema/abstract {:abstract abstract}}
		  					{})]
		  (merge m variant-map abstract-map)))

(defn parse-schema
  [lo-schema & fns]
  (->> lo-schema
  	(map (fn [[k v]]
  			[k (if (vector? v)
  				  (mapv expand-schema-variant v)
  				  (expand-schema-variant v))]))
  	(reduce (fn [res [_ sch]]
                (when res
                  (cond (vector? sch)
                        (when-some [abstract (first (filter #(-> % :schema/abstract :abstract) sch))]
                          (some->> (filter #(-> % :schema/variant :variant) sch)
                                   (map #(merge abstract %))
                                   (map (expand-variant-keys lo-schema fns))
                                   (concat res)))

                        (map? sch)
                        (let [variant (-> sch :schema/variant :variant)]
	                        (some->> (merge sch {:schema/abstract {:abstract variant}})
	                                 ((expand-variant-keys lo-schema fns))
	                                 (conj res))))))
              [])
      (vec)))
