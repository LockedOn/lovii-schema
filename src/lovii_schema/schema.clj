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

(defn- add-namespace-required
  [m n]
  (update-in m
             [:schema/variant :required]
             (fn [requireds]
               (map (fn [r]
                      (into (empty r)
                            (map (fn [v]
                                   (keyword (name n) (name v)))
                                 r)))
                    requireds))))

(defn expand-abstract-requireds [m n]
  (assoc-in m
            [:schema/variant :required]
            (vec (concat (:required (:schema/variant m))
                         (:required (:schema/abstract m))))))

(defn- expand-attribute-enums
  [n]
  (fn [v]
    (cond (and (= :enum (:type v))
               (map? (:values v)))
          (->> (add-namespace-map (:values v) n)
               (assoc v :values))

          (and (= :enum (:type v))
               (keyword? (:values v)))
          (assoc v :values (with-meta {} {:enum-values (:values v)}))

          :else
          v)))

(defn- expand-default-values
  [n]
  (fn [v]
    (if (keyword? (:default-value v))
      (->> (add-namespace (:default-value v) n)
           (assoc v :default-value))
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
              (expand-abstract-requireds n)
              (add-namespace-required n)
              (apply-fns [(expand-default-values n) (expand-attribute-enums n)])
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

(defn expand-enums [parsed]
  (clojure.walk/postwalk
   (fn [v]
     (let [enum (-> v meta :enum-values)]
       (cond enum
             (->> parsed
                  (apply concat)
                  (filter (fn [[k v]]
                            (and (= k enum)
                                 (map? (:values v)))))
                  first
                  val
                  :values)

             :else
             v)))
   parsed))

(defn mk-abstract->variants [parsed-schemas]
  (->> parsed-schemas
       (map (fn [s]
              [(:abstract (:schema/abstract s))
               (:variant (:schema/variant s))]))
       (group-by first)
       (map (fn [[k vs]]
              [k (mapv second vs)]))
       (into {})))

(defn expand-abstracts [parsed-schemas]
  (let [abstract->variants (mk-abstract->variants parsed-schemas)]
    (map (fn [s]
           (->> s
                (map (fn [[k v]]
                       (vector k
                               (if (= :ref (:type v))
                                 (update v :variants (fn [vs]
                                                       (vec (mapcat #(get abstract->variants % [%]) vs))))
                                 v))))
                (into {})))
         parsed-schemas)))

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
       expand-enums
       expand-abstracts
       (vec)))
