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

(defn- all-variants
	[schema]
	(reduce (fn [res m]
				(conj res (:schema/variant m)))
			[] schema))

(defn flatten-schema
	[schema]
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
								 				(all-variants schema))}})))

(defn- cardinality-one
	[v]
	(assoc v :cardinality (get v :cardinality :one)))

(defn- expand-variant-keys
	[lo-schema fns]
	(fn [m] 
		(when-some [n (:schema/abstract m)]
				(some-> m 
						(add-namespace-map n)
						(apply-fns [(expand-attribute-enums n) (expand-attribute-required n)])
						(apply-fns [cardinality-one])
						(apply-fns fns)))))

(defn parse-schema
	[lo-schema & fns]
	(-> (reduce (fn [res [_ sch]]
					(when res 
						(cond (vector? sch)
							  (when-some [abstract (first (filter #(and (:schema/abstract %)) sch))]
							  	(some->> (filter #(and (:schema/variant %)) sch)
							  			 (map #(merge abstract %))
							  			 (map (expand-variant-keys lo-schema fns))
									  	 (concat res)))

							  (map? sch)
							  (some->> (merge sch {:schema/abstract (:schema/variant sch)})
							  	   ((expand-variant-keys lo-schema fns))
							  	   (conj res)))))
			[] lo-schema)
		(vec)))
