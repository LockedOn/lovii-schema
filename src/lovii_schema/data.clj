(ns lovii-schema.data
	(:require [lovii-schema.util :refer [flatten-schema]]))

(declare clean-data)

(defn- clean-value 
	[flat-schema attr value]
	(let [descriptor (get flat-schema attr)
		  type (:type descriptor)]
		(cond (and (= (:cardinality descriptor :has-many)) 
				   (vector? value))
			  (mapv #(clean-value flat-schema attr %) value)

			  (and (string? value) 
				   (= type :uuid))
			  (java.util.UUID/fromString value)

			  (and (keyword? value)
			  	   (= type :enum))
			  value

			  (and (string? value)
			  	   (= type :enum))
			  (keyword value)

			  (contains? #{:string :boolean :long :bigint :double :bigdec :float :uuid :keyword} type)
			  value

			  (and (map? value) 
			  	   (= type :ref))
			  (clean-data flat-schema value))))

(defn clean-data
	[schema data]
	(let [flat-schema (flatten-schema schema)]
		(reduce (fn [res [attr value]] 
					(assoc res attr (clean-value flat-schema attr value)))
				{} 
				data)))