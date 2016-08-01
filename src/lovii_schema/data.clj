(ns lovii-schema.data
  (:require [lovii-schema.util :refer [flatten-schema]])
  (:import [java.text SimpleDateFormat]
           [java.time Instant]
           [java.util Date]))

(declare clean-data-flat)

(defn- clean-value
  [flat-schema attr value]
  (let [descriptor (get flat-schema attr)
        t (:type descriptor)]
    (cond (and (= (:cardinality descriptor :has-many))
               (vector? value))
          (mapv #(clean-value flat-schema attr %) value)

          (and (map? value)
               (= t :ref))
          (clean-data-flat flat-schema value)

          (and (string? value)
               (= t :uuid))
          (java.util.UUID/fromString value)

          ;; FIXME: hacky coercion method for dates
          (and (string? value)
               (= t :date))
          (.parse
           (SimpleDateFormat. "yyyy-mm-dd")
           value)

          ;; FIXME: hacky coercion method for dates
          (and (string? value)
               (= t :date-time))
          (try
           (Date/from (Instant/parse value))
           (catch Exception _
             (.parse
              (SimpleDateFormat. "yyyy-mm-dd")
              value)))

          (and (= (type value) java.util.Date)
               (#{:date :date-time} t))
          value

          (and (keyword? value)
               (= t :enum))
          value

          (and (string? value)
               (= t :enum))
          (keyword value)

          (contains? #{:string :string-large :edn :boolean :long :int :bigint :double :decimal :bigdec :float :uuid :keyword} t)
          value

          ;; allow backref's
          (and (nil? descriptor)
               (map? value))
          (clean-data-flat flat-schema value)

          (nil? descriptor)
          (throw (ex-info "Attribute not present in schema" 
                          {:attr attr}))

          :else
          (throw (ex-info "Unhandled clean value case" 
                          {:descriptor descriptor
                           :attr attr 
                           :value value})))))

(defn clean-data-flat
  [flat-schema data]
  (reduce (fn [res [attr value]]
            (if-let [value (clean-value flat-schema attr value)]
	      (assoc res attr value)
	      res))
	  {}
	  data))

(defn clean-data
  [schema data]
  (clean-data-flat (flatten-schema schema) data))
