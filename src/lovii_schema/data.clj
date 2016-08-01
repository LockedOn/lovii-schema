(ns lovii-schema.data
  (:require [lovii-schema.util :refer [flatten-schema back-ref? forward-ref]])
  (:import [java.text SimpleDateFormat]
           [java.time Instant]
           [java.util Date]))

(declare clean-data-flat)

(defn- clean-value
  [flat-schema attr value]
  (let [descriptor (get flat-schema attr)
        t (:type descriptor)]
    (cond ;; allow backref's
      (and (back-ref? attr)
           (-> (get flat-schema (forward-ref attr)) :type (= :ref)))
      (do (prn (-> (clean-data-flat flat-schema {(keyword (first value)) (second value)}) seq first)) 
          (-> (clean-data-flat flat-schema {(keyword (first value)) (second value)}) seq first))

      (and (= (:cardinality descriptor :has-many))
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

      (nil? descriptor)
      (throw (ex-info (str "Attribute not present in schema: " attr)
                      {:attr attr}))

      :else
      (throw (ex-info (str "Unhandled clean value case: " attr)
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
