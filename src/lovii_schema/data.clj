(ns lovii-schema.data
  (:require [lovii-schema.util :refer [flatten-schema back-ref? forward-ref]]
            [clojure.edn]))

(defn parse-inst 
  [attr value]
  (let [v (when (string? value)
            (clojure.edn/read-string (str "#inst " (pr-str value))))]
    (if (inst? v)
       v 
       (throw (ex-info (str "Not a valid inst: " attr " - " value)
                      {:attr attr
                       :value value})))))

(declare clean-data-flat)

(defn- clean-value
  [flat-schema attr value]
  (let [descriptor (get flat-schema attr)
        t (:type descriptor)]
    (cond 

      ;; allow backref's
      (and (back-ref? attr)
           (-> (get flat-schema (forward-ref attr)) :type (= :ref)))
      (let [[v1 v2] (cond (vector? value)
                          value

                          (map? value)
                          (-> value seq first))]
        (-> (clean-data-flat flat-schema {(keyword v1) v2}) seq first))

      (and (= (:cardinality descriptor :has-many))
           (vector? value))
      (mapv #(clean-value flat-schema attr %) value)

      (and (map? value)
           (= t :ref))
      (clean-data-flat flat-schema value)

      (and (string? value)
           (= t :uuid))
      (java.util.UUID/fromString value)

      (and (string? value)
           (= t :date))
      (parse-inst attr value)

      (and (string? value)
           (= t :date-time))
      (parse-inst attr value)

      (and (= (type value) java.util.Date)
           (#{:date :date-time} t))
      value

      (and (keyword? value)
           (= t :enum))
      value

      (and (string? value)
           (= t :enum))
      (keyword value)

      (and (= t :double)
           (-> value type (not= java.lang.Double)))
      (-> value str Double/parseDouble)

      (and (= t :float)
           (-> value type (not= java.lang.Float)))
      (-> value str Float/parseFloat)

      (contains? #{:string :string-large :edn :boolean :long :int :bigint :double :decimal :bigdec :float :uuid :keyword} t)
      value

      ;; nil value to be turned into retractions
      (nil? value)
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
            (let [value (clean-value flat-schema attr value)]
              (assoc res attr value)))
          {}
          data))

(defn clean-data
  [schema data]
  (clean-data-flat (flatten-schema schema) data))
