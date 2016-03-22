(ns lovii-schema.util
  (:require [aero.core :refer [reader read-config]]
            [clojure.java.io :as io]
            [clojure.walk :refer [postwalk]]))

(defn- all-variants
  [schema]
  (reduce (fn [res m]
            (conj res (:schema/variant m)))
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


;;;
;;; Here's how to extend the reader

(defmethod reader 'path
  [_ tag value]
  (with-meta value {:keypath true}))


(let [config (read-config (io/resource "config.edn"))]
  (postwalk (fn [z] (if (:keypath (meta z))
                      (get-in config z)
                      z))
            config))
