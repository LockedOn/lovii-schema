(ns lovii-schema.stats
  (:require [lovii-schema.util :refer :all]))

(defn schema-stats
  ([parsed-schema]
   (if (-> parsed-schema meta :schema-stats)
     parsed-schema
     (schema-stats parsed-schema (flatten-schema parsed-schema))))
  ([parsed-schema flat-schema]
   (let [variant-keys
         (-> flat-schema
             (get-in [:schema/variant :values])
             (keys)
             (set))

         abstract-attribs
         (group-by get-abstract (keys flat-schema))

         abstract-variants
         (group-by get-abstract variant-keys)

         abstract-keys
         (set (keys abstract-variants))

         abstract-and-variants
         (clojure.set/union variant-keys abstract-keys)

         by-variant
         (->> parsed-schema
              (group-by #(-> % :schema/variant :variant))
              (reduce
               (fn [a [k [v]]]
                 (assoc a k v))
               {}))

         by-abstract
         (->> parsed-schema
              (group-by #(-> % :schema/abstract :abstract))
              (reduce
               (fn [a [k [v]]]
                 (assoc a k v))
               {}))

         propagates-to-by-variant
         (->> parsed-schema
              (mapcat #(->> %
                            (filter (fn [[k v]]
                                      (:permissions/propagate v)))
                            (mapcat (fn [[k v]]
                                      (->> v
                                           :variants
                                           (map (fn [vv] [vv k])))))))
              (reduce (fn [a [b c]]
                        (if (-> c back-ref? not)
                          (assoc a b (conj (set (get a b)) (back-ref c)))
                          a)) {}))]
     ^:schema-stats {:parsed-schema parsed-schema
                     :flat-schema flat-schema
                     :keys {:abstracts abstract-keys
                            :variants variant-keys
                            :both abstract-and-variants}
                     :variants-by-abstract abstract-variants
                     :attributes-by-abstract abstract-attribs
                     :schema-by-variant by-variant
                     :propagation-by-variant propagates-to-by-variant
                     :schema-by-abstract by-abstract})))