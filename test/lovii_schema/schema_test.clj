(ns lovii-schema.schema-test
  (:require [clojure.test :refer :all]
            [lovii-schema.core :refer :all]
            [lovii-schema.schema :as ls]
            [datomic.api :as d]
            [schema.core :as s]
            [schema.coerce :as coerce]
            [lovii-schema.validation :as v]
            [lovii-schema.data :as ldata]
            [cheshire.core :as json]
            [com.rpl.specter :as sp])
  (:import [java.util UUID])
  (:use [com.rpl.specter.macros]))

(def example
  {:users {:schema/variant {:variant :users}
           :uuid {:type :uuid :unique :identity :required true}
           :name {:type :string :label "Full Name"}
           :tags {:type :ref :variants [:tags/set] :cardinality :one}
           :social {:type :ref :variants [:social] :cardinality :has-many}}

   :social {:schema/variant {:variant :social :required [#{:social/all-nothing-1 :social/all-nothing-2}]}
            :uuid {:type :uuid :unique :identity :required true}
            :all-nothing-1 {:type :string}
            :all-nothing-2 {:type :string}
            :service {:type :enum :values {:twitter "Twitter"
                                           :google  "Google"
                                           :facebook "Facebook"
                                           :linkedin "LinkedIn"
                                           :icq "ICQ"
                                           :myspace "MySpace"}
                      :label "Social Service"}
            :account {:type :string :index true :label "Social Account"}}

   :tags [{:schema/abstract {:abstract :tags :required [[:uuid]]}
           :uuid {:type :uuid :unique :identity
                  ;; set above as part of abstract requireds
                  ; :required true
                  }
           :service {:type :enum :values :social/service}
           :one-or-more1 {:type :string}
           :one-or-more2 {:type :string}
           :label {:type :string :required false
                   :min-length 2
                   :max-length 60
                   :regex ".*3.*"
                   :index true
                   :label "Label"}
           :inactive {:type :boolean :required false :index true :default false}}

          ;; Test the one or more required with :tag instead of setting to :required true
          {:schema/variant {:variant :tags/set :required [[:one-or-more1 :one-or-more2]]}
           :tags {:type :ref :required false
                  :cardinality :has-many
                  :variants [:tags/leaf]}}
          {:schema/variant {:variant :tags/leaf}
           :tag {:type :string :required true :index true}}]})

(def variant-pre
 {
  :users1 {:schema/variant :users1}  ; should be expanded
  :users2 {:schema/variant {:variant :users2}}
  :users3 [{:schema/variant :users3}]  ; should be expanded
  :users4 [{:schema/variant {:variant :users4}} ]
  })

(def variant-after
  {:users1 {:schema/variant {:variant :users1}}
   :users2 {:schema/variant {:variant :users2}}
   :users3 [{:schema/variant {:variant :users3}}]
   :users4 [{:schema/variant {:variant :users4}}]})

(deftest expand-schema-variant-test
  (testing "expand"  
    (is (= variant-after
           (ls/expand-schema-variant2 variant-pre))))
  (testing "parse"  
    (let [p1 (ls/parse-schema  variant-pre)
          p2 (ls/parse-schema2 variant-pre)]
          (is (map? p1))
          (is (= p1 p2))
          (is (= p1 variant-after)))))

(def abstract-pre
 {:users1 {:schema/abstract :users1}    ; should be expanded
  :users2 {:schema/abstract {:abstract :users2}}
  :users3 [{:schema/abstract :users3}]  ; should be expanded
  :users4 [{:schema/abstract {:abstract :users4}}]})

(def abstract-after
 {:users1 {:schema/abstract {:abstract :users1}}
  :users2 {:schema/abstract {:abstract :users2}}
  :users3 [{:schema/abstract {:abstract :users3}}]
  :users4 [{:schema/abstract {:abstract :users4}}]})

(deftest expand-schema-abstract-test
  (testing "expand"  
    (is (= abstract-after
           (ls/expand-schema-abstract2 abstract-pre))))
  (testing "parse"
    (let [p1 (ls/parse-schema  abstract-pre)
          p2 (ls/parse-schema2 abstract-pre)]
          (is (map? p1))
          (is (= p1 p2))
          (is (= p1 abstract-after)))))

(comment
  (ls/parse-schema  variant-pre)
  (ls/parse-schema2 variant-pre)
  (ls/parse-schema  example)
  (ls/parse-schema2 example))
