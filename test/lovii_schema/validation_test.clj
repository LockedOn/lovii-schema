(ns lovii-schema.validation-test
  (:require [clojure.test :refer :all]
            [lovii-schema.core :refer :all]
            [lovii-schema.schema :as loschema]
            [datomic.api :as d]
            [schema.core :as s]
            [schema.coerce :as coerce]
            [lovii-schema.validation :as v]
            [lovii-schema.data :as ldata]
            [cheshire.core :as json])
  (:import [java.util UUID]))

(def example
  {:users {:schema/variant {:variant :users}
           :uuid {:type :uuid :unique :identity :required true}
           :name {:type :string :label "Full Name" }
           :tags {:type :ref :variants [:tags/set] :cardinality :one}
           :social {:type :ref :variants [:social] :cardinality :has-many}}

   :social {:schema/variant {:variant :social}
            :uuid {:type :uuid :unique :identity :required true}
            :service {:type :enum :values {:twitter "Twitter"
                                           :google  "Google"
                                           :facebook "Facebook" 
                                           :linkedin "LinkedIn"
                                           :icq "ICQ"
                                           :myspace "MySpace"}
                      :label "Social Service"}
            :account {:type :string :index true :label "Social Account"}}

   :tags [{:schema/abstract {:abstract :tags}
           :uuid {:type :uuid :unique :identity :required true}
           :label {:type :string :required false 
                   :min-length 2
                   :max-length 60 
                   :regex ".*3.*"
                   :index true 
                   :label "Label"}
           :inactive {:type :boolean :required false :index true :default false}}
          {:schema/variant {:variant :tags/set}
           :tags {:type :ref :required false 
                  :cardinality :has-many 
                  :variants [:tags/leaf]}}
          {:schema/variant {:variant :tags/leaf}
           :tag {:type :string :required true :index true}}]})

(v/defrecursive-schema validation-schema (loschema/parse-schema example))

(deftest validators-test
    (testing "Required keys"
      ;; Missing UUID
      (is (thrown? Throwable (s/validate validation-schema 
                                         {:schema/variant :tags/set
                                          ;:tags/uuid (java.util.UUID/randomUUID)
                                          :tags/label "hi th 99 39393"})))
      ;; Missing :tags/label, which should succeed since required is false
      (is (s/validate validation-schema 
                      {:schema/variant :tags/set
                       :tags/uuid (java.util.UUID/randomUUID)}))) 

    (testing "Regex"
      (is (thrown? Throwable (s/validate validation-schema 
                                         {:schema/variant :tags/set
                                          :tags/uuid (java.util.UUID/randomUUID)
                                          ;; tags/label should have a 3 in it
                                          :tags/label "hi th"}))))

    (testing "Enum" 
      (is (s/validate validation-schema 
                           {:schema/variant :social
                            :social/uuid (java.util.UUID/randomUUID)
                            :social/service :social/twitter})))
    
    (testing "Min length" 
      (is (thrown? Throwable (s/validate validation-schema 
                                              {:schema/variant :tags/set
                                               :tags/uuid (java.util.UUID/randomUUID)
                                               :tags/label "3"})))
      
      (is (s/validate validation-schema 
                           {:schema/variant :tags/set
                            :tags/uuid (java.util.UUID/randomUUID)
                            :tags/label "33"})))
    
    (testing "Max length" 
      (is (s/validate validation-schema 
                           {:schema/variant :tags/set
                            :tags/uuid (java.util.UUID/randomUUID)
                            :tags/label (apply str (repeat 60 3))}))
      
      (is (thrown? Throwable (s/validate validation-schema 
                                              {:schema/variant :tags/set
                                               :tags/uuid (java.util.UUID/randomUUID)
                                               :tags/label (apply str (repeat 61 3))}))))
    
    (testing "Ref validation"
      (is (s/validate validation-schema 
                           {:schema/variant :tags/set
                            :tags/tags [{:schema/variant :tags/leaf
                                         :tags/uuid (java.util.UUID/randomUUID)
                                         :tags/tag "leaf tag string"}]
                            :tags/uuid (java.util.UUID/randomUUID)
                            :tags/label "hi th 3"})))
    (testing "Ref to wrong variant type"
      (is (thrown? Throwable (s/validate validation-schema 
                                         {:schema/variant :tags/set
                                          :tags/tags [{:schema/variant :social
                                                       :social/uuid (java.util.UUID/randomUUID)
                                                       :social/service :social/twitter}]
                                          :tags/uuid (java.util.UUID/randomUUID)
                                          :tags/label "hi th 3"}))))) 
