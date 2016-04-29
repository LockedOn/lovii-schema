(ns lovii-schema.validation-test
  (:require [clojure.test :refer :all]
            [lovii-schema.core :refer :all]
            [lovii-schema.schema :as loschema]
            [datomic.api :as d]
            [schema.core :as s]
            [schema.coerce :as coerce]
            [lovii-schema.validation :as v]
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

(def validation-schema
  nil)

(alter-var-root #'validation-schema (constantly (v/schemas->validators #'validation-schema (loschema/parse-schema example))))

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

; (def lovii-schema {
; 	:settings {:schema/variant :settings
; 			   :uuid {:type :uuid :managed true}
; 			   :name {:type :string :required true :index true}
; 			   :value {:type :edn}} 
; 	;; {:agents [previous_crm(crm), 
; 	;;			allow_dup_email, allow_match_all, show_full_name
; 	;;			new_client_newsletter, new_client_xmas, show_contract_req, client_taken_email]
; 	;;  :users [add_contact(google/outlook), add_appoint(google/outlook), see_all(clients and properties), share_all_buyers]}

; 	;; Agents -> allow_commission to move to being a module

; 	:names {:schema/variant :names
; 			:uuid {:type :uuid :managed true}
; 			:given-name {:type :string :index true :label "Given Name"}
; 			:family-name {:type :string :index true :label "Family Name"}
; 			:title {:type :string :label "Title"}
; 			:salutation {:type :string :label "Letter Salutation"}
; 			:greeting {:type :string :label "Letter Greeting"}
; 			:nickname {:type :string :index true :label "Nickname"}}

; 	:addresses {:schema/variant :addresses
; 				:uuid {:type :uuid :managed true}
; 				:building-name {:type :string :index true :label "Building Name"}
; 				:att {:type :string :index true :label "Attention"}
; 				:lot {:type :string :index true :label "Lot"}
; 				:unit {:type :string :index true :label "Unit"}
; 				:street-number {:type :string :index true :label "Street Number"}
; 				:street-name {:type :string :index true :label "Street Name"}
; 				:address1 {:type :string :index true :label "Address 1"} ;; used for old data should only have this if we cant split into seperate chunks.
; 				:address2 {:type :string :index true :label "Address 2"} ;; also require in UI to save as chunked address
; 				:suburb {:type :string :index true :label "Suburb"}
; 				:state {:type :string :index true :label "State"}
; 				:country {:type :string :index true :label "Country"}
; 				:postal-code {:type :string :index true :label "Nickname"}
; 				:timezone {:type :string}
; 				:district {:type :string :index true :label "Districts"} ;; nz districts
; 				:geo-location {:type :ref :variants [:co-ordinates.single]}
;                                 ;; FIXME: BUG HERE
; 				;:notifications-allowed {:type :enum :values :notifications/method :cardinality :has-many}
;                                 }

; 	:co-ordinates [{:schema/abstract :co-ordinates
; 				    :uuid {:type :uuid :managed true}
; 				    :latitude {:type :double :required true :index true :label "Latitude"}
; 				    :longitude {:type :double :required true :index true :label "Longitude"}}
; 				   {:schema/variant :co-ordinates/area
; 				    :order {:type :int :required true}}
; 				   {:schema/variant :co-ordinates/single
;                                     ;; FIXME: made these sets
; 				    :pov-heading {:type :double :label "POV Heading" :required #{:pov-heading :pov-pitch}}
; 				    :pov-pitch {:type :double :label "POV Pitch" :required #{:pov-heading :pov-pitch}}}]

; 	:identifiers {:schema/variant :identifiers
; 				  :uuid {:type :uuid :managed true}
; 				  :number {:type :string :label "Identity Number"}
; 				  :issuer {:type :string :label "Identity Issuer"}
; 				  :type {:type :string :label "Identity Type"}
; 				  :licensee {:type :string :label "Identity Licensee"}
; 				  :expiry {:type :date :label "Identity Expiry"}}

; 	:auth-credentials {:schema/variant :auth-credentials
; 					   :uuid {:type :uuid :managed true}
; 					   :username {:type :string :unique :value :required true :label "Username"} ;; email / old username
; 					   :password {:type :string :label "Password"}
; 					   :salt {:type :string}
; 					   :two-factor {:type :boolean}}

; 	:auth-providers {:schema/variant :auth-providers
; 					 :uuid {:type :uuid :managed true}
; 					 :name {:type :string :index true :label "Auth Name"}}

; 	:auth-associations {:schema/variant :auth-associations
; 						:uuid {:type :uuid :managed true}
; 						:provider {:type :ref :variants [:auth-providers]}
; 						:provider-id {:type :string :index true} ;; for lockedOn this will be the user id
; 						:user {:type :ref :variants [:users]}}

; 	:auth-tokens {:schema/variant :auth-tokens
; 				  :uuid {:type :uuid :managed true}
; 				  :provider {:type :ref :variants [:auth-providers]}
; 				  :status {:type :enum :values {:inactive "Inactive" :active "Active"}}
; 				  :token {:type :string :index true}
; 				  :refresh {:type :string}
; 				  :expiry {:type :date-time}
; 				  :user {:type :ref :variants [:users]}}
; 	;; need to track: ip-address, OS, browsers-version, device-id 

; 	:urls {:schema/variant :urls
; 		   :uuid {:type :uuid :managed true}
; 		   :url {:type :string :label "Url"}
; 		   :label {:type :string :label "Url Label"}
; 		   :primary {:type :boolean}}

; 	:emails {:schema/variant :emails
; 			 :uuid {:type :uuid :managed true}
; 			 :email {:type :string :index true :label "Email"}
; 			 :label {:type :string :label "Email Label"} ;; may need to look at this being a enum of added labels
; 			 :confirmed {:type :enum :label "Confirmed Functionality" :values {:sms "SMS" :mms "MMS" :fax "Fax" :voiceemail "Voice Mail" :email "Email"} :cardinality :has-many}
;                          ; FIXME: enum bug here
; 			 ;:notifications-allowed {:type :enum :values :notifications/method :cardinality :has-many}
; 			 :primary {:type :boolean}}

; 	:phone-numbers {:schema/variant :phone-numbers
; 					:uuid {:type :uuid :managed true}
; 					:number {:type :string :index true :label "Number"}
; 					:country-code {:type :string :label "Country Code"}
; 					:label {:type :string :label "Phone Label"} ;; may need to look at this being a enum of added labels
; 					:confirmed {:type :enum :label "Confirmed Functionality" :values {:phone "Phone" :sms "SMS" :mms "MMS" :fax "Fax" :voiceemail "Voice Mail" :email "Email"} :cardinality :has-many}
;                                         ; FIXME: enum bug here
; 					;:notifications-allowed {:type :enum :values :notifications/method :label "Allow Notifications" :cardinality :has-many}
; 					:primary {:type :boolean :label "Primary Phone Number" :default-value false}}

; 	:social-accounts {:schema/variant :social-accounts
; 					  :uuid {:type :uuid :managed true}
; 					  :service {:type :enum :values {:twitter "Twitter"
; 					  								 :google  "Google"
; 					  								 :facebook "Facebook" 
; 					  								 :linkedin "LinkedIn"
; 					  								 :whatsapp "Whatsapp" 
; 					  								 :skype "Skype"
; 					  								 :msn "MSN" 
; 					  								 :aol "AOL"
; 					  								 :icq "ICQ"
; 					  								 :wechat "WeChat"
; 					  								 :line "Line"
; 					  								 :instagram "Instagram"
; 					  								 :pinterest "Pinterest"
; 					  								 :imessage "iMessage"
; 					  								 :youtube "Youtube"
; 					  								 :viber "Viber"
; 					  								 :flickr "Flickr" 
; 					  								 :yahoo "Yahoo!"
; 					  								 :myspace "MySpace"}
; 					  			:label "Social Service"}
; 					  :account {:type :string :index true :label "Social Account"}
; 					  ;:notifications-allowed {:type :enum :values :notifications/method :cardinality :has-many}
;                                           }

; 	:roles [{:schema/abstract :roles
; 			 :uuid {:type :uuid :managed true}
; 			 :status {:type :enum :values {:active "Active" :disabled "Disabled"} :cardinality :has-many}
; 			 :permissions {:type :ref :cardinality :has-many :variants [:permissions]}}
; 			 {:schema/variant :roles/solicitor
; 			  :solicitor {:type :ref :variants [:contacts]}}
; 			 {:schema/variant :roles/vendor
; 			  :vendor {:type :ref :variants [:contacts]}}
; 			 {:schema/variant :roles/sales-agent
; 			  :sales-agent {:type :ref :variants [:office-users]}}
; 			 {:schema/variant :roles/office-admin
; 			  :office-admin {:type :ref :variants [:office-users]}}
; 			 {:schema/variant :roles/account-owner
; 			  :account-owner {:type :ref :variants [:accounts]}}
; 			 {:schema/variant :roles/support-staff
; 			  :support-staff {:type :ref :variants [:accounts]}}
; 			 {:schema/variant :roles/pa
; 			  :pa {:type :ref :variants [:office-users]}}]

; 	:users {:schema/variant :users
; 			:uuid {:type :uuid :managed true}
; 			:contact {:type :ref :variants [:contacts]}
; 			:roles {:type :ref :variants [:roles] :cardinality :has-many}
; 			:subscriptions {:type :ref :variants [:subscriptions] :cardinality :has-many}
; 			:payment-method {:type :ref :variants [:payment-methods]}
; 			:invoices {:type :ref :variants [:invoices] :cardinality :has-many}
; 			:permissions {:type :ref :cardinality :has-many :variants [:permissions]}}

; 	;; permissions add together and start from a base of deny all and can only be added.
; 	:permissions {:schema/variant :permissions
; 				  :uuid {:type :uuid :managed true}
; 				  :action {:type :string :required true :index true}
; 				  :read {:type :boolean :required [:read :write :execute]}
; 				  :write {:type :boolean :required [:read :write :execute]}
; 				  :execute {:type :boolean :required [:read :write :execute]}}

; 	:office-users {:schema/variant :office-users
; 				   :uuid {:type :uuid :managed true}
; 				   :sms-provider {:type :ref :variants [:external-services] :cardinality :has-many}
; 				   :contact {:type :ref :variants [:contacts]}
; 				   :properties {:type :ref :variants [:properties] :cardinality :has-many}
; 				   :settings {:type :ref :variants [:settings] :cardinality :has-many}
; 				   :templates {:type :ref :variants [:templates] :cardinality :has-many}
; 				   :dna-plans {:type :ref :variants [:dna-plans] :cardinality :has-many}
; 				   :clients {:type :ref :variants [:clients] :cardinality :has-many}}

; 	:contacts {:schema/variant :contacts
; 			   :uuid {:type :uuid :managed true}
; 			   :name {:type :ref :variants [:names]}
; 			   :avatar {:type :ref :variants [:files]}
; 			   :birth-date {:type :date :label "Date of Birth"}
; 			   :primary {:type :boolean :index true :default false :label "Primary Contact"}
; 			   :identifiers {:type :ref :variants [:identifiers] :cardinality :has-many}
; 			   :addresses {:type :ref :variants [:addresses] :cardinality :has-many}
; 			   :emails {:type :ref :variants [:emails] :cardinality :has-many}
; 			   :phone-numbers {:type :ref :variants [:phone-numbers] :cardinality :has-many}
; 			   :urls {:type :ref :variants [:urls] :cardinality :has-many}
; 			   :company-detail {:type :ref :variants [:company-details]}
; 			   :social-accounts {:type :ref :variants [:social-accounts] :cardinality :has-many}
; 			   :donotcontact {:type :boolean :index true :default false :label "Do Not Contact"}
; 			   :bank-details {:type :ref :variants [:bank-details] :cardinality :has-many}
; 			   :notes {:type :ref :variants [:notes] :cardinality :has-many}}

; 	:files {:schema/variant :files
; 			:uuid {:type :uuid :managed true}
; 			:label {:type :string :index true :label "Label"}
; 			:summary {:type :string :label "Summary"}
; 			:mime-type {:type :string :index true}
; 			:storage-key {:type :string :unique :value}
; 			:public {:type :boolean}
; 			:cold-storage {:type :boolean}
; 			:bytes {:type :int}
; 			:sort-order {:type :int}
; 			:original-name {:type :string :index true}
; 			:available-from {:type :date-time}
; 			:cover {:type :ref :variants [:files]}
; 			:tracking-performance {:type :ref :variants [:tracking-performances]}}

; 	:tags [{:schema/abstract :tags
; 			:uuid {:type :uuid :managed true}
; 			:label {:type :string :required false :min 0 :max 60 :regex "" :index true :label "Label"}
; 			:inactive {:type :boolean :required false :index true :default false}}
;                {:schema/variant :tags/set
;                 :tags {:type :ref :required false :cardinality :has-many :variants [:tags/leaf]}}
; 		   {:schema/variant :tags/leaf
; 			:tag {:type :string :required true :index true}}]

; 	;; required :: boolean = either always required, vector = one or more required, set = all required or none present, sets and vectors can be recursive
; 	:tags-values [{:schema/abstract :tags-values
; 				   :tag {:type :ref :required true :variants [:tags/leaf]}
; 				   :uuid {:type :uuid :managed true}}
;  				  {:schema/variant :tags-values/measurements
; 				   :measurement {:type :ref :required true :variants [:measurements] :label "Measurement"}}
; 				  {:schema/variant :tags-values/measurements-range
; 				   :measurement-min {:type :ref :required [:measurement-min :measurement-max] :variants [:measurements] :label "Min"}
; 				   :measurement-max {:type :ref :required :measurement-min :variants [:measurements] :label "Max"}}
; 				  {:schema/variant :tags-values/prices
; 				   :price {:type :ref :required true :variants [:prices] :label "Price"}}
; 				  {:schema/variant :tags-values/prices-range
; 				   :price-min {:type :ref :required [:price-min :price-max] :variants [:prices] :label "Min"}
; 				   :price-max {:type :ref :required [:price-min :price-max] :variants [:prices] :label "Max"}}
; 				  {:schema/variant :tags-values/boolean
; 				   :boolean {:type :boolean :index true}}
; 				  {:schema/variant :tags-values/int
; 				   :int {:type :int :index true}}
; 				  {:schema/variant :tags-values/int-range
; 				   :int-min {:type :int :required [:int-min :int-max] :label "Min" :index true}
; 				   :int-max {:type :int :required [:int-min :int-max] :label "Max" :index true}}
; 				  {:schema/variant :tags-values/double
; 				   :double {:type :double :index true}}
; 				  {:schema/variant :tags-values/double-range
; 				   :double-min {:type :double :required [:double-min :double-max] :label "Min" :index true}
; 				   :double-max {:type :double :required [:double-min :double-max] :label "Max" :index true}}
; 				  {:schema/variant :tags-values/date-time
; 				   :date-time {:type :date-time :index true :label "Date"}}
; 				  {:schema/variant :tags-values/date-time-range
; 				   :date-time-min {:type :date-time :required [:date-time-min :date-time-max] :label "Date From" :index true}
; 				   :date-time-max {:type :date-time :required [:date-time-min :date-time-max] :label "Date To" :index true}}
; 				  {:schema/variant :tags-values/string
; 				   :string {:type :string :index true}}
; 				  {:schema/variant :tags-values/string-large
; 				   :string-large {:type :string-large :index true}}				   
; 				  {:schema/variant :tags-values/tag-enum
; 				   :tags {:type :ref :required true :variants [:tags/set]}}]

; 	;; homepass - office, checkin note, checkins, client notes, clients/contacts
; 	;; ljh - office, user, property
; 	;; import - properties, clients, notes
; 	;; leads email processing - office, office-user
; 	:third-parties [{:schema/abstract :third-parties
; 					 :uuid {:type :uuid :managed true}
; 					 :last-sync {:type :date-time} ;; could also be tx time
; 					 :third-party-id {:type :string :index true}}
; 					{:schema/variant :third-parties/stripe
; 					 :assign-to {:type :ref :variants [:assign-tos/users]}}
; 					{:schema/variant :third-parties/homepass
; 					 :assign-to {:type :ref :variants [:assign-tos]}}
; 					{:schema/variant :third-parties/ljh
; 					 :assign-to {:type :ref :variants [:assign-tos/office :assign-tos/office-user :assign-tos/property]}}
; 					{:schema/variant :third-parties/import
; 					 :assign-to {:type :ref :variants [:assign-tos]}}
; 					{:schema/variant :third-parties/leads
; 					 :assign-to {:type :ref :variants [:assign-tos/office :assign-tos/office-user]}}]

; 	:affiliates {:schema/variant :affiliates
; 				 :uuid {:type :uuid :managed true}
; 				 :contact {:type :ref :variants [:contacts]}
; 				 :tracking-tags {:type :ref :variants [:tags/leaf] :cardinality :has-many}}

; 	:invoices {:schema/variant :invoices
; 			   :uuid {:type :uuid :managed true}
; 			   :items {:type :ref :variants [:invoice-items] :cardinality :has-many}
; 			   :invoice-number {:type :string :index true :label "Invoice Number"}
; 			   :date-time {:type :date-time :index true :label "Date"}
; 			   :recipient {:type :ref :variants [:contacts]}
; 			   :payment-method {:type :ref :variants [:payment-methods]}
; 			   :total {:type :ref :variants [:prices] :label "Total"}
; 			   :file {:type :ref :variants [:files]}} ;; pdf stored

; 	:invoice-items {:schema/variant :invoice-items
; 					:uuid {:type :uuid :managed true}
; 					:label {:type :string :label "Label"}
; 					:price {:type :ref :variants [:prices] :label "Price"}}

; 	:bank-details {:schema/variant :bank-details
; 				   :uuid {:type :uuid :managed true}
; 				   :bsb {:type :string :label "Price"}
; 				   :account-name {:type :string :label "Account Name"}
; 				   :account-number {:type :string :label "Account Number"}
; 				   :bank {:type :string :label "Bank"}
; 				   :label {:type :string :label "Label"}
; 				   :primary {:type :boolean :label "Primary Account"}}

; 	:payment-methods {:schema/variant :payment-methods
; 					  :uuid {:type :uuid :managed true}
; 					  :payment-service {:type :enum :values {:stripe "Stripe" :paypal "Paypal"}}
; 					  :payment-token {:type :string}
; 					  :payment-preview {:type :string :label "Last 4 Digits"} ;; potential better name
; 					  :expiry {:type :date-time :label "Expiry Date"}}

; 	:accounts {:schema/variant :accounts
; 			   :uuid {:type :uuid :managed true}
; 			   :exit-feedback {:type :ref :variants [:notes] :cardinality :has-many}
; 			   :settings {:type :ref :variants [:settings] :cardinality :has-many}}

; 	:taxes {:schema/variant :taxes
; 			:uuid {:type :uuid :managed true}
; 			:label {:type :string :label "Label"}
; 			:amount {:type :decimal :label "Amount"}
; 			:rate {:type :decimal :label "Rate"}
; 			:authority {:type :string :label "Authority"}}

; 	:prices {:schema/variant :prices
; 			 :uuid {:type :uuid :managed true}
; 			 :price {:type :decimal :index true :label "Price"}
; 			 :tax {:type :ref :variants [:taxes] :label "Tax" :cardinality :has-many}
; 			 :currency {:type :enum :label "Currency" :values {:aud "AUD" :nz "NZD"}}} 

; 	:subscriptions {:schema/variant :subscriptions
; 					:uuid {:type :uuid :managed true}
; 					:referral-source {:type :ref :variants [:tags/leaf]}
; 					:promo-code {:type :ref :variants [:promo-codes]}
; 					:status {:type :enum :values {:active "Active" 
; 												  :disabled "Disabled" 
; 												  :closed "Closed" 
; 												  :failed-payment "Failed Payment"
; 												  :trial "Trial"
; 												  :pending "Pending"} 
; 										 :index true :cardinality :has-many} ;; active, failed payment, etc
; 					:packages {:type :ref :variants [:packages] :cardinality :has-many} ;; {:agents [allow_portal_push]}
; 					:expiry {:type :date-time :index true}}

; 	:packages {:schema/variant :packages
; 			   :uuid {:type :uuid :managed true}
; 			   :label {:type :string :index true :label "Label"}
; 			   :summary {:type :string :label "Summary"}
; 			   :items {:type :ref :variants [:package-items] :cardinality :has-many}
; 			   :public {:type :boolean :index true}
; 			   ;; TODO: once the package goes available - read only for package and contained items
; 			   :available {:type :ref :variants [:durations]}}

; 	:package-items {:schema/variant :package-items
; 					:uuid {:type :uuid :managed true}
; 					:price {:type :ref :variants [:prices]}
; 					:duration {:type :ref :variants [:durations]} ;; re-occuring / monthly / single
; 					:module {:type :ref :variants [:modules]}
; 					:quantity {:type :int}
; 					:trial-period {:type :ref :variants [:durations]}}

; 	:modules [{:schema/abstract :modules
; 			   :uuid {:type :uuid :managed true}
; 			   :label {:type :string :index true :label "Label"}
; 			   :summary {:type :string :label "Summary"}
; 			   :dependencies {:type :ref :variants [:modules] :cardinality :has-many}
; 			   :assignable-scope {:type :enum :values {:office "Office"
; 			   										   :account "Account"
; 			   										   :office-user "Office User"
; 			   										   :user "User"} 
; 			   								  :cardinality :has-many}}
; 			  {:schema/variant :modules/system
; 			   :triggers {:type :ref :variants [:tags/leaf] :cardinality :has-many}
; 			   :permissions {:type :ref :cardinality :has-many :variants [:permissions]}}
; 			  {:schema/variant :modules/dna-pack
; 			   :dna-pack {:type :ref :variants [:dna-packs]}}
; 			  {:schema/variant :modules/consumable}]

; 	:module-uses {:schema/variant :module-uses
; 				  :uuid {:type :uuid :managed true}
; 				  :account {:type :ref :variants [:accounts] :required true}
; 				  :module {:type :ref :variants [:modules] :required true}
; 				  :assign-to {:type :ref :variants [:assign-tos/office-users :assign-tos/office :assign-tos/user] :required true}}

; 	:promo-codes {:schema/variant :promo-codes
; 				  :uuid {:type :uuid :managed true}
; 				  :code {:type :string :index true :label "Code"}
; 				  ;;TODO: read-only once available
; 				  :available {:type :ref :variants [:durations] :label "Available"}
; 				  :quantity {:type :int :label "Qty"}
; 				  :fixed-price {:type :ref :variants [:prices]} 
; 				  :discount-price {:type :ref :variants [:prices]} 
; 				  :discount-percentage {:type :double}
; 				  :price-duration {:type :ref :variants [:durations]}
; 				  :packages {:type :ref :variants [:packages] :required [:packages :package-items] :cardinality :has-many}
; 				  :package-items {:type :ref :variants [:package-items] :required [:packages :package-items] :cardinality :has-many}}

; 	:office {:schema/variant :office
; 			 :uuid {:type :uuid :managed true}
; 			 :company-detail {:type :ref :variants [:company-details]}
; 			 :portal-push {:type :ref :variants [:external-services] :cardinality :has-many}
; 			 :sms-provider {:type :ref :variants [:external-services]}
; 			 :notifications {:type :ref :variants [:notifications] :cardinality :has-many} ;; ownership of notification for display of possible notifications for clients to sign up to.
; 			 :clients {:type :ref :variants [:clients] :cardinality :has-many}
; 			 :properties {:type :ref :variants [:properties] :cardinality :has-many}
; 			 :property-details-defaults {:type :ref :variants [:property-details-defaults]}
; 			 :temperatures {:type :ref :variants [:tags/leaf] :cardinality :has-many}
; 			 :client-groups {:type :ref :variants [:tags/leaf] :cardinality :has-many}
; 			 :buyer-categories {:type :ref :variants [:tags/leaf] :cardinality :has-many}
; 			 :templates {:type :ref :variants [:templates] :cardinality :has-many}
; 			 :property-groups {:type :ref :variants [:tags/leaf] :cardinality :has-many}
; 			 :keywords {:type :ref :variants [:tags/leaf] :cardinality :has-many}
; 			 :leads {:type :ref :variants [:leads] :cardinality :has-many}
; 			 :referral-sources {:type :ref :variants [:tags/leaf] :cardinality :has-many}
; 			 :campaigns {:type :ref :variants [:campaigns] :cardinality :has-many}
; 			 :internal-expenses {:type :ref :variants [:expense-items] :cardinality :has-many}
; 			 :notes {:type :ref :variants [:notes] :cardinality :has-many}
; 			 :dna-plans {:type :ref :variants [:dna-plans] :cardinality :has-many}
; 			 :settings {:type :ref :variants [:settings] :cardinality :has-many}
; 			 :procedures {:type :ref :variants [:procedures] :cardinality :has-many}}

; 	:external-services-defaults {:schema/variant :external-services-defaults
; 								 :uuid {:type :uuid :managed true}
; 								 :external-services {:type :ref :variants [:external-services] :cardinality :has-many}}

; 	:external-services {:schema/variant :external-services
; 						:uuid {:type :uuid :managed true}
; 						:label {:type :string :index true :label "Label"}
; 						:type {:type :enum :index true :label "Service Type" :values {:portal-push "Portal Push" :sms-mms "SMS/MMS" }}
; 						:default {:type :boolean :label "Default Service"}
; 						;:external-service {:type :ref :variants [:external-services]}
; 						:inactive {:type :boolean :index true}
; 						:configuration {:type :edn :label "Configuration"}}

; 	:clients {:schema/variant :clients
; 			  :uuid {:type :uuid :managed true}
; 			  :company-detail {:type :ref :variants [:company-details]}
; 			  :referral-source {:type :ref :variants [:tags/leaf]}
; 			  :client-groups {:type :ref :variants [:tags/leaf] :cardinality :has-many}
; 			  :buyer-criterions {:type :ref :variants [:buyer-criterions] :cardinality :has-many}
; 			  :inactive {:type :boolean :index true}
; 			  :contacts {:type :ref :variants [:contacts] :cardinality :has-many}
; 			  :documents {:type :ref :variants [:files] :cardinality :has-many}
; 			  :notes {:type :ref :variants [:notes] :cardinality :has-many}}

; 	:company-details {:schema/variant :company-details
; 					  :uuid {:type :uuid :managed true}
; 					  :name {:type :string :index true :label "Company Name"}
; 					  :trading-as {:type :string :index true :label "Trading As"}
; 					  :identifiers {:type :ref :variants [:identifiers] :cardinality :has-many}}

; 	:notifications {:schema/variant :notifications
; 					:uuid {:type :uuid :managed true}
; 					:name {:type :string} ;; namespaced - allowing to group the notifications together in a large list
; 					:label {:type :string :label "Label"}
; 					:subscribable {:type :boolean}
; 					:method {:type :enum :index true :values {:email "Email" :sms "SMS" :mms "MMS" :tweet "Tweet" :voicemail "Voicemail" :letter "Letter"}}}

; 	:notifications-subscriptions {:schema/variant :notifications-subscriptions
; 								  :uuid {:type :uuid :managed true}
; 								  :notification {:type :ref :variants [:notifications]}
; 								  :contact {:type :ref :variants [:contacts]}}

; 	:property-details-defaults {:schema/variant :property-details-defaults
; 								:uuid {:type :uuid :managed true}
; 								:land-details {:type :ref :variants [:tags-values] :cardinality :has-many}
; 								:listings-details {:type :ref :variants [:tags-values] :cardinality :has-many}
; 								:property-details {:type :ref :variants [:tags-values] :cardinality :has-many}
; 								:rural-details {:type :ref :variants [:tags-values] :cardinality :has-many}
; 								:rental-details {:type :ref :variants [:tags-values] :cardinality :has-many}
; 								:commercial-details {:type :ref :variants [:tags-values] :cardinality :has-many}
; 								:business-details {:type :ref :variants [:tags-values] :cardinality :has-many}}

; 	:properties [{:schema/abstract :properties
; 				  :uuid {:type :uuid :managed true}
; 				  :category {:type :enum :index true :values {:residential "Residential"
; 				  								  			  :rural "Rural"
; 				  								 			  :land "Land"
; 				  								 			  :rental "Rental"
; 				  								 			  :holiday "Holiday"
; 				  								 			  :commercial "Commercial"
; 				  								 			  :business "Business"}}
; 				  :property-type {:type :ref :variants [:tags/leaf]} ;; house, apartment. alipine, terrace, etc
; 				  :notes {:type :ref :variants [:notes] :cardinality :has-many}
; 				  :address {:type :ref :variants [:addresses]}
; 				  :appraisal {:type :ref :variants [:appraisals]}
; 				  :campaigns {:type :ref :variants [:campaign-items]}
; 				  :internal-expenses {:type :ref :variants [:expense-items] :cardinality :has-many}
; 				  :land-details {:type :ref :variants [:tags-values] :cardinality :has-many}
; 				  :other-misc-features {:type :string :cardinality :has-many}
; 				  :documents {:type :ref :variants [:files] :cardinality :has-many}
; 				  :keywords {:type :ref :variants [:tags/leaf] :cardinality :has-many}
; 				  :property-groups {:type :ref :variants [:tags/leaf] :cardinality :has-many}
; 				  :leads {:type :ref :variants [:leads] :cardinality :has-many}
; 				  :tenants {:type :ref :variants [:tenants] :cardinality :has-many}
; 				  :temperature {:type :ref :variants [:tags/leaf]}
; 				  :vendor-type {:type :ref :variants [:tags/leaf]} ;; investor, pricate etc
; 				  :vendors {:type :ref :variants [:sales-parties] :cardinality :has-many} ;; vendor is the notition of the parties selling a property/lease/rental-agreement
; 				  :vendor-documents {:type :ref :variants [:files] :cardinality :has-many}
; 				  :vpa-payments {:type :ref :variants [:vpa-payments] :cardinality :has-many}}
; 				 {:schema/variant :properties/sales
; 				  :status {:type :enum :index true :values {:prospect "Prospect"
; 															:potential "Potential"
; 															:appraised "Appraised"
; 															:listed "Listed"
; 															:exchanged "Exchanged"
; 															:settled "Settled"
; 															:off-market "Taken Off Market"}}
; 				  :listing {:type :ref :variants [:listings]}
; 				  :listings-details {:type :ref :variants [:tags-values] :cardinality :has-many} ;; to be maintained programatically based off the listing
; 				  :property-details {:type :ref :variants [:tags-values] :cardinality :has-many}
; 				  :rental-details {:type :ref :variants [:tags-values] :cardinality :has-many}
; 				  :rural-details {:type :ref :variants [:tags-values] :cardinality :has-many}
; 				  :sales-advice {:type :ref :variants [:sales-advices]}
; 				  :sales-offers {:type :ref :variants [:sales-offers] :cardinality :has-many}
; 				  :inspection-times {:type :ref :variants [:calendar] :cardinality :has-many}}
; 				 {:schema/variant :properties/rentals
; 				  :status {:type :enum :index true :values {:prospect "Prospect"
; 															:potential "Potential"
; 															:appraised "Appraised"
; 															:off-market "Taken Off Market"
; 															:for-rent "For Rent"
; 															:leased "Leased"}}
; 				  :rental-details {:type :ref :variants [:tags-values] :cardinality :has-many}
; 				  :property-details {:type :ref :variants [:tags-values] :cardinality :has-many}
; 				  :inspection-times {:type :ref :variants [:calendar] :cardinality :has-many}}
; 				 {:schema/variant :properties/commercial
; 				  :status {:type :enum :index true :values {:prospect "Prospect"
; 															:potential "Potential"
; 															:appraised "Appraised"
; 															:on-market "Active"
; 															:exchanged "Exchanged"
; 															:settled "Settled"
; 															:off-market "Taken Off Market"
; 															:leased "Leased"}}
; 				  :commercial-details {:type :ref :variants [:tags-values] :cardinality :has-many}}
; 				 {:schema/variant :properties/business
; 				  :status {:type :enum :index true :values {:prospect "Prospect"
; 															:potential "Potential"
; 															:appraised "Appraised"
; 															:listed "Listed"
; 															:exchanged "Exchanged"
; 															:settled "Settled"
; 															:off-market "Taken Off Market"}}
; 				  :business-details {:type :ref :variants [:tags-values] :cardinality :has-many}}]

; 	:sales-parties {:schema/variant :sales-parties
; 					:uuid {:type :uuid :managed true}
; 					:client {:type :ref :variants [:clients]}
; 					:solicitor {:type :ref :variants [:clients]}
; 					:solicitor-contact {:type :ref :variants [:contacts]}
; 					:solicitor-rate {:type :double :label "Rate"}}

; 	:buyer-criterions {:schema/variant :buyer-criterions
; 					   :uuid {:type :uuid :managed true}
; 					   :label {:type :string :label "Label"}
; 					   :inactive {:type :boolean :index true}
; 					   :temperature {:type :ref :variants [:tags/leaf]}
; 					   :property-types {:type :ref :variants [:tags/leaf] :cardinality :has-many} ;; matching that of the property
; 					   :listing-details {:type :ref :variants [:tags-values] :cardinality :has-many}
; 					   :land-details {:type :ref :variants [:tags-values] :cardinality :has-many}
; 					   :commercial-details {:type :ref :variants [:tags-values] :cardinality :has-many}
; 					   :property-details {:type :ref :variants [:tags-values] :cardinality :has-many}
; 					   :rental-details {:type :ref :variants [:tags-values] :cardinality :has-many}
; 					   :keywords {:type :ref :variants [:tags/leaf] :cardinality :has-many}
; 					   :addresses {:type :ref :variants [:addresses] :cardinality :has-many} ;;  suburb matching
; 					   :geo-locations {:type :ref :variants [:co-ordinates/area] :cardinality :has-many :min 2} ;; geo matching
; 					   :price-range {:type :ref :variants [:tags-values.prices-range]} ;;  price matching
; 					   :notes {:type :ref :variants [:notes] :cardinality :has-many}}

; 	:agentpoint-contacts {:schema/variant :agentpoint-contacts
; 						  :uuid {:type :uuid :managed true}
; 						  :provider {:type :ref :variants [:external-services]}
; 						  :notification {:type :ref :variants [:notifications] :cardinality :has-many}
; 						  :office {:type :ref :variants [:office]}}

; 	:vpa-payments {:schema/variant :vpa-payments
; 				   :uuid {:type :uuid :managed true}
; 				   :price {:type :ref :variants [:prices] :label "Price"}
; 				   :type {:type :enum :values {:payments/credit-card "Credit Card" 
; 				   							   :payments/direct-deposit "Direct Deposit" 
; 				   							   :payments/cash "Cash" 
; 				   							   :payments/cheque "Cheque"}
; 				   		  :label "Payment Method"}
; 				   :date-time {:type :date-time :index true :label "Date Time"}}

; 	:notes {:schema/variant :notes
; 			:uuid {:type :uuid :managed true}
; 			:note {:type :string-large :index true}}

; 	:campaigns {:schema/variant :campaigns
; 				:uuid {:type :uuid :managed true}
; 				:label {:type :string :label "Label"}
; 				:summary {:type :string :label "Summary"}
; 				:campaign-items {:type :ref :variants [:campaign-items] :cardinality :has-many}}

; 	:campaign-items {:schema/variant :campaign-items
; 					 :uuid {:type :uuid :managed true}
; 					 :campaign {:type :ref :variants [:campaigns]}
; 					 :category {:type :enum :values {:administration "Administration"
; 													 :copywriting "Copywriting"
; 													 :display "Display"
; 													 :distribution "Distribution"
; 													 :floorplan "Floor Plan"
; 													 :magazine "Magazine"
; 													 :newspaper "Newspaper"
; 													 :photography "Photography"
; 													 :print "Print"
; 													 :radio "Radio"
; 													 :signage "Signage"
; 													 :video "Video"
; 													 :website "Website"
; 													 :portal-push "Portal Push"
; 													 :sms "SMS"
; 													 :mms "MMS"
; 													 :letter "Letter"
; 													 :email "Email"
; 													 :misc "Misc"}}
; 					 :portal {:type :ref :variants [:portal-subscriptions/defaults] :cardinality :has-many}
; 					 :product-code {:type :string}
; 					 :publication {:type :string}
; 					 :commenced {:type :date}
; 					 :price {:type :ref :variants [:prices]}
; 					 :notes {:type :ref :variants [:notes]}
; 					 :template {:type :ref :variants [:templates] :comment "Link to template for tracking of emails / sms / mms"}
; 					 :campaign-items {:type :ref :variants [:campaign-items] :comment "Indicate if this item was from a campaign"}
; 					 :tracking-performance {:type :ref :variants [:tracking-performances]}
; 					 :marketing {:type :ref :variants [:marketing-items]}}

; 	:tracking-performances {:schema/variant :tracking-performances
; 						    :uuid {:type :uuid :managed true}
; 						    :status {:type :enum :index true :values {:sent "Sent" :bounced "Bounced" :failed "Failed"}}
; 						    :failed-reason {:type :string}
; 						    :clicks {:type :int}
; 						    :views {:type :int}}

; 	:marketing-items {:schema/variant :marketing-items
; 					  :uuid {:type :uuid :managed true}
; 					  :type {:type :enum :label "Type" :index true :values {:website "Website" :email "Email" :print "Print"} :cardinality :has-many}
; 					  :websites {:type :ref :variants [:urls] :cardinality :has-many}
; 					  :videos {:type :ref :variants [:urls] :cardinality :has-many}
; 					  :virtual-tours {:type :ref :variants [:urls] :cardinality :has-many}
; 					  :display {:type :enum :label "Display" :values {:hide-address "Hide Address" 
; 					  												  :hide-price "Hide Price"
; 					  												  :hide-under-offer "Hide Under Offer"
; 					  												  :hide-sold-price "Hide Sold Price"} :cardinality :has-many}
; 					  :floorplans {:type :ref :variants [:files] :cardinality :has-many}
; 					  :images {:type :ref :variants [:files] :cardinality :has-many}
; 					  :language {:type :enum :label "Language" :index true :values {:language/en-au "English (Australian)"}}
; 					  :subject {:type :string :label "Subject"}
; 					  :heading {:type :string :label "Heading"}
; 					  :summary {:type :string-large :label "Summary"}}

; 	:portal-subscriptions [{:schema/abstract :portal-subscriptions
; 							:uuid {:type :uuid :managed true}
; 							:unique-id {:type :string :comment "Used to send to a portal to identify the property, generally set by sender or previous sender."}
; 							:portal-unique-id {:type :string :comment "Portal's id on the portals site. (Useful for matching leads)"}
; 							:last-upload {:type :date-time :index true}
; 							:activity {:type :string :cardinality :has-many} ;; REVIEW
; 							:property-status {:type :enum :label "Status" :values {:current "Current" :off-market "Off Market" :sold "Sold" :leased "Leased"}}
; 							:primary-agent {:type :ref :variants [:assign-tos/office-user]}
; 							:secondary-agent {:type :ref :variants [:assign-tos/office-user]}}
; 						   {:schema/variant :portal-subscriptions/defaults
; 							:portals {:type :ref :variants [:portal-subscriptions/specifics] :cardinality :has-many :comment "Used to overwrite settings for a single portal"}}
; 						   {:schema/variant :portal-subscriptions/specifics
; 							:portal {:type :ref :variants [:external-services]}}]

; 	:durations {:schema/variant :durations
; 				:uuid {:type :uuid :managed true}
; 				:start {:type :date-time :index true :label "Start"}
; 				:duration {:type :int :index true :label "Duration"}
; 				:type {:type :enum :index true :values {:seconds "Seconds" 
; 														:minutes "Minutes"
; 														:hours "Hours"
; 														:days "Days"
; 														:weeks "Weeks"
; 														:fortnights "Fortnights"
; 														:months "Months"
; 														:years "Years"}
; 					   :label "Duration Type"}
; 				:end {:type :date-time :index true :label "End"}}

; 	:calendar {:schema/variant :calendar
; 			   :uuid {:type :uuid :managed true}
; 			   :label {:type :string :index true :label "Label"}
; 			   :venue-address {:type :ref :variants [:addresses]}
; 			   :notes {:type :ref :variants [:notes] :cardinality :has-many}
; 			   :completed {:type :date-time :label "Completed" :cardinality :has-many :index true}
; 			   :missed {:type :date-time :label "Missed" :cardinality :has-many :index true}
; 			   :start {:type :ref :variants [:durations]}
; 			   :interval {:type :ref :variants [:durations]}
; 			   :travel-time {:type :ref :variants [:durations]}
; 			   :alert {:type :ref :variants [:durations]}
; 			   :owner {:type :ref :variants [:assign-tos/office-user]}
; 			   :attendees {:type :ref :variants [:assign-tos/office-user :assign-tos/contacts] :cardinality :has-many}
; 			   :invitees {:type :ref :variants [:assign-tos/office-user :assign-tos/contacts] :cardinality :has-many}
; 			   :notifications {:type :ref :variants [:notifications] :cardinality :has-many}}

; 	:appraisals {:schema/variant :appraisals
; 				 :uuid {:type :uuid :managed true}
; 				 :notes {:type :ref :variants [:notes] :cardinality :has-many}
; 				 :appointments {:type :ref :variants [:calendar] :cardinality :has-many}
; 				 :client-price {:type :ref :variants [:prices]}
; 				 :appraised-price {:type :ref :variants [:prices]}
; 				 :commission-rate {:type :double}
; 				 :estimated-commission {:type :ref :variants [:price]}}

; 	:listings {:schema/variant :listings
; 			   :uuid {:type :uuid :managed true}
; 			   :sale-method {:type :enum :index true :label "Method Of Sale" :values {:sale-method/private-treaty "Private" :sale-method/auction "Auction"}}
; 			   :listing-start {:type :date :label "Date Listed" :index true}
; 			   :listing-contract-received {:type :date :index true :label "Contract Received Date"}
; 			   :listing-contract-expiry {:type :date :index true :label "Contract Expiration Date"}
; 			   :listing-agents {:type :ref :variants [:office-users] :cardinality :has-many :label "Listed By"}
; 			   :withdrawn {:type :date :index true :label "Date Withdrawn"}
; 			   :withdrawn-reason {:type :string :label "Withdrawn Because?"}
; 			   :auctioneer {:type :ref :variants [:contacts] :label "Auctioneer"}
; 			   :auction {:type :ref :variants [:calendar] :label "Auction Date"}
; 			   :sale-deadline {:type :date :label "To Be Sold By Date"}
; 			   :vendor-price {:type :ref :variants [:prices] :label "Vendor Expectation" :comment "Vendor expected price"}
; 			   :agent-price {:type :ref :variants [:prices] :label "Agent Expectation" :comment "Agent expected price"}
; 			   :vendor-notes {:type :ref :variants [:notes] :cardinality :has-many :label "Vendor Notes" :comment "Notes to be displayed on the vendor portal"}
; 			   :market-price {:type :ref :variants [:prices] :label "Listing Price - Sorting"}
; 			   :market-price-guide {:type :string :label "Listing Price - Guide"}
; 			   :file-reference {:type :string :index true :label "Agency File Ref Number"}
; 			   :sent-contracts {:type :ref :variants [:sent-contracts] :cardinality :has-many}
; 			   :vpa-agreed-amount {:type :ref :variants [:price] :label "VPA - Agreed Amount"}
; 			   :commission-rate {:type :ref :variants [:part-prices]}}

; 	:sent-contracts {:schema/variant :sent-contracts
; 					 :uuid {:type :uuid :managed true}
; 					 :client {:type :ref :variants [:clients]}
; 					 :issued {:type :date-time :index true}
; 					 :notes {:type :ref :variants [:notes] :cardinality :has-many}}

; 	:sales-advices {:schema/variant :sales-advices
; 				    :uuid {:type :uuid :managed true}
; 				    :sale-method {:type :enum :label "Method Of Sale" :values {:sale-method/private-treaty "Private" :sale-method/auction "Auction"}} 
; 				    :sales-agents {:type :ref :variants [:office-users] :cardinality :has-many} ;;opentially has-many
; 				    :vendors {:type :ref :variants [:sales-parties] :cardinality :has-many}
; 				    :purchasers {:type :ref :variants [:sales-parties] :cardinality :has-many}
; 				    :contract-price {:type :ref :variants [:prices]}
; 				    :commission-splits {:type :ref :variants [:price-assignments] :cardinality :has-many}
; 				    :commission-amount {:type :ref :variants [:prices]}
; 				    :outstanding-marketing {:type :ref :variants [:prices]}
; 				    :outstanding-monies {:type :ref :variants [:prices]}
; 				    :deposit-required {:type :ref :variants [:prices]}
; 				    :deposit-held {:type :ref :variants [:prices]}
; 				    :receipt-number {:type :string :min 0 :label "Receipt Number"} ;; ui control to integer > 0
; 				    :special-conditions {:type :string :label "Special Conditions"}
; 				    :settlement-weeks {:type :int :label "Settlement Weeks"}
; 				    :under-contract-date {:type :date :label "Under Contract Date"}
; 				    :exchanged-date {:type :date :label "Exchanged Date"}
; 				    :settled-date {:type :date :label "Settled Date"}
; 				    :proposed-settlement-date {:type :date :label "Proposed Settlement Date"}
; 				    :finance-date {:type :date :label "Finance Date"}
; 				    :pest-inspection-date {:type :date :label "Pest Inspection Date"}
; 				    :building-inspection-date {:type :date :label "Building Inspection Date"}
; 				    :deposit-due {:type :date :label "Deposit Due"}}

; 	:measurements {:schema/variant :measurements
; 				   :uuid {:type :uuid :managed true}
; 				   :variant {:type :enum :values {:area "Area" :distance "Distance"}} 
; 				   :value {:type :double}
; 				   :co-ordinate-type {:type :enum :values {:top "Top" :bottom "Bottom" :left "Left" :right "Right" :centre "Centre"} :cardinality :has-many}
; 				   :unit {:type :enum :label "Unit Of Measure" :values {:sqm "sqm" :hectare "ha" :acre "ac" :mm "mm" :inch "in"}}}

; 	:commercial-details {:schema/variant :commercial-details
; 						 :uuid {:type :uuid :managed true}}

; 	:sales-offers {:schema/variant :sales-offers
; 				   :uuid {:type :uuid :managed true}
; 				   :accepted {:type :boolean :index true :label "Accpeted"}
; 				   :offer-date-time {:type :date-time :label "Offer Date"}
; 				   :notes {:type :ref :variants [:notes] :cardinality :has-many}
; 				   :price {:type :ref :variants [:prices]}
; 				   :purchaser {:type :ref :variants [:clients]}}

; 	:tenants {:schema/variant :tenants
; 			  :uuid {:type :uuid :managed true}
; 			  :client {:type :ref :variants [:clients]}}

; 	:templates [{:schema/abstract :templates
; 				 :uuid {:type :uuid :managed true} ;; merge with uuid in the body
; 				 :label {:type :string :index true :label "Label"}
; 				 :summary {:type :string :label "Summary"} ;; is the subject for emails, or description for articles
; 				 :body {:type :string-large :label "Body"} ;; merge in images/files with a merge code {file_{uuid}} merge other templates withe {template_{uuid}}
; 				 :urls {:type :ref :variants [:urls] :cardinality :has-many}
; 				 :sensitive {:type :boolean :label "Sensitive" :index true :default-value false :hint "Contains sensitive merge fields" :comment "Restrict documents with what merge tags a supported"} ;; does the template contain sensitive information requiring elevated permissions to use
; 				 :template {:type :ref :variants [:templates]}
; 				 :tracking-tag {:type :ref :variants [:tags/leaf]}
; 				 :merge-defaults {:type :ref :variants [:merge-defaults] :cardinality :has-many}
; 				 :files {:type :ref :variants [:files] :required false :cardinality :has-many}}
; 				{:schema/variant :templates/email
; 				 :orientation {:type :enum :label "Orientation" :values {:fluid "Fluid" :fixed "Fixed"} :default-value :fluid}} ;; attachments for emails - mms file 
; 				{:schema/variant :templates/mms
; 				 :files {:type :ref :variants [:files] :required true :cardinality :has-many}}
; 				{:schema/variant :templates/html
; 				 :orientation {:type :enum :label "Orientation" :values {:fluid "Fluid" :fixed "Fixed"} :default-value :fluid}}
; 				{:schema/variant :templates/brochrue
; 				 :orientation {:type :enum :label "Orientation" :values {:protrait "Portrait" :landscape "Landscape"} :default-value :protrait}}
; 				{:schema/variant :templates/property}
; 				{:schema/variant :templates/letter}
; 				{:schema/variant :templates/client}
; 				{:schema/variant :templates/sms}
; 				{:schema/variant :templates/article}
; 				{:schema/variant :templates/css}
; 				{:schema/variant :templates/gallery
; 				 :orientation {:type :enum :label "Orientation" :values {:fluid "Fluid" :fixed "Fixed"} :default-value :fluid}}
; 				{:schema/variant :templates/ipad
; 				 :orientation {:type :enum :label "Orientation" :values {:fluid "Fluid" :fixed "Fixed"}}}
; 				{:schema/variant :templates/phone}
; 				{:schema/variant :templates/script}]

; 	:template-defaults {:schema/variant :template-defaults
; 					    :uuid {:type :uuid :managed true}
; 					    :template {:type :ref :variants [:templates]}}

; 	:merge-defaults {:schema/variant :merge-defaults
; 					 :uuid {:type :uuid :managed true}
; 					 :code {:type :string :label "Code" :index true}
; 					 :value {:type :string :label "Value"}
; 					 :files {:type :ref :variants [:files]}}

; 	:assign-tos [{:schema/abstract :assign-to
; 				  :uuid {:type :uuid :managed true}}
; 				 {:schema/variant :assign-tos/client
; 				  :client {:type :ref :variants [:clients]}}
; 				 {:schema/variant :assign-tos/contact
; 				  :contact {:type :ref :variants [:contacts]}}
; 				 {:schema/variant :assign-tos/property
; 				  :property {:type :ref :variants [:properties]}}
; 				 {:schema/variant :assign-tos/office-user
; 				  :office-user {:type :ref :variants [:office-users]}}
; 				 {:schema/variant :assign-tos/office
; 				  :office {:type :ref :variants [:office]}}
; 				 {:schema/variant :assign-tos/price-assignments
; 				  :price-assignments {:type :ref :variants [:price-assignments] :cardinality :has-many}}
; 				 {:schema/variant :assign-tos/note
; 				  :note {:type :ref :variants [:notes]}}
; 				 {:schema/variant :assign-tos/checkin
; 				  :checkin {:type :ref :variants [:checkins]}}]

; 	:price-assignments [{:schema/abstract :price-assignments
; 					    :uuid {:type :uuid :managed true}}
; 					   {:schema/variant :price-assignments/assign-to
; 					     :assign-to {:type :ref :variants [:assign-tos/office :assign-tos/office-user :assign-tos/client :assign-tos/property]}}
; 					   {:schema/variant :price-assignments/part-price
; 					     :part-price {:type :ref :variants [:part-prices] :cardinality :has-many}}]

; 	:part-prices {:schema/variant :part-prices
; 				  :uuid {:type :uuid :managed true}
; 				  :price {:type :ref :variants [:prices]}
; 				  :percent {:type :double}
; 				  :assignment {:type :ref :variants [:price-assignments]}}


; 	:expense-items {:schema/variant :expense-items
; 					:uuid {:type :uuid :managed true}
; 					:assign-to {:type :ref :variants [:assign-tos/office-user :assign-tos/office]}
; 					:creditor {:type :string :index true :label "Creditor"}
; 					:category {:type :string :index true :label "Category"}
; 					:summary {:type :string :label "Summary"}
; 					:price {:type :ref :variants [:prices]}
; 					:notes {:type :ref :variants [:notes]}
; 					:date-time {:type :date-time :index true :label "Date Time"}
; 					:assignments {:type :ref :variants [:price-assignments]}}

; 	:checkins {:schema/variant :checkins
; 			   :uuid {:type :uuid :managed true}
; 			   :property {:type :ref :variants [:property]}
; 			   :client {:type :ref :variants [:clients]}
; 			   :office-user {:type :ref :variants [:office-users]}
; 			   :price-guide {:type :string :label "Price Guide"}
; 			   :rating {:type :int :label "Rating"} ;; range 1 - 10
; 			   :duration {:type :ref :variants [:durations]}
; 			   :referral-source {:type :ref :variants [:tags/leaf]}
; 			   :notes {:type :ref :variants [:notes] :cardinality :has-many}}

; 	:leads {:schema/variant :leads
; 			:uuid {:type :uuid :managed true}
; 			:status {:type :enum :label "Status" :values {:completed "Completed" :pending "Pending" :archived "Archived"}}
; 			:date-time {:type :date-time :index true :label "Date Created"}
; 			:referral-source {:type :ref :variants [:tags/leaf]}
; 			:referral-reference {:type :string :label "Refferal Reference"}
; 			:assign-tos {:type :ref :variants [:assign-tos/office-user] :cardinality :has-many}
; 			:property-url {:type :ref :variants [:urls]} ;; use the label for the property address
; 			:contact {:type :ref :variants [:contacts]}
; 			:client {:type :ref :variants [:clients]}
; 			:notes {:type :ref :variants [:notes] :cardinality :has-many}}

; 	:lead-emails {:schema/variant :lead-emails
; 				  :uuid {:type :uuid :managed true}
; 				  :lead {:type :ref :variants [:leads]}
; 				  :raw-email {:type :ref :variants [:files]}
; 				  :subject {:type :string :label "Subject"}
; 				  :message-id {:type :string :index true}
; 				  :body-hash {:type :string :index true}
; 				  :from {:type :ref :variants [:emails]}
; 				  :tos {:type :ref :variants [:emails] :cardinality :has-many}
; 				  :ccs {:type :ref :variants [:emails] :cardinality :has-many}
; 				  :bccs {:type :ref :variants [:emails] :cardinality :has-many}
; 				  :forwared-tos {:type :ref :variants [:emails] :cardinality :has-many}}

; 	:dna-plans [{:schema/abstract :dna-plans
; 				 :uuid {:type :uuid :managed true}
; 				 :label {:type :string :index true :label "Label"}
; 				 :dna-pack {:type :ref :variants [:dna-plans]}}
; 				 {:schema/variant :dna-plans/client
; 				  :plan-items {:type :ref :variants [:tasks/client] :cardinality :has-many}}
; 				 {:schema/variant :dna-plans/property
; 				  :plan-items {:type :ref :variants [:tasks/property] :cardinality :has-many}}
; 				 {:schema/variant :dna-plans/office-user
; 				  :plan-items {:type :ref :variants [:tasks/office-user] :cardinality :has-many}}
; 				 {:schema/variant :dna-plans/office
; 				  :plan-items {:type :ref :variants [:tasks/office] :cardinality :has-many}}
; 				 {:schema/variant :dna-plans/campaign
; 				  :campaign-items {:type :ref :variants [:campaign-items] :cardinality :has-many}}]

; 	;; published by being referenced by the store
; 	:dna-packs {:schema/variant :dna-packs
; 				:uuid {:type :uuid :managed true}
; 				:label {:type :string :index true :label "Label"}
; 				:summary {:type :string :label "Summary"}
; 				:files {:type :ref :variants [:files] :cardinality :has-many}
; 				:dna-plans {:type :ref :variants [:dna-plans] :cardinality :has-many}}

; 	:tasks [{:schema/abstract :tasks
; 			 :uuid {:type :uuid :managed true}
; 			 :label {:type :string :index true :label "Label"}
; 			 :type {:type :enum :values {:sms "SMS" 
; 			 							 :mms "MMS" 
; 			 							 :email "Email" 
; 			 							 :letter "Letter"
; 			 							 :phone "Phone Call"
; 			 							 :task "Task"}
; 			 			  :index true :label "Type"}
; 			 :template {:type :ref :variants [:templates]}
; 			 :calendar-item {:type :ref :variants [:calendar]}
; 			 :completed-by {:type :ref :variants [:office-users]}}
; 			{:schema/variant :tasks/client
; 			 :assign-to {:type :ref :variants [:assign-tos/client :assign-tos/office-user]}
; 			 :attach-to {:type :ref :variants [:assign-tos/client]}}
; 			{:schema/variant :tasks/property
; 			 :assign-to {:type :ref :variants [:assign-tos/property :assign-tos/office-user]}
; 			 :attach-to {:type :ref :variants [:assign-tos/property]}}
; 			{:schema/variant :tasks/office-user
; 			 :assign-to {:type :ref :variants [:assign-tos/office-user]}}
; 			{:schema/variant :tasks/office
; 			 :assign-to {:type :ref :variants [:assign-tos/office-user :assign-tos/office]}}]

; 	:template-activity {:schema/variant :template-activity
; 						:uuid {:type :uuid :managed true}
; 						:template {:type :ref :variants [:templates]}
; 						:tracking-performance {:type :ref :variants [:tracking-performances] :cardinality :has-many} ;; doubles as sending status
; 						:sender {:type :ref :variants [:assign-tos/office-user :assign-tos/office] :cardinality :has-many}
; 						:attach-tos {:type :ref :variants [:assign-tos/client :assign-tos/property] :cardinality :has-many}
; 						:tos {:type :ref :variants [:emails] :cardinality :has-many}
; 						:ccs {:type :ref :variants [:emails] :cardinality :has-many}
; 						:bccs {:type :ref :variants [:emails] :cardinality :has-many}
; 						:sent-body {:type :ref :variants [:files]}
; 						:sent-files {:type :ref :variants [:files] :cardinality :has-many}
; 						:external-service {:type :ref :variants [:external-services]}}

; 	:agentmail-settings {:schema/variant :agentmail-settings
; 						 :assign-to {:type :ref :variants [:assign-tos/office-user :assign-tos/office]}
;   						 :letterhead {:type :enum :label "Letterhead" :values {:blank "Blank" :letterhead "Letterhead"}}
;   						 :colour {:type :enum :label "Colour" :values {:bw "Black / White" :colour "Colour"}}
;   						 :duplex {:type :enum :label "Duplex" :values {:single "Single Sided" :double "Double Sided"}}
;   						 :return-address {:type :enum :label "Return Address" :values {:blank "No Return Address" :back "Return Address on Envelope Back"}}
;   						 :paper-type {:type :enum :label "Paper Type" :values {:a4-plain "A4 Plain" :a4-semi-gloss "A4 Semi-Gloss"}}
;   						 :delivery {:type :enum :label "Delivery" :values {:standard "Standard Delivery" :priority "Priority Delivery"}}
;   						 :envelope {:type :enum :label "Envelope" :values {:window "Window Envelope" :plain "Plain Envelope"}}
;   						 :special-instructions {:type :string :label "Letterhead"}}

; 	:agentmail-transactions {:schema/variant :agentmail-transactions
; 							 :status {:type :enum :index true :values {:pending "Pending" :sent "Sent" :received "Received" :completed "Completed" :canceled "Canceled"}}
; 							 :office-user {:type :ref :variants [:office-users]}
; 							 :template {:type :ref :variants [:templates.letter]}
; 							 :order-package {:type :ref :variants [:files]} ;; zip containing csv, template, and merged docx
; 							 :num_letters {:type :int} ;; number of recipients
; 							 :pages_per_letter {:type :int}
; 							 :price {:type :ref :variants [:prices]}
; 							 :discount {:type :ref :variants [:prices]}
; 							 :tx-time {:type :int} ;; potentially basis ref
; 							 :agentmail-setting {:type :ref :variants [:agentmail-settings]}}

; 	:message-channels {:schema/variant :message-channels
; 					   :uuid {:type :uuid :managed true}
; 					   :label {:type :string :index true :label "Label"}
; 					   :owner {:type :ref :variants [:users]}
; 					   :members {:type :ref :variants [:users] :cardinality :has-many}}

; 	:messages {:schema/variant :messages
; 			   :uuid {:type :uuid :managed true}
; 			   :owner {:type :ref :variants [:users]}
; 			   :channel {:type :ref :variants [:message-channels]}
; 			   :seen {:type :ref :variants [:users] :cardinality :has-many}
; 			   :body {:type :string-large}}

; 	:procedures {:schema/variant :procedures
; 				 :uuid {:type :uuid :managed true}
; 				 :label {:type :string :index true :label "Label"}
; 				 :summary {:type :string :label "Summary"}
; 				 :type {:type :enum :values {:administration "Administration"
; 				 							 :standards "Standards"
; 				 							 :form "Form"
; 				 							 :legal "Legal"
; 				 							 :listing "Listing"
; 				 							 :negotiation "Negotiation"
; 				 							 :checkin "Checkin / Open Home"
; 				 							 :sale "Sale"
; 				 							 :settlement "Settlement"
; 				 							 :team-law "Team Law"
; 				 							 :training "Training"
; 				 							 :misc "Misc"}
; 				 		 :label "Type"} 
; 				 :files {:type :ref :variants [:files] :cardinality :has-many}}

; 	:tfrs {:schema/variant :tfrs
; 		   :uuid {:type :uuid :managed true}
; 		   :type {:type :enum :index true :label "Type" :values {:week "Week" :fortnight "Fortnight" :month "Month" :year "Year"}}
; 		   :assign-to {:type :ref :variants [:assign-tos/office :assign-tos/office-user]}
; 		   :appraisals {:type :int :label "Appraisal"}
; 		   :listings {:type :int :label "Listings"}
; 		   :sales {:type :int :label "Sales"}
; 		   :checkins {:type :int :label "Checkins"}
; 		   :commission {:type :ref :variants [:prices]}
; 		   :vpa {:type :ref :variants [:prices]}
; 		   :settled {:type :int :label "Settled"}
; 		   :days-on-market {:type :int :label "Days on Market"}
; 		   :clearence-level {:type :int :label "Clearence Level"}}
; })


;; Has to deal with has-many, enum

; (def lovii-validation-schema
;   nil)

; (alter-var-root #'lovii-validation-schema (constantly (v/schemas->validators #'lovii-validation-schema (loschema/parse-schema lovii-schema))))


; (s/validate lovii-validation-schema 
;             {:schema/variant :auth-credentials
;              :auth-credentials/uuid #uuid "297d5fb9-13b9-47f9-a4d6-30ce7cdcec19",
;              :auth-credentials/username "everblaze",
;              :auth-credentials/password "password",
;              :auth-credentials/two-factor false})

; (def parse-comment-request
;   (coerce/coercer lovii-validation-schema coerce/json-coercion-matcher))

; (parse-comment-request {"schema/variant" "auth-credentials",
;                         "auth-credentials/uuid" "297d5fb9-13b9-47f9-a4d6-30ce7cdcec19",
;                         "auth-credentials/username" "everblaze",
;                         "auth-credentials/password" "password",
;                         "auth-credentials/two-factor" false})
