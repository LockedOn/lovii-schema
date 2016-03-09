# LOVII Schema

[![Build Status](https://travis-ci.org/LockedOn/lovii-schema.svg?branch=master)](https://travis-ci.org/LockedOn/lovii-schema)

Describe & design your application using data. Version control your data structures.
A fast data driven way to design complex entity relationships for graph databases. Designed to keep as much relevant data together..

### Example EDN

```edn
{
	:users {:schema/variant :users
			:uuid {:type :uuid :unique :identity :required true}
			:name {:type :string :label "Full Name" }
			:tags {:type :ref :variants [:tags/set] :cardinality :one}
			:social {:type :ref :variants [:social] :cardinality :has-many}}

	:social {:schema/variant :social
		     :uuid {:type :uuid :unique :identity :required true}
		     :service {:type :enum :values {:twitter "Twitter"
	  								      :google  "Google"
	  								      :facebook "Facebook" 
	  								      :linkedin "LinkedIn"
	  								      :icq "ICQ"
	  								      :myspace "MySpace"}
					  		     :label "Social Service"}
		     :account {:type :string :index true :label "Social Account"}}

	:tags [{:schema/abstract :tags
			:uuid {:type :uuid :unique :identity :required true}
			:label {:type :string :required false 
								  :min 0 
								  :max 60 
								  :index true 
								  :label "Label"}
			:inactive {:type :boolean :required false :index true :default false}}
		   {:schema/variant :tags/set
			:tags {:type :ref :required false 
							  :cardinality :has-many 
							  :variants [:tags/leaf]}}
		   {:schema/variant :tags/leaf
			:tag {:type :string :required true :index true}}]
}
```

### Schema Format

The whole structure is defined in a nested map. Each node itself is a map, additional information is preserved through transformation where possible. Allowing the use of custom transformers which could be used for a number of operation e.g. UI field generator, custom validation, transform to different data formats etc.

The first level shows all entity namespaces available in your structure. Each entity namespace contains a map of attributes, these will get expanded with the entity namespace unless the keyword has already been namespaced e.g. :schema/variant or :schema/abstract. See the expanded version below for an example of how the namespacing works.

All datomic types are supported without the namespace and we have mapped some of them to the following list which make the schema easier to reason.

* uuid
* enum -> ref (db.ident)
* ref
* string
* int -> long
* long
* float
* double
* decimal -> bigdec
* boolean
* date-time -> instant
* date -> instant
* instant

### Expanded Version

Calling `(lovii-schema.schema/parse-schema schema-edn)` will produce the following given schema-edn example above.

```edn
[{:schema/abstract :users,
  :schema/variant :users,
  :users/uuid {:type :uuid, :unique :identity, :required true, :cardinality :one},
  :users/name {:type :string, :label "Full Name", :cardinality :one},
  :users/tags {:type :ref, :variants [:tags/set], :cardinality :one},
  :users/social {:type :ref, :variants [:social], :cardinality :has-many}}
 {:schema/abstract :social,
  :schema/variant :social,
  :social/uuid {:type :uuid, :unique :identity, :required true, :cardinality :one},
  :social/service {:type :enum, :label "Social Service", :cardinality :one
				   :values {:social/twitter "Twitter",
				    		:social/google "Google",
				    		:social/facebook "Facebook",
				    		:social/linkedin "LinkedIn",
				    		:social/icq "ICQ",
				    		:social/myspace "MySpace"}},
  :social/account {:type :string, :index true, :label "Social Account", :cardinality :one}}
 {:schema/abstract :tags,
  :schema/variant :tags/set,
  :tags/uuid {:type :uuid, :unique :identity, :required true, :cardinality :one},
  :tags/label {:type :string, :required false, :min 0, :max 60, :index true, :label "Label", :cardinality :one},
  :tags/inactive {:type :boolean, :required false, :index true, :default false, :cardinality :one},
  :tags/tags {:type :ref, :required false, :cardinality :has-many, :variants [:tags/leaf]}}
 {:schema/abstract :tags,
  :schema/variant :tags/leaf,
  :tags/uuid {:type :uuid, :unique :identity, :required true, :cardinality :one},
  :tags/label {:type :string, :required false, :min 0, :max 60, :index true, :label "Label", :cardinality :one},
  :tags/inactive {:type :boolean, :required false, :index true, :default false, :cardinality :one},
  :tags/tag {:type :string, :required true, :index true, :cardinality :one}}]
```

This expanded version above shows what the schema looks like before it's sent to the transformers.
 
`lovii-schema.schema/parse-schema` is a many arity function allowing you to extend the default transformation with custom types.

```clojure
(require 'lovii-schema.schema)

(defn email-type
	[desc]
	(if (= (:type desc) :email)
		(merge desc {:type :string :regex "emailsregex"})
		desc))

(lovii-schema.schema/parse-schema {:testns {:schema/variant :testns 
											:email {:type :email :required true}}} 
								  email-type)

#_=> [{:schema/abstract :testns
       :schema/variant :testns
       :testns/email {:type :string :regex "emailsregex" :required true :cardinality :one}}]
```

*Notes* 

* All abstracts will be removed and merged into the variants
* Cardinality :one will be added to any field that dosnt have :has-many.
* Enum values will also be namespaced unless they already have one.

### Enums

```edn
{:social {:uuid {:type :uuid :unique :identity :required true}
		 :service {:type :enum :values {:twitter "Twitter"
	  								  :google  "Google"
	  								  :facebook "Facebook" 
	  								  :linkedin "LinkedIn"
	  								  :icq "ICQ"
	  								  :myspace "MySpace"}
					  		 :label "Social Service"}
		 :account {:type :string :index true :label "Social Account"}}}
```

Enum is a special type we have created which allows you to set keywords that can be referenced. The above example shows the values in a map, where the key is what is stored and the value is the display label. This allows you to generate ui elements directly from the schema output.

### Cardinality & Indexes

There are two options for cardinatily :has-many or :one. Has many allowing multiple values to be stored for this field.

Indexing required the following :index true. This will allow the schema transformer to apply indexes to the field.

### Validation

```edn
:label {:type :string :required false 
					  :min 0 
					  :max 60 
					  :index true 
					  :label "Label"}
```
Each field can have multiple validation applied to it, from the above example you can see the use of :required, :min & :max. The :required will allow data validators to ensure data is available for this fields when upserting.

Min & Max let you limit the number of characters allowed for this field. This will also take the field type into consideration so if the field is an int then the min and max with look for valid integers in that range.

Regex expressions can be applied to the data which will bring more advanced data validation e.g. valid email format

### Abstract & Variants 
```edn
:tags [{:schema/abstract :tags
		:uuid {:type :uuid :unique :identity :required true}
		:label {:type :string :required false 
							  :min 0 
							  :max 60 
							  :index true 
							  :label "Label"}
		:inactive {:type :boolean :required false :index true :default false}}
	   {:schema/variant :tags/set
		:tags {:type :ref :required false 
						  :cardinality :has-many 
						  :variants [:tags/leaf]}}
	   {:schema/variant :tags/leaf
		:tag {:type :string :required true :index true}}]
```
Variants allow you to specify fields that will only be available when referenced from another field requiring those specific fields. 

Using an abstract concept, saves you from writing out the complete set for each variant. This allows you to store the common fields in the abstract which gets merged for each variant. If we look at the above example for :tags/set and :tags/leaf they will both have :uuid, :label and :inactive.

### Transformers

The following are the included transformers, this list will be expanded in the future look at the roadmap for example of what's coming up next.

*Datomic Schema*

>Takes the lovii schema and transforms into vaild datomic schema.

### Roadmap

*Validate Schema With Rules*

* All attributes of the same name (different variants) must be the same type.

*Validate Data against the schema constraints*

* Plugable validation functions
* Refs enforce variant
* Enforced required rules
    

### License

MIT
