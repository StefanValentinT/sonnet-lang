#import "spec_template.typ": *

= Definitions

The following definitions apply to the specification. Other terms are defined when they first occur or in a more appropriate section; when a term is defined it is being written in _italic_ type.

- Observable *behaviour* is all action that influences the world. Observable influence may also be called physical. Non-observable behaviour does not influence the world and may be assumed to not exist in a program.

- *Bit* is a unit of data storage capable of holding one of two values, commonly referred to as ${0, 1}.$ Individual bits do not need to be addressable.

- A *byte* is the smallest unit of bits that is addressable.
	#note[A byte commonly consists of 8 bits though a different length can be used for a conforming implementation.]

- *Binary data* is the representation of data using bits, consisting of one or more bytes.

- A *character* is a member of a set of characters that make up the textual representation of a Sonnet program.

- An *implementation-defined* value or behaviour is one left unspecified by the specification where each implementation must document its choice. A subset of this is *locale-defined* where rather than the implementation the circumstances of its invocation define certain behaviours. 
	#example[The behaviour of functions such as `is_lower` may depend on the language a user has set for their computer.]

- An *ill-formed* state is a state entered upon evaluating an ill-formed construct or data. This specification imposes no requirements on a program that enters an ill-formed state. Reaching an ill-formed state completely invalidates the semantics of the entire program execution; no statements are made regarding any behaviour, including behaviour prior to the point at which the ill-formed state was reached. A construct or data is ill-formed if its evaluation violates the semantic rules of this specification.

- *Unspecified behaviour* is behaviour for which the specification imposes no requirements, allowing multiple valid outcomes. After the unspecified behaviour, program execution continues normally. Unspecified behaviour is defined by explicitly declaring it as such or by the omission of any explicit definition of behaviour.
	#example[The evaluation order of + is unspecified; thus
	```
	print("Hello ") + print("world!")
	``` may produce either "Hello World!" or "world!Hello ".]

= Conventions

- The term _value set_ is used to refer to the set of representable values for some type.
