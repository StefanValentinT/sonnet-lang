#import "spec_template.typ": *

= Introduction

Sonnet ought to be beautiful in its simplicity, pristine in its architecture, bright-shining as the ivory tower, with a long spiral staircase winding its way all the way to the top, twisting round and round the tower. Sonnet shall be there for everyone, shall be easy to use because of its simplictly, but limitless in its reach. Sonnet shall bring back the simple, arcane joy of programming.


Therefore, the specification is written in a way to facilitate both unambiguity and accessibility for everyone. Whenever these goals conflicted, we opted to add important information using a #smallcaps("Note"), or, where needed, to provide additional reiteration or exemplification marked with an #smallcaps("Example"), whilst not compromising the precise actual specification. Both shall be regarded as non-authoritative for the meaning of the language and may only aid in its understanding.

== Scope

We first detail the actual representation of Sonnet programs, before specifying their syntactical structure. Thereafter the constraints and requirements enacted upon the maening of a program are detailed and its evaluation via the reduction of terms. The means by which a program may interface with the surrounding world are described and lastly a collection of definitions is given that a conforming implementation shall provide as minimal, opaque "atoms" of computation.


== Limitations

No bound is given for the complexity of a program that may exceed the capabilities of a particular evaluator, compiler or processor. Although such limits seem to be necessary in practice, especially for data, we also do not demand minimal abilities of a system to not constrain the language by artificial limits.

=== Optimization
The means by which a conforming evaluator is invoked or the way it evaluates a given program are left unspecified to allow for greater freedom of optimization. Only directly observable behaviour of the language itself shall be specified.

= Conformance & Specification

The following definitions of verbs shall only apply within the context of a constraint on the implementation.

The use of "shall" denotes a mandatory requirement on an implementation, while "shall not" and "may not" declare a strict prohibition.

"May" is used to specify that some behaviour is conformant to the specification, noticeably it is not exclusive, signifying multiple interpretations may be within the bounds of conformance. "Need not" is used to explicitly denote the non-requiredness of a particular property.

A "conforming" implementation is any one that abides to all requirements laid out by this specification, including those requiring the rejection of invalid or erroneous programs. Similarily, a conforming program is one not rejected by a conforming implementation.

A implementation shall be accompanied by a clear definition of the behaviour in all cases of implementation-defined behaviour in this specification, for all cases where the characteristics do not match the recommendations of this specifications non-authorative text.
