#import "spec_template.typ": *
#import "@preview/tdtr:0.5.5" : *

= Syntax

== Textual representation

The representation form of Sonnet programs is text; a program is a sequence of characters.
The set of characters must at least suffice the following requirements, being able to encode all listed characters:

- all the letters from the Latin alphabet, those being:
#grid(
	columns: (1fr,) * 13,
	row-gutter: 1em,
	align: center,
	[A], [B], [C], [D], [E], [F], [G], [H], [I], [J], [K], [L], [M],
	[N], [O], [P], [Q], [R], [S], [T], [U], [V], [W], [X], [Y], [Z]
)

- and their lowercase equivalents:
#grid(
	columns: (1fr,) * 13,
	row-gutter: 1em,
	align: center,
	[a], [b], [c], [d], [e], [f], [g], [h], [i], [j], [k], [l], [m],
	[n], [o], [p], [q], [r], [s], [t], [u], [v], [w], [x], [y], [z]
)
- all arabic digits:
#grid(
	columns: (1fr,) * 10,
	row-gutter: 1em,
	align: center,
	[0], [1], [2], [3], [4], [5], [6], [7], [8], [9],
)
- as well as the following set of special characters ("␣" denotes the invisible space character):
#grid(
	columns: (1fr,) * 13,
	row-gutter: 1em,
	align: center,
	[+], [-], [\*], [/], [<], [>], [=], [!], [?], [&], [|], [%], [\$], [.],
	[:], [;], [,], [(], [)],[\[], [\]], [{], [}], [\~], [\_], [␣]
)

The encoding of a program except for all string literals it might contain, is implementation-defined.

#note[At the time of writing (#today.year()) it seems best to encode the whole file in UTF-8 unless special domain constraints would apply.]

Implementations may allow to substitute Unicode signs such as "→" for their normal counterparts (e. g. "->").

String literals shall be represented as text encoded in the UTF-8 format.

Additonally a character or sequence of character shall exist, to encode a line break; in the following chapters this will be represented by the sequence "\\n".

== Definition of the syntax notation

A terminal is a sequence of characters, a non-terminal is a variable, with at least one expansion. An item is either a terminal or a non-terminal. Whitespace is insignificant and not a terminal.

Terminals are written in *bold* type, whilst non-terminals are written in normal type.

A production rule is defined by a non-terminal on the left followed by an arrow and a expansion on the right. It may be read as "non-terminal _x_ may expand into _expansion_".

Optional items are enclosed within braces. If the brace pair is prepended with a "+" the enclosed item shall appear at least once, with no upper bound on the amount of repetitions, if prepended with a "\*" the item may appear zero or more times.

Alternation is defined using the vertical bar as in " _a_ | _b_"), which reads "expand into either _a_ or _b_". Similarily, "one of" expands into out of a list of terminals.

The expansion process of production rules terminates successfully if and only if the input sequence is syntactically correct. Upon termination, no further production rules can be applied, and the resulting string consists exclusively of terminal symbols.

== The Syntax
/*
#prod([identifier], [first_char \*{identifier_char}], [more])
#prod([letter], [*one of:* #box(grid(
	columns: (auto,) * 18,
	row-gutter: 1em,
	align: center,
	[A], [B], [C], [D], [E], [F], [G], [H], [I], [J], [K], [L], [M],
	[N], [O], [P], [Q], [R], [S], [T], [U], [V], [W], [X], [Y], [Z],
	[a], [b], [c], [d], [e], [f], [g], [h], [i], [j], [k], [l], [m],
	[n], [o], [p], [q], [r], [s], [t], [u], [v], [w], [x], [y], [z]
))])

#prod([digit], [*one of:* #box(grid(
	columns: (auto,) * 10,
	row-gutter: 1em,
	align: center,
	[0], [1], [2], [3], [4], [5], [6], [7], [8], [9],
))])

#prod([extended\_char], [letter],[*one of:* #box(grid(
	columns: (auto,) * 10,
	row-gutter: 1em,
	align: center,
	[\_], [.], [!], [?]

))])
*/

= Abstract Syntax <abstract_types>

The abstract syntax describes the structure of a Sonnet program independent of its textual
representation.
It is presented in a similiar fashion as inductive datatypes in most programming languages.
This naturally maps to their modelling within Lean in @concrete.
In the grammar, $italic(a)^*$ denotes that $italic(a)$ may appear once or multiple times. $x$ represents an identifier. The set of all possible expansions of a non-terminal $x$ is written as $epsilon(x)$.


#let desc(x) = box(width: 5cm, align(right, [(#x)]))

#place(
	auto,
	scope: "parent",
	float: true,
	[#figure(
		$
		mat(delim: #none,
			iota, :=, "i8" divides "i16" divides "i32" divides "i64", desc("signed integer types") ;,
			divides, "u8" divides "u16" divides "u32" divides "u64", desc("unsigned integer types") ;,
			divides, "f16" divides "f32" divides "f64", desc("floating-point types");;

			tau, :=, iota, desc("primitive types") ;,
			divides, alpha, desc("type variable");,
			divides, forall alpha . tau, desc("quantified type");,
			divides, tau and tau, desc("intersection");,
			divides, tau^* -> tau, desc("function type") ;,
			divides, *tau, desc("pointer type") ;,
			divides, [tau, n], desc("array type") ;,
			divides, "struct"(phi), desc("anonymous struct type") ;,
			divides, "union"(phi), desc("anonymous union type") ;;
			phi, :=, (x, tau)^*
		) $,

		caption: [The abstract syntax of types in Sonnet.]
	)<abstract_syntax_types>]
) 

== Types

The abstract syntax for types is given in @abstract_syntax_types.
The set of types of some rank $n$ $T_n$ is a subset of the types derived by expansion of $tau$ as defined in this grammar.

=== Rank
A type is defined to be of rank $n$ with respect to a specific syntactic construct if every path from the root of the type tree to that construct traverses the left branch of fewer than $n$ function type constructors, as @examples_rank illustrates.

#let tree = tidy-tree-graph.with(
	draw-node: ((label,)) => (stroke: none, label: $label$),
	draw-edge: (stroke: .5pt, marks: "-"),
	spacing: (10pt, 10pt),
	node-width: auto,
	node-height: auto,
)

#figure(
	grid(
	columns: 2,
	column-gutter: 2em,
	tree[
	- $->$
		- $and$
			- $a$
			- $->$
				- $b$
				- $c$
		- $and$
			- $d$
			- $e$
	],
	tree[
	- $->$
		- $->$
			- $and$
				- $a$
				- $b$
			- $c$

		- $and$
			- $d$
			- $e$
	]
	),
	caption: [Rank-2 intersection type on the left and a non-rank-2 type on the right.]
) <examples_rank>

Types of rank 0 are also called "simple types". $T_2$, the set of rank-2 intersection types, is defined inductively:

$ T_0^(0) = epsilon(alpha) = T_1^(0) = T_2^(0) $

For each natural number $i$, the sets $T_0^(i+1)$ and $T_1^(i+1)$ and $T_2^(i+1)$ are defined as follows:

$ T_0^(i+1) = T_0^(0) union {(sigma -> tau) | sigma, tau in T_0^(i)} $

$ T_1^(i+1) = T_0^(i+1) union {(sigma and tau) | sigma, tau in T_1^(i)} $

$ T_2^(i+1) = T_0^(i+1) union {(sigma -> tau) | sigma in T_1^(i), tau in T_2^(i)} $

Finally, let the complete sets of types be:

$ T_2 = union.big_i T_2^(i) $
$ T = T_2 union {(forall t. sigma) | sigma in T, t in epsilon(alpha)} $

== Terms

#place(
	auto,
	scope: "parent",
	float: true,
	[#figure(
		$
		mat(delim: #none,
			eta, :=, x divides eta : tau;;

			t, :=, x, desc("variable");,
			divides, (n, iota), #desc([typed literals, $n in QQ$]) ;,
			divides, t : tau, desc("annotation");,
			divides, eta^* -> t, desc("function terms") ;,
			divides, x(t^*), desc("function application");,
			divides, {t^*}, desc("data literal");,
			divides, "if" t "then" t "else" t, desc("conditional branching");,
			divides, "while" t "do" t, desc("loop");,
			divides, "return" t, desc("function exit");,
			divides, "block" t^*, desc("")
		) $,

		caption: [The abstract syntax of terms.]
	)<abstract_syntax_terms>]
)

The abstract syntax for terms is given in @abstract_syntax_terms.
A term is any construct that can be derived by expansion of $t$ as defined in this grammar.


= Typing

As a consequence of Rice's Theorem the type system can only be a syntactic mechanism to approximate a programs semantics @Rice1953. Thus it has been designed to allow the programmar great freedom in the way a program is written within the bounds of making both type checking and type inference sound and decidable. Since it has been shown that these restrictions severly limit the expressivness of a type system, excluding 


