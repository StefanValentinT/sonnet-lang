#import "spec_template.typ": *

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
