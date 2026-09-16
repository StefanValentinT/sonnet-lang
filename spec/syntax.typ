#import "spec_template.typ": *
#import "@preview/tdtr:0.5.5": *
#import "@preview/curryst:0.6.0": prooftree, rule, rule-set

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
  [N], [O], [P], [Q], [R], [S], [T], [U], [V], [W], [X], [Y], [Z],
)

- and their lowercase equivalents:
#grid(
  columns: (1fr,) * 13,
  row-gutter: 1em,
  align: center,
  [a], [b], [c], [d], [e], [f], [g], [h], [i], [j], [k], [l], [m],
  [n], [o], [p], [q], [r], [s], [t], [u], [v], [w], [x], [y], [z],
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
  [+], [-], [\*], [/], [<], [>], [=], [!], [?], [&], [|], [%], [\$],
  [.], [:], [;], [,], [(], [)], [\[], [\]], [{], [}], [\~], [\_], [␣],
)

The encoding of a program except for all string literals it might contain, is implementation-defined.

#note[At the time of writing (#today.year()) it seems best to encode the whole file in UTF-8 unless special domain constraints would apply.]

Implementations may allow to substitute Unicode signs such as "→" for their normal counterparts (e. g. "->").

String literals shall be represented as text encoded in the UTF-8 format.

Additonally a character or sequence of character shall exist, to encode a line break; in the following section this will be represented by the sequence "\\n".

== Definition of the syntax notation

A terminal is a sequence of characters, appearing verbatim in the expansion,
a non-terminal is a variable in the grammar, with one expansion.
An item is either a terminal or a non-terminal.
Whitespace is insignificant and not a terminal.

A production rule is
defined by a non-terminal on the left followed by an arrow and an expansion on the right.
An expansion is a non-empty list of terminals and non-terminals.
It may be read as "non-terminal _x_ may expand into _expansion_".

The expansion process of production rules terminates successfully if and only if the input sequence is syntactically correct.
Upon termination, no further production rules can be applied, and the resulting string consists exclusively of terminal symbols.

The following conventions apply to the presentation:

#table(
  columns: 2,
  align: (left, left),
  stroke: none,
  [#terminal("terminal")],
  [literal text in the expansion is set in typewriter font],

  [#terminal("a") $divides$ #terminal("b")],
  [*Alternation*: may expand into either #terminal("a") or #terminal("b")],

  [#rept(terminal("repetitive"))],
  [*Repetition*: may appear zero or more times in the expansion],

  [#opt(terminal("item"))],
  [*Optionality*: #terminal("item") may or may not appear in the expansion],
  [$()$], [Parentheses are used for grouping.]
)

== Identifiers & Keywords

== Numeric constants
#table(
	columns: 3,
	align: (left, center, left),
	stroke: none,
	$nonterminaldef("digit")$, $->$, 
	$#terminal[0]
	divides #terminal[1]
	divides #terminal[2]
	divides #terminal[3]
	divides #terminal[4]
	divides #terminal[5]
	divides #terminal[6]
	divides #terminal[7]
	divides #terminal[8]
	divides #terminal[9]$,
	$nonterminaldef("hex-digit")$, $->$, 
	$#nonterminal("digit") 
	divides #terminal[A] 
	divides #terminal[B]
	divides #terminal[C] 
	divides #terminal[D] 
	divides #terminal[E]
	divides #terminal[F]
	divides #terminal[a] 
	divides #terminal[b]
	divides #terminal[c] 
	divides #terminal[d] 
	divides #terminal[e]
	divides #terminal[f]$,
	$nonterminaldef("octal-digit")$, $->$, $#terminal[0]
	divides #terminal[1]
	divides #terminal[2]
	divides #terminal[3]
	divides #terminal[4]
	divides #terminal[5]
	divides #terminal[6]
	divides #terminal[7]$,
	$#nonterminaldef("digits")$, $->$, $#nonterminal("digit")$, "", $divides$, $#nonterminal("digit") #nonterminal("digits")$,
	$#nonterminaldef("hex-digits")$, $->$, $#nonterminal("hex-digit")$, "", $divides$, $#nonterminal("hex-digit") #nonterminal("hex-digits")$,
	$#nonterminaldef("octal-digits")$, $->$, $#nonterminal("octal-digit")$, "", $divides$, $#nonterminal("octal-digit") #nonterminal("octal-digits")$,
	$#nonterminaldef("number")$, $->$, $#nonterminal("digit")_*$, "", $divides$, $( #terminal("0x") divides  #terminal("0X") ) #nonterminal("hex-digit")_*$,
	"", $divides$, $( #terminal("0o") divides  #terminal("0O") ) #nonterminal("octal-digit")_*$

										
)


= Abstract Syntax <abstract_types>

The abstract syntax describes the structure of a Sonnet program independent of its textual
representation.
It is presented in a similiar fashion as the lexical syntax in the preceeding chapters.

#let desc(x) = box(width: 5cm, align(right, [(#x)]))

#place(
  auto,
  scope: "parent",
  float: true,
  [#figure(
    $
      mat(
        delim: #none,
        iota, :=, "i8" divides "i16" divides "i32" divides "i64", desc("signed integer types"); ,
        divides, "u8" divides "u16" divides "u32" divides "u64", desc("unsigned integer types"); ,
        divides, "f16" divides "f32" divides "f64", desc("floating-point types"); ,
        divides, "bool",
        ; ;
        tau, :=, iota, desc("primitive types"); ,
        divides, alpha, desc("type variable"); ,
        divides, forall alpha . tau, desc("universally quantified type"); ,
        divides, tau and tau, desc("intersection"); ,
        divides, tau^* -> tau, desc("function type"); ,
        divides, *tau, desc("pointer type"); ,
        divides, [tau], desc("array type"); ,
      )
    $,

    caption: [The abstract syntax of types in Sonnet.],
  )<abstract_syntax_types>],
)

== Types

The abstract syntax for types is given in @abstract_syntax_types.
The set of types of some rank $n$ $T_n$ is a subset of the types derived by expansion of $tau$ as defined in this grammar.

=== Notation
Let $"FTV"(tau)$ denote the set of free type variables (those not bound by a $forall$ quantifier)
occurring in the type $tau$ and $"FTV"(A)$ denote the set of free type variables occurring in the codomain of $A$.
The function type constructor $->$ associates to the right and binds weaker than all other type constructors,
both $t_1 dots t_n -> sigma$ and $forall t.sigma$ extend rightward to the end of the expression within they occur,
meaning $t -> t -> t and t$ is equal to $t ->(t -> (t and t))$ and $forall t. t and t$ is equal to $forall t. (t and t)$.

=== Rank
A type is of rank $n$ with respect to a specific syntactic construct
if every path from the root of the type tree to that construct
traverses the left branch of fewer than $n$ function type constructors.

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
        - $->$
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
    ],
  ),
  caption: [A rank-2 intersection type on the left and a non-rank-2 type on the right.],
) <examples_rank>

Types of rank 0 are also called "simple types". $T_2$, the set of rank-2 intersection types, is defined inductively:

$ T_0^(0) = epsilon(alpha) union epsilon(iota) = T_1^(0) = T_2^(0) $

For each natural number $i$, the sets $T_0^(i+1)$ and $T_1^(i+1)$ and $T_2^(i+1)$ are defined as follows:

$
  T_0^(i+1) = T_0^(0) union {*rho | rho in T_0^i} \ {(sigma_1 dots sigma_n -> tau)
    | forall j. sigma_j in T_0^(i), tau in T_0^(i)}
$

$
  T_1^(i+1) = T_0^(i+1) union {(sigma and tau)
    | sigma, tau in T_1^(i)}
$

$
  T_2^(i+1) = T_0^(i+1) union {(sigma_1 dots sigma_n -> tau)
    | forall j. sigma_j in T_1^(i) , tau in T_2^(i)}
$

$T_n$ is the union of all rank-n construction stages.

$
  T_0 = union.big_i T_0^(i) wide T_1 = union.big_i T_1^(i) wide T_2 = union.big_i T_2^(i)
$

Finally, let the complete set of types be $T$:

$ T = T_2 union {(forall t. sigma) | sigma in T, t in epsilon(alpha)} $

As is evident from the definition, quantification can only appear at the top-level of a type.

Since $and$ is commutative and idempotent ($alpha and alpha = alpha$) any rank-1 type may also be written as the intersection over a non-empty, finite set of simple types: $and.big S "where" S subset.eq T_0 "and" S != emptyset$.

=== Subtyping
A type $rho$ is a subtype of another type $sigma$ (written $rho lt.eq_s sigma$) if and only if it can be derived by the following inductive structural rules:

- *Reflexivity*: For any simple type $tau in T_0$, $tau <: tau$.
- *Intersection Elimination*: For any types $sigma, tau in T_1$, $(sigma and tau) lt.eq_s sigma$ and $(sigma and tau) lt.eq_s tau$.
- *Intersection Introduction*: If $rho lt.eq_s sigma$ and $rho lt.eq_s tau$, then $rho lt.eq_s (sigma and tau)$.

Given $(sigma_1 dots sigma_n -> tau) in T_2$ and $(sigma'_1 dots sigma'_n -> tau') in T_2$:
$(sigma_1 dots sigma_n -> tau) lt.eq_s (sigma'_1 dots sigma'_n -> tau')$
if and only if $forall j. sigma'_j lt.eq_s sigma_j$ (for $sigma_j, sigma'_j in T_1$) and $tau lt.eq_s tau'$ (for $tau, tau' in T_2$).

A universally quantified type is a subtype of its instances.
$ (forall t. sigma) lt.eq_s sigma [x := rho] $
where $rho in T_0$ is a simple type, and $sigma [x := rho]$ denotes the capture-avoiding substitution of $x$ with $rho$ in $sigma$.
- *Generalization*: If $sigma lt.eq_s tau$ and $t in/ "FTV"(sigma)$, then:
  $sigma lt.eq_s (forall t. tau)$

- *Transitivity*: If $rho lt.eq_s sigma$ and $sigma lt.eq_s tau$, then $rho lt.eq_s tau$.

If and only if $tau lt.eq^s sigma$ and $sigma lt.eq^s tau$ both types are identical, written as $sigma equiv tau$.

#example[
  It is shown that the rank-2 type from @examples_rank is in fact contained within $T$:

  By definition, all type variables belong to $T_0^(0)$.

  Using the (reduced) definition for $T_0^(i+1)$:

  $ T_0^1 = T_0^(0) union {(sigma -> tau) | sigma, tau in T_0^0} $

  From $b, c, d, e in T_0^(0)$,
  it follows that $(b -> c) in T_0^(1)$ and $(d -> e) in T_0^(1)$.

  Using the definition of $T_1^2$:
  $ T_1^2 = T_0^2 union {(sigma and tau) | sigma, tau in T_1^1} $

  Since $a in T_0^0 subset.eq T_1^0$ and $(b -> c) in T_0^1 subset.eq T_1^1$, it follows that
  $(a and (b -> c)) in T_1^2$.

  Using the definition for $T_2^3$:

  $ T_2^3 = T_0^3 union {(sigma -> tau) | sigma in T_1^2, tau in T_2^2} $

  Because $T_0^1 subset.eq T_0^2 subset.eq T_2^2$, the previously established $(d -> e) in T_0^1$ means that $(d -> e) in T_2^2$.

  It has also been established that $(a and (b -> c)) in T_1^2$.

  Therefore, by choosing these elements for $sigma$ and $tau$, the type is obtained:
  $ ((a and (b -> c)) -> (d -> e)) in T_2^3 $

  Since $T_2^3 subset.eq T_2$ by definition of $T_2$ and $T_2 subset.eq T$, it has been shown that the type is contained within $T$.
]

== Terms

#place(
  auto,
  scope: "parent",
  float: true,
  [#figure(
    $
      mat(
        delim: #none,
        eta, :=, x divides eta : tau; ;
        t, :=, x, desc("variable"); ,
        divides, (n, iota), #desc([typed literals, $n in QQ$]) ; ,
        divides, t : tau, desc("annotation"); ,
        divides, lambda (eta^*).t, desc("function terms"); ,
        divides, x(t^*), desc("function application"); ,
        divides, {t^*}, desc("data literal"); ,
        divides, "if" t "then" t "else" t, desc("conditional branching"); ,
        divides, "while" t "do" t, desc("loop"); ,
        divides, "return" t, desc("function exit"); ,
        divides, "block"(t^*), desc("")
      )
    $,

    caption: [The abstract syntax of terms.],
  )<abstract_syntax_terms>],
)

The abstract syntax for terms is given in @abstract_syntax_terms.
A term is any construct that can be derived by expansion of $t$ as defined in this grammar.


= Typing

As a consequence of Rice's Theorem the type system can only be a syntactic mechanism to approximate a programs semantics @Rice1953. Thus it has been designed to allow the programmar great freedom in the way a program is written within the bounds of making both type checking and type inference sound and decidable.


A type environment is a relation between a set of variables and a set of types, defined as a finite set of distinct variable-type pairs ${x_1 : sigma_1, dots, x_n : sigma_n}$ that tracks the types of variables currently in scope. $A$ is used as a meta-variable ranging over arbitrary type environments. The domain of a type environment, written $"dom"(A)$, is the set of all variable names present in the relation being ${x divides exists sigma. (x : sigma) in A}$. Conversely, the codomain, written $"codom"(A)$, is the set of all types in $A$. For any variable $x in "dom"(A)$, $A(x) = sigma$ is defined such that $(x : sigma) in A$.

All terms have a type, written as $t : tau$. Typing is performed relative to an environment (or context) $Gamma$, and typing judgements are written: $ Gamma tack.r e : tau $ which reads as "$e$ has type $tau$ in the current context". $Gamma$ contains the current typing assumptions under which typing is performed.




The typing rules are given in @typing_rules. $I$ denotes a finite index set, used to parametrize finite collections of types. The types used in the position of premises are of rank 1, whereas those derived are of rank 2.


#place(
  auto,
  scope: "parent",
  float: true,
  [#figure(
      gap: 2em,
      align(center, grid(
        columns: (auto, auto),
        row-gutter: 2em,
        align: (left, left),
        column-gutter: 2em,
        [(Var)],
        prooftree(vertical-spacing: 0.35em, rule(
          [$x : tau in Gamma$],
          [$Gamma tack.r x : tau$],
        )),

        [(Literal)],
        prooftree(vertical-spacing: 0.35em, rule(
          [(n, iota) : iota],
        )),

        [(Annotation)],
        prooftree(vertical-spacing: 0.35em, rule(
          [$Gamma tack.r t : tau$],
          [$Gamma tack.r (t : tau) : tau$],
        )),

        [($and$-introduction)],
        prooftree(vertical-spacing: 0.35em, rule(
          [$(forall i in I) Gamma tack.r t : tau_i$],
          [$forall i in I, tau_i in T_0$],
          [$Gamma tack.r t : and.big_(i in I) tau_i$],
        )),

        [($and$-elimination)],
        prooftree(vertical-spacing: 0.35em, rule(
          [$Gamma tack.r t : and.big_(i in I) tau_i$],
          [$i_0 in I$],
          [$Gamma tack.r t : tau_(i_0)$],
        )),

        [($lambda$-Abstraction)],
        prooftree(vertical-spacing: 0.35em, rule(
          [$Gamma union {x_1 : sigma_1, dots, x_n : sigma_n} tack.r t : tau$],
          [
            $"where" sigma_i = cases(
              tau_i &"if" eta_i = (x_i : tau_i),
              alpha_i &"if" eta_i = x_i
            )$
          ],
          [$Gamma tack.r (lambda (eta_1, dots, eta_n) . t) : (sigma_1 dots sigma_n) -> tau$],
        )),

        [(Application)],
        prooftree(vertical-spacing: 0.35em, rule(
          [$Gamma tack.r l : (and.big_(i in I) tau_i) -> sigma$],
          [$(forall i in I) Gamma tack.r N : tau_i$],
          [$Gamma tack.r m(n) : sigma$],
        )),

        [($forall$-generation) #h(1em)],
        prooftree(vertical-spacing: 0.35em, rule(
          label: [],
          [$Gamma tack.r t : tau$],
          [$alpha in.not "FTV"(Gamma)$],
          [$Gamma tack.r t : forall alpha . tau$],
        )),
      )),
      caption: "The declarative typing rules of Sonnet.",
    ) <typing_rules>
  ],
)


#example[
  It is shown that self-application $lambda x. x(x)$ can be typed as: $ forall alpha. forall beta. (alpha and (alpha -> beta)) -> beta $
]
