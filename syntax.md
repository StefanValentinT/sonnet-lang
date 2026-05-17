# Sonnet

Sonnet is a systems programming language that strives to be both principled and to execute every idea to its perfection. It aims to be a complete language where every component fits together seamlessly; if a feature cannot be implemented perfectly, it is discarded.

Many languages suffer from a gap between what is possible within their native syntax and what can only be achieved externally (or through "unsafe" blocks, as in Rust). Sonnet, however, is designed to be capable of everything. It achieves this by doing nothing on its own, ensuring it never hinders the programmer. This philosophy naturally excludes features like a borrow checker or garbage collector.

Sonnet is a simple language in the sense of a language from which nothing can be removed, and one that performs no action unless explicitly commanded by the user.


# Syntax

(x a b) - the S-expression, will be evaluated
(x)

(x, a, b) - array literal, will not be evaluated
(x,)
()



# Identifiers

Any sequence of non-whitespace characters not starting with a digit or `:` and not
containing any of the following characters: `()[]{},;?"`

# Primitives

true
false
nihil

## Keywords

:identifier

## Numbers

There are the numeric types:

u8 u16 u32 u64 i8 i16 i32 i64 f16 f32 f64

A literal is constructed by writing for example 13u8 the suffix can not be omitted,
there is no default numeric type.


## Strings

A string is wrtien in double quotes like this "Hello!", it is a poitner to an u8 like in C.

# Execution

The top level consists of only definitions. All of them come into being at the same time. THis allows for mutual recursion.
Execution starts with a function main being called. Main must be present.

# Special Forms

(def name value)

Binds `name to `value` in the current scope. `name` must be unbound. name is bound inside the value, this allows for recursion. Top-level defintiosn are treated differently, but others are lexically scoped.

(var name value)

Mutable binding.

(set name value)

Sets an identifier that is mutably bound to the given value.

(fun param body)

A function. Binds param inside the body, param may be an array destruction.
Functions can only capture free variables (those being bound by a function), there are no closures. It can capture top-level definitions.

(fun (a b) (print a) (print b)) 

A fucntion can take multiple params and also have multple statments in its body.
A fucntion body is its own scope.

(do e e2 e3)

Perform a sequence of actions in a new scope.

(if cond then else)
(while cond body)

Both conditional constructs do not create new scopes in their bodies.

(struct ...)

Takes even finite number of params and constructs a struct out of them that behvaes like in C, Every second elemnt must be a keyword. A struct created with (struct :a 0i32 :b 3i32) can be passed to a C function expecting a struct like struct {i32 a; i32 b}

(set struct-val keyword value)

Not the same as the set prior emtnoiend this sets a struct field. Only possible if the struct-val is bound mutably.

(get struct-val keyword)

Get the value associated with some key from a struct.


# Pointers

(ref val ? offset)
Creates a pointer to a variable.

(deref pointer-val)
Dereferences a pointer.

(set-ptr pointer-val val)

Sets the value a pointer is pointin to.

Pointers work with arrays as expected:

```
(var buffer (10, 20, 30, 40,))

(var p0 (ref buffer))

(var p2 (ref buffer 2))
```


# Primitives

Primitives are function builtin into the compiler, because they either can not be defined using other functions or the compiler needs to know more about them.

They are:

- `+-*/^` numeric functions upon two same nuber types - self explanatory

- print-int - takes any numeric type and prints it

- print - takes a String or a numeric type and prints it

- print-char - takes a numeric type and interprets it as utf-8 then prints it, u8 maps to the ascii standard and biger types to multicharacter sequences.

- input - takes a string that is given to the user as a prompt and returns a string that contians the users response

- string-to-int - takes a string and returns a struct with two fields :tag which is either :some or :none and :val which contains a value iff :tag is :some

- use-c - (use-c header.h h) no brackets or quotes around the apth to the header, may contain . and .. the slash is used as a seperator platform-independent, takes an identifer as a secodn argument that all funciton form the header are prefixed with, so that if header.h were to declare a function add it could eb sued from the program using h/add, h/add is just a normal dientifier