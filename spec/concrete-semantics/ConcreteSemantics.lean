import LiterateLean

= Concrete Semantics <concrete>

To eliminate ambiguity in the interpretation of the semantics presented above,
they have been formalized in the Lean proof assistant.
The complete formalization is included as part of this specification.
Wherever possible, references to the corresponding informal definitions are provided.

The formalization was carried out via a reference implementation
that evaluates an abstract syntactic construct of a Sonnet program to the
set of all its defined values and behaviours, thereby encompassing every value and every behaviour
the construct may yield in a conforming implementation.

The type definitions of @abstract_syntax_types in Lean.

```lean
-- primitive types
inductive PrimitiveType : Type
  | i8  : PrimitiveType
  | i16 : PrimitiveType
  | i32 : PrimitiveType
  | i64 : PrimitiveType
  | u8  : PrimitiveType
  | u16 : PrimitiveType
  | u32 : PrimitiveType
  | u64 : PrimitiveType
  | f16 : PrimitiveType
  | f32 : PrimitiveType
  | f64  : PrimitiveType
  deriving Repr

-- 𝛼
structure TypeVar where
  name : String
  deriving Repr, DecidableEq, Hashable

inductive SimpleType : Type where
  | var : TypeVar → SimpleType
  | arrow : SimpleType → SimpleType → SimpleType
  deriving Repr, DecidableEq

inductive NonEmptyList (α : Type)
  | mk : α → List α → NonEmptyList α
  deriving Repr, DecidableEq

structure IntersectionType where
  components : NonEmptyList SimpleType
  deriving Repr

inductive Rank2Type : Type where
  | simple : SimpleType → Rank2Type
  | inter : IntersectionType → Rank2Type
  | arrow : IntersectionType → Rank2Type → Rank2Type
  deriving Repr


```

