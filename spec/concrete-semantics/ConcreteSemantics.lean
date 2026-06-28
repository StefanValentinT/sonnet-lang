import LiterateLean
import Aesop
import Mathlib

= Concrete Semantics <concrete>

To eliminate ambiguity in the interpretation of the semantics presented above,
they have been formalized in the Lean proof assistant.
The complete formalization is included as part of this specification.
Wherever possible, references to the corresponding informal definitions are provided.

Numeric types as defined in @numeric.

```lean
inductive NumericType : Type
  | uInt    : Nat → NumericType
  | int     : Nat → NumericType
  | float   : Nat → NumericType
  deriving Repr

def i8_type  : NumericType := NumericType.int 8
def i16_type : NumericType := NumericType.int 16
def i32_type : NumericType := NumericType.int 32
def i64_type : NumericType := NumericType.int 64

def u8_type  : NumericType := NumericType.uInt 8
def u16_type : NumericType := NumericType.uInt 16
def u32_type : NumericType := NumericType.uInt 32
def u64_type : NumericType := NumericType.uInt 64

def f16_type : NumericType := NumericType.float 16
def f32_type : NumericType := NumericType.float 32
def f64_type : NumericType := NumericType.float 64
```
The set of representable values of a signed type (@signed_numbers) is defined by `representableInt`.

```lean
def representableInt (n : Nat) (v : Int) : Prop :=
  let maxExp : Nat := n - 1
  (-(2 : Int)^maxExp ≤ v) ∧ (v ≤ (2 : Int)^maxExp - 1)
```

Conversely, `representableUInt`.

```lean
def representableUInt (n : Nat) (v : Int) : Prop :=
  let maxVal : Int := (2 : Int)^n - 1
  (0 ≤ v) ∧ (v ≤ maxVal)
```

An integer value is represented by an `Int` along with a proof of it being representable by the type.

```lean
def SonnetInt (n : Nat) := Subtype (fun v : Int => representableInt n v)
def SonnetUInt (n : Nat) := Subtype (fun v : Int => representableUInt n v)

def mkInt  (n : Nat) (a : Int) (h : representableInt n a)  : SonnetInt n := ⟨a, h⟩
def mkUInt (n : Nat) (a : Int) (h : representableUInt n a) : SonnetUInt n := ⟨a, h⟩
```

Using these numeric types and values, general values are defined.
```lean
def numValueType : NumericType → Type
  | NumericType.int n   => SonnetInt n
  | NumericType.uInt n  => SonnetUInt n
  | NumericType.float _ => Float


inductive Value : Type
  | numeric : (t : NumericType) → numValueType t → Value
```

Conversion between two integers of equal size as defined in @int_to_int_equal first 
requires a proof of the size equality of two integer types.

```lean
```

Floating-point types are defined in accordance to the IEEE 754 standard. (@float_numbers)

```lean
structure SonnetFloat (exp mant : Nat) where
  sign     : Bool
  exponent : BitVec exp
  mantissa : BitVec mant

def f16 : SonnetFloat 5 10 := {sign := false, exponent := 0#5, mantissa := 0#10}
def f32 : SonnetFloat 8 23 := {sign := false, exponent := 0#8, mantissa := 0#23}
def f64 : SonnetFloat 11 52 := {sign := false, exponent := 0#11, mantissa := 0#52}
```



