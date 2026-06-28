#import "spec_template.typ": *

= Semantics

== Numeric Types <numeric>

The primitive types of numbers are `i8`, `i16`, `i32`, `i64`, `u8`, `u16`, `u32`, `u64`, `f16`, `f32` and `f64`. The number types of the form i$n$, u$n$ or f$n$ where $n in NN$ use $n$ bits for the representation of a value of this type. $n$ is a multiply of bit-length of a byte.

More primitive number types may be defined by an implementation.

=== Signed integer types <signed_numbers>
The four signed integer types are `i8`, `i16`, `i32` and `i64`.

A signed integer type i$n$ represents a value encoded using a two's-complement binary representation consisting of $n$ bits. The set of representable values $V_("i"n)$ for a type i$n$ is:

$ V_("i"n) = { x in ZZ | -2^(n-1) <= x <= 2^(n-1) - 1 } $

=== Unsigned integer types <unsigned_numbers>
The four unsigned integer types are `u8`, `u16`, `u32` and `u64`.

An unsigned integer type u$n$ represents a value encoded as a standard binary number. The set of representable values $V_("u"n)$ for a type u$n$ is:

$ V_("u"n) = { x in ZZ | 0<= x <= 2^n - 1 } $

=== Floating-point types <float_numbers>
The three floating-point types are f16, f32 and f64. A floating-point type f$n$ represents a value encoded using the IEEE 754 standard for floating-point arithmetic @IEEE_754.

Correspondence between floating-point types and the formats defined by IEEE 754:
#grid(
	columns: (auto, 1fr),
	gutter: 12pt,
	row-gutter: 8pt,

	[`f16`], [half-precision floating-point],
	[`f32`], [single-precision floating-point],
	[`f64`], [double-precision floating-point]
)

== Conversion between number types <conversion>

=== Conversion between integer types <con_princip>
The conversion of an integer type to a larger integer type, where the target value set is a superset of the source value set, preserves the original value.

=== Integer to integer of equal size <int_to_int_equal>
The set of nonnegative values inhabiting a signed integer type is a subset of the set for the corresponding unsigned integer type, and the representation of the same value shall be identical in both types.

A negative integer $a$ is converted to a positive unsigned integer by adding $2^n$ where n is the number of bits used to represent $a$. Conversely, converting an unsigned integer to a signed integer is done by subtracting $2^n$.

#note[
	A signed value $-2^(n-1)$ becomes $2^(n-1)$, the middle of the range of integers of the correspondent unsigned type.
	The biggest value representable by an unsigned type, $(2^n - 1)$, become $-1$.
]

=== Unsigned integer to larger integer
The value remains preserved exactly in accordance to @con_princip.

=== Signed integer to larger integer
A signed integer value $a$ is converted to a larger target integer type of $n$ bits by preserving its value exactly if the target type is signed or if $a >= 0$. If $a < 0$ and the target type is unsigned, the value is converted by adding $2^n$.

=== Integer to integer of lesser size
An integer value $a$ is converted to a smaller target integer type of $n$ bits by first reducing its value modulo $2^n$ to an integer in the range $[0, 2^n - 1]$. If the target type is signed and the resulting value is greater than or equal to $2^(n-1)$, the final value is obtained by subtracting $2^n$ from that result.

#note[
	Operationally, this corresponds to truncating the higher-order bits, retaining only the lowest $n$ bits of the original binary representation. 

	If the target type is unsigned, these remaining bits are interpreted as a standard binary number. If the target type is signed, the highest bit of the remaining $n$ bits acts as the sign bit.
]

=== Floating-point number to integer
If the floating-point numbers' integral component can be represented in the target type that shall be the result, otherwise the result is implementation-defined.

=== Integer to floating-point
The result of converting a value of an integer type (i$n$ or u$n$) to a floating-point type f$m$ is determined by whether the value can be exactly represented in the target format:

- If the integer value can be represented exactly in the target floating-point type, the result is that exact representation.
- If the integer value cannot be represented exactly but falls between two representable floating-point values, the value is rounded to one of the adjacent representable values. The choice of rounding direction is implementation-defined but must conform to the IEEE 754 standard.
- If the integer value exceeds the maximum representable finite value of the target floating-point type, the result is undefined.

=== Floating-point to floating-point
The result of converting a value of a floating-point type f$n$ to another floating-point type f$m$ depends on the relative precision of the source and target formats:

- If the source value can be represented exactly in the target format, the value shall remian unchanged.
- If the source value cannot be represented exactly because the target format has less precision, the value is rounded to one of the adjacent representable values. The rounding direction is implementation-defined but must conform to the IEEE 754 rounding modes.
- If the magnitude of the source value exceeds the maximum representable finite value of the target type, the result is implementation-defined.

=== Integer Overflow
If an integer by means of any operation on it overflows the resulting value is implementation-defined.
