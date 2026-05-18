package org.example.token;

public sealed interface Token {
	record TrueToken() implements Token {}

	record FalseToken() implements Token {}

	record TrueTypeToken() implements Token {}

	record FalseTypeToken() implements Token {}

	record StringTypeToken() implements Token {}

	record AnySymbolTypeToken() implements Token {}

	record SymbolTypeToken(String symbolVal) implements Token {}

	record NihilTypeToken() implements Token {}

	record NihilToken() implements Token {}

	record F16TypeToken() implements Token {}

	record F32TypeToken() implements Token {}

	record F64TypeToken() implements Token {}

	record I8TypeToken() implements Token {}

	record I16TypeToken() implements Token {}

	record I32TypeToken() implements Token {}

	record I64TypeToken() implements Token {}

	record U8TypeToken() implements Token {}

	record U16TypeToken() implements Token {}

	record U32TypeToken() implements Token {}

	record U64TypeToken() implements Token {}

	record IntToken(int value) implements Token {}

	record FloatToken(double value) implements Token {}

	record StringToken(String value) implements Token {}

	record SymbolToken(String symbol) implements Token {}

	record IdentifierToken(String name) implements Token {}

	record OpeningParen() implements Token {}

	record ClosingParen() implements Token {}
}
