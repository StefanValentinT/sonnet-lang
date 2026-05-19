package token;

public sealed interface Token {

	record IntToken(int value) implements Token {
	}

	record FloatToken(double value) implements Token {
	}

	record StringToken(String value) implements Token {
	}

	record IdentifierToken(String name) implements Token {
	}

	record OpeningParen() implements Token {
	}

	record ClosingParen() implements Token {
	}
}
