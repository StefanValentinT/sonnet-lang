package org.example.token;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Tokenizer {
	private final Pattern idPattern = Pattern.compile("[a-zA-Z0-9!@$%^&/\\-_+=:<>.?*']+");

	public List<Token> tokenize(String input) {
		List<Token> tokens = new ArrayList<>();
		// tokens.add(new Token.OpeningParen());
		int length = input.length();
		int pos = 0;

		while (pos < length) {
			pos = skipWhitespaceAndComments(input, pos, length);
			if (pos >= length) break;

			char ch = input.charAt(pos);

			switch (ch) {
				case '(':
					tokens.add(new Token.OpeningParen());
					pos++;
					break;

				case ')':
					tokens.add(new Token.ClosingParen());
					pos++;
					break;

				case '"': {
					pos++;
					int start = pos;
					while (pos < length && input.charAt(pos) != '"') {
						pos++;
					}
					if (pos >= length) throw new TokenizerError();

					tokens.add(new Token.StringToken(input.substring(start, pos)));
					pos++;
					break;
				}

				case '0':
				case '1':
				case '2':
				case '3':
				case '4':
				case '5':
				case '6':
				case '7':
				case '8':
				case '9': {
					int start = pos;
					boolean hasDot = false;

					while (pos < length) {
						char curr = input.charAt(pos);
						if (curr == '.') {
							if (hasDot) throw new TokenizerError();
							hasDot = true;
						} else if (isSequencingOmen(curr)) {
							break;
						} else if (!Character.isDigit(curr)) {
							throw new TokenizerError();
						}
						pos++;
					}

					String lexeme = input.substring(start, pos);

					if (hasDot) {
						tokens.add(new Token.FloatToken(Double.parseDouble(lexeme)));
					} else {
						tokens.add(new Token.IntToken(Integer.parseInt(lexeme)));
					}

					break;
				}

				default: {
					int start = pos;
					while (pos < length) {
						Matcher m = idPattern.matcher(String.valueOf(input.charAt(pos)));
						if (!m.matches()) break;
						pos++;
					}

					String lexeme = input.substring(start, pos);
					if (lexeme.isEmpty()) throw new TokenizerError();
					tokens.add(new Token.IdentifierToken(lexeme));
					break;
				}
			}
		}
		// tokens.add(new Token.ClosingParen());
		return tokens;
	}

	private boolean isSequencingOmen(char c) {
		return Character.isWhitespace(c) || c == '(' || c == ')';
	}

	private int skipWhitespaceAndComments(String input, int pos, int length) {
		while (pos < length) {
			char ch = input.charAt(pos);
			if (Character.isWhitespace(ch)) {
				pos++;
			} else if (ch == '#') {
				while (pos < length && input.charAt(pos) != '\n') {
					pos++;
				}
			} else {
				break;
			}
		}
		return pos;
	}
}
