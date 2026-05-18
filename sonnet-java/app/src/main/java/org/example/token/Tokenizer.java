package org.example.token;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Tokenizer {
	private final Pattern idPattern = Pattern.compile("[a-zA-Z0-9!@$%^&/\\-_+=:<>.?*']+");

	public List<Token> tokenize(String input) {
		List<Token> tokens = new ArrayList<>();
		tokens.add(new Token.OpeningParen());
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

				case ':': {
					pos++;
					int start = pos;
					while (pos < length) {
						Matcher m = idPattern.matcher(String.valueOf(input.charAt(pos)));
						if (!m.matches()) break;
						pos++;
					}
					String lexeme = input.substring(start, pos);

					if (lexeme.isEmpty()) {

						tokens.add(new Token.IdentifierToken(":"));
					} else {

						tokens.add(new Token.SymbolToken(lexeme));
					}
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

					Token token =
							switch (lexeme) {
								case "true" -> new Token.TrueToken();
								case "false" -> new Token.FalseToken();
								case "true-type" -> new Token.TrueTypeToken();
								case "false-type" -> new Token.TrueTypeToken();
								case "string-type" -> new Token.StringTypeToken();
								case "sym-type" -> new Token.AnySymbolTypeToken();

								case "nihil-type" -> new Token.NihilTypeToken();
								case "nihil" -> new Token.NihilToken();

								case "f16" -> new Token.F16TypeToken();
								case "f32" -> new Token.F32TypeToken();
								case "f64" -> new Token.F64TypeToken();

								case "i8" -> new Token.I8TypeToken();
								case "i16" -> new Token.I16TypeToken();
								case "i32" -> new Token.I32TypeToken();
								case "i64" -> new Token.I64TypeToken();

								case "u8" -> new Token.U8TypeToken();
								case "u16" -> new Token.U16TypeToken();
								case "u32" -> new Token.U32TypeToken();
								case "u64" -> new Token.U64TypeToken();

								default -> {
									if (lexeme.startsWith("sym-") && lexeme.endsWith("-type") && lexeme.length() > 9) {
										String symbolVal = lexeme.substring(4, lexeme.length() - 5);
										yield new Token.SymbolTypeToken(symbolVal);
									}
									yield new Token.IdentifierToken(lexeme);
								}
							};

					tokens.add(token);
					break;
				}
			}
		}
		tokens.add(new Token.ClosingParen());
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
