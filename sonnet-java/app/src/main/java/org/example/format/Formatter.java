package org.example.format;

import java.util.List;
import java.util.Stack;
import org.example.token.Token;

public class Formatter {
	private static final int MAX_LINE_LENGTH = 60;

	public String format(List<Token> tokens) {
		if (tokens == null || tokens.isEmpty()) return "";

		StringBuilder sb = new StringBuilder();
		Stack<String> contextStack = new Stack<>();
		Stack<Integer> elementCountStack = new Stack<>();

		int depth = 0;
		int currentLineLength = 0;
		int openParensOnCurrentLine = 0;
		boolean isClosingParenLine = false;

		for (int i = 1; i < tokens.size() - 1; i++) {
			Token token = tokens.get(i);

			if (token instanceof Token.OpeningParen) {
				if (depth == 0 && sb.length() > 0) {
					sb.append("\n\n");
					currentLineLength = 0;
					openParensOnCurrentLine = 0;
				} else if (depth > 0) {
					elementCountStack.push(elementCountStack.pop() + 1);
					int spaceAdded = handleSpacing(
							sb, contextStack.peek(), elementCountStack.peek(), depth, tokens, i, currentLineLength);
					if (spaceAdded == -1) {
						currentLineLength = depth * 4;
						openParensOnCurrentLine = 0;
					} else {
						currentLineLength += spaceAdded;
					}
				}

				sb.append("(");
				currentLineLength += 1;
				depth++;
				openParensOnCurrentLine++;
				isClosingParenLine = false;
				contextStack.push("");
				elementCountStack.push(0);

			} else if (token instanceof Token.ClosingParen) {
				if (depth > 0) {
					depth--;
					contextStack.pop();
					elementCountStack.pop();

					if (openParensOnCurrentLine <= 0) {
						if (!isClosingParenLine && !fitsOnCurrentLine(tokens, i, depth + 1, currentLineLength)) {
							sb.append("\n").append("\t".repeat(depth));
							currentLineLength = depth * 4;
							openParensOnCurrentLine = 0;
							isClosingParenLine = true;
						}
					} else {
						openParensOnCurrentLine--;
					}

					sb.append(")");
					currentLineLength += 1;
				}
			} else {
				String str = stringify(token);

				if (depth > 0) {
					int currentIdx = elementCountStack.pop() + 1;
					elementCountStack.push(currentIdx);

					if (currentIdx == 1) {
						contextStack.pop();
						contextStack.push(str);
					}

					int spaceAdded =
							handleSpacing(sb, contextStack.peek(), currentIdx, depth, tokens, i, currentLineLength);
					if (spaceAdded == -1) {
						currentLineLength = depth * 4;
						openParensOnCurrentLine = 0;
					} else {
						currentLineLength += spaceAdded;
					}
				}

				sb.append(str);
				currentLineLength += str.length();
				isClosingParenLine = false;
			}
		}
		return sb.toString().trim();
	}

	private int handleSpacing(
			StringBuilder sb,
			String context,
			int index,
			int depth,
			List<Token> tokens,
			int currentTokenIdx,
			int currentLineLength) {
		if (sb.length() == 0) return 0;
		char lastChar = sb.charAt(sb.length() - 1);
		if (lastChar == '(') return 0;

		Token prevToken = tokens.get(currentTokenIdx - 1);
		if (prevToken instanceof Token.IdentifierToken && ":".equals(((Token.IdentifierToken) prevToken).name())) {
			return 0;
		}

		if (fitsOnCurrentLine(tokens, currentTokenIdx, depth, currentLineLength)) {
			sb.append(" ");
			return 1;
		}

		boolean isMultiLineFun = "fun".equals(context) && index == 3 && hasBodyStatementsAhead(tokens, currentTokenIdx);

		boolean shouldLineBreak = ("def".equals(context) && index == 3 && depth == 1)
				|| isMultiLineFun
				|| ("do".equals(context) && index > 1);

		if (shouldLineBreak) {
			sb.append("\n").append("\t".repeat(depth));
			return -1;
		} else {
			sb.append(" ");
			return 1;
		}
	}

	private boolean fitsOnCurrentLine(List<Token> tokens, int currentIndex, int targetDepth, int currentLineLength) {
		int scanIndex = currentIndex;
		int currentDepth = targetDepth;

		while (scanIndex > 1 && currentDepth > 1) {
			scanIndex--;
			if (tokens.get(scanIndex) instanceof Token.OpeningParen) currentDepth--;
			if (tokens.get(scanIndex) instanceof Token.ClosingParen) currentDepth++;
		}

		int startIdx = scanIndex;
		int parenDepth = 1;
		int expressionLength = 0;

		while (startIdx < tokens.size() - 1 && parenDepth > 0) {
			Token t = tokens.get(startIdx);
			if (startIdx != scanIndex) {
				if (t instanceof Token.OpeningParen) parenDepth++;
				else if (t instanceof Token.ClosingParen) parenDepth--;
			}

			Token prev = tokens.get(startIdx - 1);
			boolean isStickyColon =
					(prev instanceof Token.IdentifierToken && ":".equals(((Token.IdentifierToken) prev).name()));

			if (startIdx > scanIndex
					&& !(t instanceof Token.ClosingParen)
					&& !(tokens.get(startIdx - 1) instanceof Token.OpeningParen)
					&& !isStickyColon) {
				expressionLength += 1;
			}

			expressionLength += stringify(t).length();
			if (t instanceof Token.OpeningParen || t instanceof Token.ClosingParen) {
				expressionLength += 1;
			}
			startIdx++;
		}

		return (currentLineLength + expressionLength) <= MAX_LINE_LENGTH;
	}

	private boolean hasBodyStatementsAhead(List<Token> tokens, int currentIndex) {
		int lookAheadLimit = Math.min(tokens.size() - 1, currentIndex + 5);
		int internalParenCount = 0;

		for (int i = currentIndex; i < lookAheadLimit; i++) {
			Token t = tokens.get(i);
			if (t instanceof Token.OpeningParen) internalParenCount++;
			if (t instanceof Token.ClosingParen) {
				if (internalParenCount == 0) return false;
				internalParenCount--;
			}
		}
		return true;
	}

	private String stringify(Token t) {
		if (t instanceof Token.IntToken token) return String.valueOf(token.value());
		if (t instanceof Token.FloatToken token) return String.valueOf(token.value());
		if (t instanceof Token.StringToken token) return "\"" + token.value() + "\"";
		if (t instanceof Token.SymbolToken token) return ":" + token.symbol();
		if (t instanceof Token.IdentifierToken token) return token.name();
		return "";
	}
}
