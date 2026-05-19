package org.example.parser;

import java.util.ArrayList;
import java.util.List;
import org.example.syntax.AST;
import org.example.token.Token;

public class Parser {
	public Parser() {}

	public AST parse(List<Token> tokenStream) {
		if (tokenStream.isEmpty()) {
			return new AST.ListNode(List.of());
		}

		Token tok = tokenStream.removeFirst();

		return switch (tok) {
			case Token.IntToken(var val) -> new AST.IntNode(val);
			case Token.FloatToken(var val) -> new AST.FloatNode(val);
			case Token.StringToken(var val) -> new AST.StringNode(val);
			case Token.IdentifierToken(var val) -> {
				if (val.equals(":")) {
					AST quotedContent = parse(tokenStream);
					yield new AST.ListNode(List.of(new AST.IdentNode("quote"), quotedContent));
				} else {
					yield new AST.IdentNode(val);
				}
			}
			case Token.OpeningParen() -> {
				ArrayList<AST> listChildren = new ArrayList<>();
				while (!tokenStream.isEmpty() && !(tokenStream.getFirst() instanceof Token.ClosingParen)) {
					listChildren.add(parse(tokenStream));
				}
				if (tokenStream.isEmpty()) {
					throw new ParserError();
				}
				tokenStream.removeFirst();
				yield new AST.ListNode(listChildren);
			}
			case Token.ClosingParen() -> throw new ParserError();
		};
	}
}
