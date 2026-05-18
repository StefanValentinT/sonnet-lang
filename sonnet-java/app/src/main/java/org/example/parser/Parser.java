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
			case Token.TrueToken() -> new AST.TrueNode();
			case Token.FalseToken() -> new AST.FalseNode();
			case Token.NihilToken() -> new AST.NihilNode();

			case Token.FalseTypeToken() -> new AST.FalseTypeNode();
			case Token.TrueTypeToken() -> new AST.TrueTypeNode();
			case Token.NihilTypeToken() -> new AST.NihilTypeNode();
			case Token.StringTypeToken() -> new AST.StringTypeNode();
			case Token.AnySymbolTypeToken() -> new AST.AnySymbolTypeNode();
			case Token.SymbolTypeToken(String val) -> new AST.SymbolTypeNode(val);
			case Token.F16TypeToken() -> new AST.F16TypeNode();
			case Token.F32TypeToken() -> new AST.F32TypeNode();
			case Token.F64TypeToken() -> new AST.F64TypeNode();
			case Token.I8TypeToken() -> new AST.I8TypeNode();
			case Token.I16TypeToken() -> new AST.I16TypeNode();
			case Token.I32TypeToken() -> new AST.I32TypeNode();
			case Token.I64TypeToken() -> new AST.I64TypeNode();
			case Token.U8TypeToken() -> new AST.U8TypeNode();
			case Token.U16TypeToken() -> new AST.U16TypeNode();
			case Token.U32TypeToken() -> new AST.U32TypeNode();
			case Token.U64TypeToken() -> new AST.U64TypeNode();

			case Token.IntToken(var val) -> new AST.IntNode(val);
			case Token.FloatToken(var val) -> new AST.FloatNode(val);
			case Token.StringToken(var val) -> new AST.StringNode(val);
			case Token.SymbolToken(var val) -> new AST.SymbolNode(val);
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
