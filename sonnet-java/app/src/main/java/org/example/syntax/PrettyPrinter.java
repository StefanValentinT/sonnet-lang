package org.example.syntax;

public class PrettyPrinter {
	public String prettifyAST(AST node) {
		return prettifyAST(node, 0);
	}

	private String prettifyAST(AST node, int tabLevel) {
		return switch (node) {
			case AST.IntNode(int value) -> String.valueOf(value);
			case AST.FloatNode(double value) -> String.valueOf(value);
			case AST.StringNode(String value) -> "\"" + value + "\"";
			case AST.SymbolNode(String symbol) -> ":" + symbol;
			case AST.IdentNode(String name) -> name;
			case AST.ListNode(var elements) -> {
				if (elements.isEmpty()) {
					yield "()";
				}
				StringBuilder sb = new StringBuilder("(\n");
				String currentTabs = "\t".repeat(tabLevel);
				String childTabs = "\t".repeat(tabLevel + 1);
				for (AST element : elements) {
					sb.append(childTabs)
							.append(prettifyAST(element, tabLevel + 1))
							.append("\n");
				}
				sb.append(currentTabs).append(")");
				yield sb.toString();
			}
		};
	}
}
