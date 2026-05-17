package org.example.syntax;

public class PrettyPrinter {
	public String prettifyAST(AST node) {
		return prettifyAST(node, 0);
	}

	private String prettifyAST(AST node, int tabLevel) {
		return switch (node) {
			case AST.IntNode(int value) -> node.toString();
			case AST.FloatNode(double value) -> node.toString();
			case AST.StringNode(String value) -> node.toString();
			case AST.SymbolNode(String symbol) -> node.toString();
			case AST.IdentNode(String name) -> node.toString();
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
