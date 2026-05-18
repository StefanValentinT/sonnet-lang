package org.example.syntax;

import java.util.List;

public sealed interface AST {
	record TrueNode() implements AST {
		@Override
		public String toString() {
			return "true";
		}
	}

	record StringTypeNode() implements AST {
		@Override
		public String toString() {
			return "string-type";
		}
	}

	record AnySymbolTypeNode() implements AST {
		@Override
		public String toString() {
			return "sym-type";
		}
	}

	record SymbolTypeNode(String symbolVal) implements AST {
		@Override
		public String toString() {
			return "sym-" + symbolVal + "-type";
		}
	}

	record TypeTypeNode() implements AST {
		@Override
		public String toString() {
			return "type";
		}
	}

	record FalseNode() implements AST {
		@Override
		public String toString() {
			return "false";
		}
	}

	record TrueTypeNode() implements AST {
		@Override
		public String toString() {
			return "true-type";
		}
	}

	record FalseTypeNode() implements AST {
		@Override
		public String toString() {
			return "false-type";
		}
	}

	record NihilNode() implements AST {
		@Override
		public String toString() {
			return "nihil";
		}
	}

	record NihilTypeNode() implements AST {
		@Override
		public String toString() {
			return "nihil-type";
		}
	}

	record F16TypeNode() implements AST {
		@Override
		public String toString() {
			return "f16";
		}
	}

	record F32TypeNode() implements AST {
		@Override
		public String toString() {
			return "f32";
		}
	}

	record F64TypeNode() implements AST {
		@Override
		public String toString() {
			return "f64";
		}
	}

	record I8TypeNode() implements AST {
		@Override
		public String toString() {
			return "i8";
		}
	}

	record I16TypeNode() implements AST {
		@Override
		public String toString() {
			return "i16";
		}
	}

	record I32TypeNode() implements AST {
		@Override
		public String toString() {
			return "i32";
		}
	}

	record I64TypeNode() implements AST {
		@Override
		public String toString() {
			return "i64";
		}
	}

	record U8TypeNode() implements AST {
		@Override
		public String toString() {
			return "u8";
		}
	}

	record U16TypeNode() implements AST {
		@Override
		public String toString() {
			return "u16";
		}
	}

	record U32TypeNode() implements AST {
		@Override
		public String toString() {
			return "u32";
		}
	}

	record U64TypeNode() implements AST {
		@Override
		public String toString() {
			return "u64";
		}
	}

	record IntNode(int value) implements AST {
		@Override
		public String toString() {
			return String.valueOf(value);
		}
	}

	record FloatNode(double value) implements AST {
		@Override
		public String toString() {
			return String.valueOf(value);
		}
	}

	record StringNode(String value) implements AST {
		@Override
		public String toString() {
			return "\"" + value + "\"";
		}
	}

	record SymbolNode(String symbol) implements AST {
		@Override
		public String toString() {
			return ":" + symbol;
		}
	}

	record IdentNode(String name) implements AST {
		@Override
		public String toString() {
			return name;
		}
	}

	record ListNode(List<AST> elements) implements AST {
		@Override
		public String toString() {
			StringBuilder sb = new StringBuilder();
			sb.append("(");
			for (int i = 0; i < elements.size(); i++) {
				sb.append(elements.get(i).toString());
				if (i < elements.size() - 1) {
					sb.append(" ");
				}
			}
			sb.append(")");
			return sb.toString();
		}
	}
}
