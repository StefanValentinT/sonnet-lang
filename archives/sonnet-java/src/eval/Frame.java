package eval;

import java.util.HashMap;
import java.util.Map;
import syntax.AST;

public class Frame {
	private final Frame parent;
	private final Map<String, AST> bindings = new HashMap<>();

	public Frame(Frame parent) {
		this.parent = parent;
	}

	public Frame() {
		this.parent = null;
	}

	public void define(String name, AST value) {
		bindings.put(name, value);
	}

	public void assign(String name, AST value) {
		if (this.bindings.containsKey(name)) {
			this.bindings.put(name, value);
		} else if (this.parent != null) {
			this.parent.assign(name, value);
		} else {
			throw new EvaluationError("Unbound variable: " + name);
		}
	}

	public AST lookup(String name) {
		if (bindings.containsKey(name)) {
			return bindings.get(name);
		}
		return parent != null ? parent.lookup(name) : null;
	}

	public boolean contains(String name) {
		if (bindings.containsKey(name)) {
			return true;
		} else {
			return false;
		}
	}

	public StringBuilder toStringBuilder(StringBuilder sb) {
		bindings.forEach((key, value) -> sb.append(key + " : " + value + "\n"));
		if (parent != null) {
			parent.toStringBuilder(sb);
		}
		return sb;
	}

	@Override
	public String toString() {
		return toStringBuilder(new StringBuilder("--- Frame ---\n")).append("\n-------------").toString();
	}
}
