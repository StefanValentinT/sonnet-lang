package org.example.eval;

import java.util.HashMap;
import java.util.Map;
import org.example.syntax.AST;

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
}
