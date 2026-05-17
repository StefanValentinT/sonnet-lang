package org.example;

public class CompilerError extends RuntimeException {
	public CompilerError(String who, String detail) {
		if (detail != null) {
			detail = "\n Reasoning: " + detail;
		}
		super(who + " determined that compilation must be aborted." + detail);
	}
}
