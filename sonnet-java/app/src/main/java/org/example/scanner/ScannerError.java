package org.example.scanner;

import org.example.CompilerError;

public class ScannerError extends CompilerError {
	public ScannerError() {
		super("Scanner", "Failed to read or locate the source file.");
	}
}
