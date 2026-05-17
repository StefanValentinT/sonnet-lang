package org.example.scanner;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

public class FileScanner {

	public String readFile(String filePath) {
		try {
			return Files.readString(Path.of(filePath));
		} catch (IOException | NullPointerException e) {
			throw new ScannerError();
		}
	}
}
