#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#include "lexer.c"
#include "log.c"
#include "parser.c"
#include "syntax.c"
#include "typer.c"
#include "emitter.c"

void panic(void) { exit(EXIT_FAILURE); }

void testPlatform(void)
{
	int fSize = sizeof(float);
	int lSize = sizeof(double);
	printf(
	    "The sizes of a float and a double on this platform are %d (float) and "
	    "%d (double).\n",
	    fSize, lSize
	);
	if (fSize == 4 && lSize == 8)
	{
		printf("Therefore this platform is suitable for running the compiler.\n");
	}
	else
	{
		logFatal("This compiler relies on 32-bit floats and 64-bit doubles.\n"
		         "As your platform does not provide these, the compiler can "
		         "not function.\n"
		         "In the future there may be work to simulate these widths to "
		         "increase portability.\n"
		         "We are sorry for having caused you this inconvenience. :(\n"

		);
	}
}

char* readFile(char* path)
{
	FILE* file = fopen(path, "rb");
	if (file == NULL)
	{
		logFatal("Could not open file '%s'.", path);
	}

	fseek(file, 0L, SEEK_END);
	size_t fileSize = (size_t)ftell(file);
	rewind(file);

	char* buffer = (char*)malloc(fileSize + 1);
	if (buffer == NULL)
	{
		logFatal("Can not allocate anough memory to read '%s'.\n", path);
	}
	size_t bytesRead = fread(buffer, sizeof(char), fileSize, file);
	if (bytesRead < fileSize)
	{
		logFatal("Could not read entire file '%s'", path);
	}
	buffer[bytesRead] = '\0';

	fclose(file);
	return buffer;
}

int main(int argc, char** argv)
{
	testPlatform();

	if (argc != 2)
	{
		logFatal("%s may be invoked with one argument, but got %d\n", argv[0], argc);
	}
	char* fileName = argv[1];
	printfn("Compiling %s.", fileName);

	char* source = readFile(fileName);
	Program ast = parse(source);
	printProgram(&ast);
	type(&ast, getMaxId());
	printf("--- Typed AST ---\n");
	printProgram(&ast);
	printf("--- Emitting ----\n");
	emitProgram(&ast);

	free(source);

	return 0;
}
