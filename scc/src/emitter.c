#ifndef EMITER_H
#define EMITTER_H
#endif

#include "syntax.c"

void emitProgram(Program* program);

#if __INCLUDE_LEVEL__ == 0

#include <stdio.h>

#define out stdout

void emitExpression(Term* term)
{
	switch (term->kind)
	{
	case RETURN:
		fprintf(out, "ret ");
		emitExpression(term->data.retur.exp);
		fprintf(out, "\n");
	case CONSTANT:
		switch (term->data.constant.numericType.kind)
		{
		case I32:
			fprintf(out, "%d", term->data.constant.data.i32Val);
		}
	}
}

void emitDeclaration(DeclarationData* decl)
{
	if (decl->exp->kind == FUNCTION)
	{
		string name = toString(&decl->name);
		fprintf(out, "export function w $%.*s ()\n", (int)name.length, name.chars);
		fprintf(out, "{\n");
		fprintf(out, "    @start\n");
		fprintf(out, "    ");
		Term retExpr = *decl->exp->data.block.stmts[0].data.unit_expression;
		printTerm(&retExpr);
		emitExpression(&retExpr);
		fprintf(out, "}");
	}
}

void emitProgram(Program* program)
{
	for (size i = 0; i < program->_declCount; i++)
	{
		emitDeclaration(&program->decls[i]);
	}
}


#endif
