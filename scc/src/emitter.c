#ifndef EMITER_H
#define EMITTER_H
#endif

#include "syntax.c"

void emitProgram(Program* program);

#if __INCLUDE_LEVEL__ == 0

#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "log.c"

#define out stdout

// TODO: How to handle it, if this were to literally be the name of a variable?
#define TEMP "%%__qbe__temp__%llu"
#define LABEL "@__label__%llu"

// 0 is an invalid temporary
u64 newTemp(void)
{
	static u64 maxTemp = 0;
	return ++maxTemp;
}

u64 newLabel(void)
{
	static u64 maxLabel = 0;
	return ++maxLabel;
}


int indent_depth = 0;
void indent(void)
{
	for (int i = 0; i < indent_depth; i++)
	{
		fprintf(out, "    ");
	}
}
void down(void){
	indent_depth++;
}
void up(void){
	indent_depth--;
}

void inst(char* fmt, ...)
{
	va_list args;
	va_start(args, fmt);
	indent();
	vfprintf(out, fmt, args);
	fprintf(out, "\n");
	va_end(args);
}

char* typeToQbe(const Type* t)
{
	switch (t->kind)
	{
		case BOOL: return "ub";
		
		case I8:  return "sb";
		case U8:  return "ub";
		case I16: return "sh";
		case U16: return "uh";

		case I32: return "w";
		case U32: return "w";
		case I64: return "l";
		case U64: return "l";

		case F32: return "s";
		case F64: return "d";

		default:
			logFatal("Typekind %d not yet implemented in emitter.", t->kind);
			break;
	}
}

u64 emitExpression(Term* term)
{
	u64 result = 0;
	switch (term->kind)
	{
	case RETURN:
		result = 0;
		inst("ret " TEMP "\n",  emitExpression(term->data.retur.exp));
		break;

	case BOOLEAN:
		result = newTemp();
		if (term->data.boolean.boolVal)
		{
			inst(TEMP " =w copy 1", result);
		} else
		{
			fprintf(out, TEMP " =w copy 0", result);
		}
		break;
		
	case CONSTANT:
		result = newTemp();
		indent();
		fprintf(out, TEMP, result);
		uint64_t bits;
		switch (term->data.constant.numericType.kind)
		{
		case I8:
			fprintf(out, " = copy %d", term->data.constant.data.i8Val);
			break;
		case I16:
			fprintf(out, " = copy %d", term->data.constant.data.i16Val);
			break;
		case I32:
			fprintf(out, " = copy %d", term->data.constant.data.i32Val);
			break;
		case I64:
			fprintf(out, " = copy %" PRIu64, term->data.constant.data.i64Val);
			break;
		case U8:
			fprintf(out, " = copy %u", term->data.constant.data.u8Val);
			break;
		case U16:
			fprintf(out, " = copy %u", term->data.constant.data.u16Val);
			break;
		case U32:
			fprintf(out, " = copy %u", term->data.constant.data.u32Val);
			break;
		case U64:
			fprintf(out, " = copy %" PRIu64, term->data.constant.data.u64Val);
			break;
		case F32:
			memcpy(&bits, &term->data.constant.data.f32Val, sizeof(uint32_t));
			fprintf(out, " = copy %" PRIu64, bits);
			break;
		case F64:
			memcpy(&bits, &term->data.constant.data.f64Val, sizeof(uint64_t));
			fprintf(out, " = copy %" PRId64, (int64_t)bits);
			break;
		default:
			logFatal("Not a numeric type: %d.", term->data.constant.numericType.kind);
		}
		fprintf(out, "\n");
		break;

	case BINARY_OP:
		result = newTemp();

		if (term->data.binOp.kind == AND)
		{
			u64 bLabel = newLabel();
			u64 falseLabel = newLabel();
			u64 trueLabel = newLabel();
			u64 endLabel = newLabel();

			u64 leftVal = emitExpression(term->data.binOp.a);
			inst("jnz " TEMP ", " LABEL ", " LABEL, leftVal, bLabel, falseLabel);
			inst(LABEL, bLabel);
			u64 rightVal = emitExpression(term->data.binOp.b);
			inst("jnz " TEMP ", " LABEL ", " LABEL, rightVal, trueLabel, falseLabel);
			inst(LABEL, falseLabel);
			inst(TEMP " =w copy 0", result);
			inst("jmp " LABEL, endLabel);
			inst(LABEL, trueLabel);
			inst(TEMP " =w copy 1", result);
			inst(LABEL, endLabel);
		} else if (term->data.binOp.kind == OR)
		{
			u64 bLabel = newLabel();
			u64 falseLabel = newLabel();
			u64 trueLabel = newLabel();
			u64 endLabel = newLabel();

			u64 leftVal = emitExpression(term->data.binOp.a);
			inst("jnz " TEMP ", " LABEL ", " LABEL, leftVal, trueLabel, bLabel);
			inst(LABEL, bLabel);
			u64 rightVal = emitExpression(term->data.binOp.b);
			inst("jnz " TEMP ", " LABEL ", " LABEL, rightVal, trueLabel, falseLabel);
			inst(LABEL, falseLabel);
			inst(TEMP " =w copy 0", result);
			inst("jmp " LABEL, endLabel);
			inst(LABEL, trueLabel);
			inst(TEMP " =w copy 1", result);
			inst(LABEL, endLabel);
		} else
		{
			u64 left = emitExpression(term->data.binOp.a);
			u64 right = emitExpression(term->data.binOp.b);
			indent();
			fprintf(out, TEMP " =w ", result);
			bool sign = isSignedType(term->data.binOp.a->type);
			bool dot = isFloatType(term->data.binOp.a->type);
			switch (term->data.binOp.kind)
			{
			case ADD:
				fprintf(out, "add");
				break;
			case SUBTRACT:
				fprintf(out, "sub");
				break;
			case MULTIPLY:
				fprintf(out, "mul");
				break;
			case DIVIDE:
				if (sign)
					fprintf(out, "div");
				else
					fprintf(out, "udiv");
				break;
			case REMAINDER:
				if (sign)
					fprintf(out, "rem");
				else
					fprintf(out, "urem");
				break;

			case EQUAL:
				fprintf(out, "eq");
				break;
			case NOT_EQUAL:
				fprintf(out, "ne");
				break;

			case LESS_THAN:
				if (sign)
					fprintf(out, "slt");
				else if (dot)
					fprintf(out, "lt");
				else
					fprintf(out, "ult");
				break;
			case LESS_OR_EQUAL:
				if (sign)
					fprintf(out, "sle");
				else if (dot)
					fprintf(out, "le");
				else
					fprintf(out, "ule");
				break;
			case GREATER_THAN:
				if (sign)
					fprintf(out, "sgt");
				else if (dot)
					fprintf(out, "gt");
				else
					fprintf(out, "ugt");
				break;
			case GREATER_OR_EQUAL:
				if (sign)
					fprintf(out, "sge");
				else if (dot)
					fprintf(out, "ge");
				else
					fprintf(out, "uge");
				break;
			default:
				logFatal("Unreachable.");
				break;
			}
			fprintf(out, TEMP ", " TEMP "\n", left, right);
		}
		break;

	case APPLICATION:
		result = newTemp();
		u64* args = calloc(term->data.app._argCount, sizeof(u64));
		if (args == NULL)
		{
			logFatal("Could not allocate enough memory for emitter.");
		}
		for (size i = 0; i < term->data.app._argCount; i++)
		{
			args[i] = emitExpression(&term->data.app.args[i]);
		}
		string name = toString(&term->data.app.fun->data.var.name);
		indent();
		fprintf(out, "call $%.*s (", (int)name.length, name.chars);
		for (size i = 0; i < term->data.app._argCount; i++)
		{
			fprintf(out, "%s " TEMP, typeToQbe(term->data.app.args[i].type), args[i]);
			if (i + 1 < term->data.app._argCount)
			{
				fprintf(out, ", ");
			}
		}
		fprintf(out, ")\n");
		break;
		
	case BLOCK:
		result = newTemp();
		for (size i = 0; i < term->data.block._stmtCount; i++)
		{
			Statement stmt = term->data.block.stmts[i];
			if (stmt.kind == UNIT_EXPRESSION)
			{
				emitExpression(stmt.data.unit_expression);
			} else
			{
				logFatal("TODO: handle variable declarations.");
			}
		}
		if (term->data.block.exp != NULL)
		{
			result = emitExpression(term->data.block.exp);
		}
		break;

	// case BOOLEAN:
	// case ARRAY:
	// case STRUCTURE:
	// case STRING:
	// case VAR:
	// case REF:
	// case DEREF:
	// case CAST:
	// case TYPED:
	// case BREAK:
	// case CONTINUE:
	// case UNARY_OP:
	// case BINARY_OP_ASSIGN:
	// case SUBSCRIPT:
	// case ACCESS:
	// case CONDITIONAL:
	// case LOOP:
	// case FUNCTION:
	// case ASSIGNMENT:
	// 	break;
	default:
		logFatal("Emitting of term kind %s not yet implemented.", termKindToString(term->kind));
	}
	return result;
}

void emitDeclaration(DeclarationData* decl)
{
	if (decl->exp == NULL)
	{
		return;
	}
	if (decl->exp->kind == FUNCTION)
	{
		if (decl->name.isAuto)
		{
			fprintf(out, "function w $auto_%llu ()\n", decl->name.data.intData);
		}
		else
		{
			string name = toString(&decl->name);
			fprintf(out, "export function w $%.*s ()\n", (int)name.length, name.chars);
		}
		fprintf(out, "{\n");
		down();
		fprintf(out, "    @start\n");
		u64 id = emitExpression(decl->exp->data.fun.body);
		fprintf(out, "    @return\n");
		fprintf(out, "    ret " TEMP "\n", id);
		up();
		fprintf(out, "}\n");
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
