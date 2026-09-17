#ifndef EMITER_H
#define EMITTER_H
#endif

#include "syntax.c"

void emitProgram(Program* program, char* out_path, u64 maxId);

#if __INCLUDE_LEVEL__ == 0

#include <inttypes.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>

#include "log.c"

// TODO: How to handle it, if this were to literally be the name of a variable?
#define TEMPF "%%__temp__%llu"
#define LABELF "@__label__%llu"
#define TYPEF ":__struct__%llu"
#define TYPEF_LEN 11
#define GLOBALF "$__global__%llu"

typedef struct
{
	char* buffer;
	size count;
	size capacity;
} CharBuffer;

void bufferAppend(CharBuffer* buf, const char* content)
{
    while (*content)
    {
        if (buf->count + 1 >= buf->capacity)
        {
            size_t newCapacity = buf->capacity
                ? buf->capacity * 2
                : 64;
            char* newBuffer = realloc(buf->buffer, newCapacity);
            if (newBuffer == NULL)
            {
                logFatal("could not allocate enough memory for emitter.");
            }
            buf->buffer = newBuffer;
            buf->capacity = newCapacity;
        }
        buf->buffer[buf->count++] = *content++;
    }
    buf->buffer[buf->count] = '\0';
}

void bufferPrint(FILE* file, CharBuffer* buf)
{
	fprintf(file, "%.*s", (int)buf->count, buf->buffer);
}

CharBuffer typeBuffer;
CharBuffer globalBuffer;
CharBuffer declBuffer;

int indent_depth = 0;
void indent(void)
{
	for (int i = 0; i < indent_depth; i++)
	{
		bufferAppend(&declBuffer, "    ");
	}
}
void down(void){
	indent_depth++;
}
void up(void){
	indent_depth--;
}

void declf(char* fmt, ...)
{
	va_list args;
	va_start(args, fmt);
	char temp[512];
	vsnprintf(temp, sizeof(temp), fmt, args);
	bufferAppend(&declBuffer, temp);
	va_end(args);
}
void typef(char* fmt, ...)
{
	va_list args;
	va_start(args, fmt);
	char temp[512];
	vsnprintf(temp, sizeof(temp), fmt, args);
	bufferAppend(&typeBuffer, temp);
	va_end(args);
}
void globalf(char* fmt, ...)
{
	va_list args;
	va_start(args, fmt);
	char temp[512];
	vsnprintf(temp, sizeof(temp), fmt, args);
	bufferAppend(&globalBuffer, temp);
	va_end(args);
}

void inst(char* fmt, ...)
{
	va_list args;
	va_start(args, fmt);
	indent();
	char temp[512];
	vsnprintf(temp, sizeof(temp), fmt, args);
	bufferAppend(&declBuffer, temp);
	bufferAppend(&declBuffer, "\n");
	va_end(args);
}

typedef struct
{
	enum SymbolKind {GLOBAL, LOCAL, TYPE} kind;
	bool isAuto;
	union {
		u64 id;
		struct {
			const char* chars;
			size len;
		} string;
		struct {
			const char* chars;
			size len;
			bool is_builtin;
		} type_string;
	} as;
} Symbol;

// 0 is an invalid temporary
static u64 maxLocal = 0;
Symbol newLocal(void)
{
	return (Symbol){LOCAL, true, {.id = ++maxLocal}};
}
u64 newLabel(void)
{
	static u64 maxLabel = 0;
	return ++maxLabel;
}
Symbol newStructType(void)
{
	static u64 maxStruct = 0;
	return (Symbol){TYPE, true, {.id = ++maxStruct}};
}
Symbol newGlobal(void)
{
	static u64 maxGlobal = 0;
	return (Symbol){GLOBAL, true, {.id = ++maxGlobal}};
}

void write_globalsym(Symbol sym)
{
	if (sym.isAuto)
	{
		declf(GLOBALF, sym.as.id);
	} else
	{
		declf("$%.*s", (int)sym.as.string.len, sym.as.string.chars);
	}
}
void write_localsym(Symbol sym)
{
	if (sym.isAuto)
	{
		declf(TEMPF, sym.as.id);
	} else
	{
		declf("%%%.*s", (int)sym.as.string.len, sym.as.string.chars);
	}
}
void write_typesym(Symbol sym)
{
	if (sym.isAuto)
	{
		declf(TYPEF, sym.as.id);
	} else 
	{
		if (sym.as.type_string.is_builtin)
		{
			declf("%.*s", (int)sym.as.type_string.len, sym.as.type_string.chars);
		} else
		{
			declf(":%.*s", (int)sym.as.type_string.len, sym.as.type_string.chars);
		}
	}
}
void write_sym(Symbol sym)
{
	if (sym.kind == GLOBAL)
	{
		write_globalsym(sym);
	} else if (sym.kind == LOCAL)
	{
		write_localsym(sym);
	} else
	{
		write_typesym(sym);
	}
}

Symbol idToSymbol(const identifier* id)
{
	if (id->isAuto)
	{
		return (Symbol){LOCAL, true, {.id = id->data.intData}};
	} else
	{
		const char* chars = id->data.stringData.charData;
		size len = id->data.stringData.length;
		return (Symbol){GLOBAL, false, {.string = {chars, len}}};
	}
}

Symbol type_sb = (Symbol){TYPE, false, {.type_string = {"sb", 2, true}}};
Symbol type_ub = (Symbol){TYPE, false, {.type_string = {"ub", 2, true}}};
Symbol type_sh = (Symbol){TYPE, false, {.type_string = {"sh", 2, true}}};
Symbol type_uh = (Symbol){TYPE, false, {.type_string = {"uh", 2, true}}};
Symbol type_w  = (Symbol){TYPE, false, {.type_string = {"w",  2, true}}};
Symbol type_l  = (Symbol){TYPE, false, {.type_string = {"l",  2, true}}};
Symbol type_s  = (Symbol){TYPE, false, {.type_string = {"s",  2, true}}};
Symbol type_d  = (Symbol){TYPE, false, {.type_string = {"d",  2, true}}};


bool isUnitType(const Type* t)
{
	if (t->kind == STRUCT_TYPE && t->data.structure._memberCount == 0)
		 return true;
	else return false;
}

Symbol convertType(const Type* t)
{
	switch (t->kind)
	{
		case BOOL: return type_ub;
		case I8:   return type_sb;
		case U8:   return type_ub;
		case I16:  return type_sh;
		case U16:  return type_uh;
		case I32:  return type_w;
		case U32:  return type_w;
		case I64:  return type_l;
		case U64:  return type_l;
		case F32:  return type_s;
		case F64:  return type_d;

		case STRUCT_TYPE:
			if (isUnitType(t))
			{
				logFatal("Can not emit anything for unit type. This is a compiler error.");
			}
			Symbol sym = newStructType();
			typef("type " TYPEF " = {}\n", sym.as.id);
			return sym;

		case FUN_TYPE:
			logFatal("All lambdas are already lifted.");
			break;

		default:
			logFatal("Typekind %d not yet implemented in emitter.", t->kind);
			break;
	}
}

Symbol emitExpression(Term* term);

void emitDeclaration(const identifier* name, Term* exp)
{
	Type* type = exp->type;
	Symbol sym = emitExpression(exp);
	if (!isUnitType(type))
	{
		indent();
		write_sym(idToSymbol(name));
		declf(" =");
		write_typesym(convertType(type));
		declf(" copy ");
		write_sym(sym);
		declf("\n");
	}
}

Symbol emitExpression(Term* term)
{
	Symbol result;
	switch (term->kind)
	{
	case RETURN:
		if (isUnitType(term->data.retur.exp->type))
		{
			inst("ret\n");
		} else
		{
 			result = emitExpression(term->data.retur.exp);
 			declf("ret ");
 			write_typesym(result);
 			declf("\n");
		}
		break;

	case BOOLEAN: 
		result = newLocal();
		indent();
		write_localsym(result);
		if (term->data.boolean.boolVal)
		{
			declf(TEMPF " =w copy 1\n", result);
		} else
		{
			declf(TEMPF " =w copy 0\n", result);
		}
		break;
		
	case CONSTANT:
		result = newLocal();
		indent();
		write_localsym(result);
		uint64_t bits;
		switch (term->data.constant.numericType.kind)
		{
		case I8:
			declf(" =w copy %d", term->data.constant.data.i8Val);
			break;
		case I16:
			declf(" =w copy %d", term->data.constant.data.i16Val);
			break;
		case I32:
			declf(" =w copy %d", term->data.constant.data.i32Val);
			break;
		case I64:
			declf(" =l copy %" PRIu64, term->data.constant.data.i64Val);
			break;
		case U8:
			declf(" =w copy %u", term->data.constant.data.u8Val);
			break;
		case U16:
			declf(" =w copy %u", term->data.constant.data.u16Val);
			break;
		case U32:
			declf(" =w copy %u", term->data.constant.data.u32Val);
			break;
		case U64:
			declf(" =l copy %" PRIu64, term->data.constant.data.u64Val);
			break;
		case F32:
			memcpy(&bits, &term->data.constant.data.f32Val, sizeof(uint32_t));
			declf(" =s copy %" PRIu64, bits);
			break;
		case F64:
			memcpy(&bits, &term->data.constant.data.f64Val, sizeof(uint64_t));
			declf(" =d copy %" PRId64, (int64_t)bits);
			break;
		default:
			logFatal("Not a numeric type: %d.", term->data.constant.numericType.kind);
		}
		declf("\n");
		break;

	case STRUCTURE:
		if (isUnitType(term->type))
		{
			break;
		}
		result = newGlobal();
		// TODO: proper struct handling
		globalf("data " GLOBALF " = {}\n", result.as.id);
		break;

	case VAR:
		result = idToSymbol(&term->data.var.name);
		break;
		
	case BINARY_OP:
		result = newLocal();

		if (term->data.binOp.kind == AND)
		{
			u64 bLabel = newLabel();
			u64 falseLabel = newLabel();
			u64 trueLabel = newLabel();
			u64 endLabel = newLabel();

			Symbol leftVal = emitExpression(term->data.binOp.a);
			declf("jnz ");
			write_sym(leftVal);
			declf(", " LABELF ", " LABELF "\n", bLabel, falseLabel);
			inst(LABELF, bLabel);
			Symbol rightVal = emitExpression(term->data.binOp.b);
			declf("jnz ");
			write_sym(rightVal);
			declf(", " LABELF ", " LABELF "\n", rightVal, trueLabel, falseLabel);
			inst(LABELF, falseLabel);
			inst(TEMPF " =w copy 0", result);
			inst("jmp " LABELF, endLabel);
			inst(LABELF, trueLabel);
			inst(TEMPF " =w copy 1", result);
			inst(LABELF, endLabel);
		} else if (term->data.binOp.kind == OR)
		{
			u64 bLabel = newLabel();
			u64 falseLabel = newLabel();
			u64 trueLabel = newLabel();
			u64 endLabel = newLabel();

			Symbol leftVal = emitExpression(term->data.binOp.a);
			declf("jnz ");
			write_sym(leftVal);
			declf(", " LABELF ", " LABELF "\n", trueLabel, bLabel);
			inst(LABELF, bLabel);
			Symbol rightVal = emitExpression(term->data.binOp.b);
			declf("jnz ");
			write_sym(rightVal);
			declf(", " LABELF ", " LABELF "\n", rightVal, trueLabel, falseLabel);
			inst(LABELF, falseLabel);
			write_localsym(result);
			declf(" =w copy 0\n");
			inst("jmp " LABELF, endLabel);
			inst(LABELF, trueLabel);
			write_localsym(result);
			declf(" =w copy 1\n");
			inst(LABELF, endLabel);
		} else
		{
			Symbol left = emitExpression(term->data.binOp.a);
			Symbol right = emitExpression(term->data.binOp.b);
			indent();
			write_localsym(result);
			declf(TEMPF " =w ", result);
			bool sign = isSignedType(term->data.binOp.a->type);
			bool dot = isFloatType(term->data.binOp.a->type);
			switch (term->data.binOp.kind)
			{
			case ADD:
				declf("add");
				break;
			case SUBTRACT:
				declf("sub");
				break;
			case MULTIPLY:
				declf("mul");
				break;
			case DIVIDE:
				if (sign)
					declf("div");
				else
					declf("udiv");
				break;
			case REMAINDER:
				if (sign)
					declf("rem");
				else
					declf("urem");
				break;

			case EQUAL:
				declf("eq");
				break;
			case NOT_EQUAL:
				declf("ne");
				break;

			case LESS_THAN:
				if (sign)
					declf("slt");
				else if (dot)
					declf("lt");
				else
					declf("ult");
				break;
			case LESS_OR_EQUAL:
				if (sign)
					declf("sle");
				else if (dot)
					declf("le");
				else
					declf("ule");
				break;
			case GREATER_THAN:
				if (sign)
					declf("sgt");
				else if (dot)
					declf("gt");
				else
					declf("ugt");
				break;
			case GREATER_OR_EQUAL:
				if (sign)
					declf("sge");
				else if (dot)
					declf("ge");
				else
					declf("uge");
				break;
			default:
				logFatal("Unreachable.");
				break;
			}
			declf(" ");
			write_sym(left);
			declf(", ");
			write_sym(right);
			declf("\n");
		}
		break;

	case ASSIGNMENT:
		if (term->data.assignment.lvalue->kind == VAR)
		{
			emitDeclaration(&term->data.assignment.lvalue->data.var.name, term->data.assignment.value);
		}
		break;

	case APPLICATION:
		result = newLocal();
		Symbol* args = calloc(term->data.app._argCount, sizeof(Symbol));
		if (args == NULL)
		{
			logFatal("Could not allocate enough memory for emitter.");
		}
		for (size i = 0; i < term->data.app._argCount; i++)
		{
			args[i] = emitExpression(&term->data.app.args[i]);
		}
		string name = toString(&term->data.app.fun->data.var.name);
		if (!isUnitType(term->type))
		{
			indent();
			write_localsym(result);
			declf(" =");
			write_typesym(convertType(term->type));
			declf(" ");
		} else
		{
			indent();
		}
		declf("call $%.*s (", (int)name.len, name.chars);
		for (size i = 0; i < term->data.app._argCount; i++)
		{
			Type* type = term->data.app.args[i].type;
			if (isUnitType(type))
			{
				continue;
			}
			write_typesym(convertType(type));
			declf(" ");
			write_sym(args[i]);
			if (i + 1 < term->data.app._argCount)
			{
				declf(", ");
			}
		}
		declf(")\n");
		break;
		
	case BLOCK:
		for (size i = 0; i < term->data.block._stmtCount; i++)
		{
			Statement stmt = term->data.block.stmts[i];
			if (stmt.kind == UNIT_EXPRESSION)
			{
				emitExpression(stmt.data.unit_expression);
			} else
			{
				emitDeclaration(&stmt.data.declaration.name, stmt.data.declaration.exp);
			}
		}
		if (term->data.block.exp != NULL)
		{
			Symbol sym = emitExpression(term->data.block.exp);
			if (!isUnitType(term->data.block.exp->type))
			{
				result = sym;
			}
		}
		break;

	// case ARRAY:
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
	// 	break;
	default:
		logFatal("Emitting of term kind %s not yet implemented.", termKindToString(term->kind));
	}
	return result;
}

void emitGlobalDeclaration(DeclarationData* decl)
{
	if (decl->exp == NULL)
	{
		return;
	}
	if (decl->exp->kind == FUNCTION)
	{
		Type* type = decl->type->data.fun.retType;
		bool proper = !isUnitType(type);
		if (decl->name.isAuto)
		{
			declf("function ");
			if (proper)
			{
				write_typesym(convertType(type));
				declf(" ");
			}
			declf(" $auto_%llu ()\n", decl->name.data.intData);
		}
		else
		{
			string name = toString(&decl->name);
			declf("export function ");
			if (proper)
			{
				write_typesym(convertType(type));
				declf(" ");
			}
			declf("$%.*s ()\n", (int)name.len, name.chars);
		}
		declf("{\n");
		down();
		declf("    @start\n");
		Symbol sym = emitExpression(decl->exp->data.fun.body);
		
		declf("    @return\n");
		if (proper)
		{
			declf("    ret ");
			write_sym(sym);
			declf("\n");
		} else
		{
			declf("    ret\n");
		}
		
		up();
		declf("}\n");
	}
}

void emitProgram(Program* program, char* path, u64 maxId)
{
	maxLocal = maxId;
	
	for (size i = 0; i < program->_declCount; i++)
	{
		emitGlobalDeclaration(&program->decls[i]);
	}

	if (path != NULL)
	{
		FILE* file = fopen(path, "w");
		if (file == NULL)
		{
			logFatal("Could not open file %s.", path);
		}
		bufferPrint(file, &typeBuffer);
		bufferPrint(file, &globalBuffer);
		bufferPrint(file, &declBuffer);
		fclose(file);
	} else
	{
		bufferPrint(stdout, &typeBuffer);
		bufferPrint(stdout, &globalBuffer);
		bufferPrint(stdout, &declBuffer);
	}
}

#endif
