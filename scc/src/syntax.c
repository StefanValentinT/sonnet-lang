#ifndef SYNTAX_H
#define SYNTAX_H

#include "dynarray.c"
#include "lexer.c"
#include <stdbool.h>
#include <stdint.h>

// The AST definition
// Fields prefixed with an underscore indicate
// that the field is not part of the Abstract Syntax
// but merely there for implementation reasons
// (e. g. we always store the length of a string together with the string)

typedef struct
{
	int line;
} SourceInfo;

typedef struct
{
	bool isAuto;
	union
	{
		u64 intData;
		struct
		{
			const char* charData;
			size length;
		} stringData;
	} data;
} identifier;

typedef struct
{
	const char* chars;
	size length;
} string;

identifier newIdent(void);
u64 getMaxId(void);
identifier makeIdent(const char* content, size length);
bool isEqualIdent(const identifier* id1, const identifier* id2);
u64 toAutoID(const identifier* id);
string toString(const identifier* id);
void printIdent(identifier ident);

// Types

typedef enum
{
	TYPE_VAR,
	
	I8,
	I16,
	I32,
	I64,
	U8,
	U16,
	U32,
	U64,
	F32,
	F64,
	BOOL,

	POINTER_TYPE,
	ARRAY_TYPE,
	FUN_TYPE,
	STRUCT_TYPE,
} TypeKind;

typedef struct Type Type;

typedef struct
{
	Type* elemType;
	size* elemCount; // nullable
} ArrayTypeData;

typedef struct
{
	Type* pointee;
} PointerTypeData;

typedef struct
{
	Type* paramTypes;
	size _paramCount;
	Type* retType;
} FunTypeData;

typedef struct
{
	identifier name;
	Type* type;
} MemberType;

typedef struct
{
	MemberType* memberTypes;
	size _memberCount;
	bool isUnion;
} StructTypeData;

struct Type
{
	TypeKind kind;
	union
	{
		size typeVar;

		PointerTypeData pointer;
		
		ArrayTypeData arr;
		FunTypeData fun;
		StructTypeData structure;
	} data;
};

// Terms

typedef enum
{
	CONSTANT,
	BOOLEAN,
	ARRAY,
	STRUCTURE,
	STRING,
	VAR,
	REF,
	DEREF,
	CAST,
	TYPED,
	RETURN,
	BREAK,    // no data
	CONTINUE, // no data
	UNARY_OP,
	BINARY_OP,
	BINARY_OP_ASSIGN,
	SUBSCRIPT,
	ACCESS,
	CONDITIONAL,
	LOOP,
	APPLICATION,
	FUNCTION,
	ASSIGNMENT,
	BLOCK
} TermKind;

typedef enum
{
	ADD,
	SUBTRACT,
	MULTIPLY,
	DIVIDE,
	REMAINDER,

	EQUAL,
	NOT_EQUAL,
	LESS_THAN,
	LESS_OR_EQUAL,
	GREATER_THAN,
	GREATER_OR_EQUAL,

	AND,
	OR,
} BinaryOpKind;

typedef enum
{
	BIT_NOT,
	NOT,
	NEG,
	POST_INCREMENT,
	POST_DECREMENT,
	PRE_INCREMENT,
	PRE_DECREMENT,
} UnaryOpKind;

typedef enum
{
	DECLARATION,
	UNIT_EXPRESSION
} StatementKind;

typedef struct Term Term;
typedef struct Statement Statement;

typedef struct
{
	union
	{
		i8 i8Val;
		i16 i16Val;
		i32 i32Val;
		i64 i64Val;
		u8 u8Val;
		u16 u16Val;
		u32 u32Val;
		u64 u64Val;
		float f32Val;
		double f64Val;
	} data;
	Type numericType;
} ConstantData;

typedef struct{
	bool boolVal;
} BooleanData;

typedef struct
{
	ConstantData* constants;
	size _constantsCount;
} ArrayData;

typedef struct
{
	identifier name;
	Term* term;
} Member;

typedef struct
{
	bool isUnion;
	Member* members;
	size memberCount;
} StructData;

typedef struct
{
	identifier string;
} StringData;

typedef struct
{
	identifier name;
} VarData;

typedef struct
{
	Term* exp;
} RefData;

typedef struct
{
	Term* exp;
} DerefData;

typedef struct
{
	Term* exp;
	Type type;
} CastData;

typedef struct
{
	Term* exp;
	Type type;
} TypedData;

typedef struct
{
	Term* exp; // nullable
} ReturnData;

typedef struct
{
	BinaryOpKind kind;
	Term* a;
	Term* b;
} BinOpData;

// a *= val
typedef struct
{
	BinaryOpKind kind;
	Term* a;
	Term* value;
} BinOpAssignData;

typedef struct
{
	UnaryOpKind kind;
	Term* t;
} UnOpData;

typedef struct
{
	Term* term;
	Term* index;
} SubscriptData;

typedef struct
{
	Term* term;
	identifier member;
} AccessData;

typedef struct
{
	Term* cond;
	Term* thenBranch;
	Term* elseBranch; // nullable
} ConditionalData;

typedef struct
{
	Term* cond;
	Term* body;
} LoopData;

typedef struct
{
	Term* fun;
	Term* args;
	size _argCount;
} ApplicationData;

typedef struct
{
	identifier name;
	Type* type; // nullable
} Formal;

typedef struct
{
	identifier recBinder;
	Formal* formals;
	size _formalCount;
	Type* retType; // nullable
	Term* body;
} FunctionData;

typedef struct
{
	Term* lvalue;
	Term* value;
} AssignmentData;

typedef struct
{
	Statement* stmts;
	size _stmtCount;
	Term* exp; // nullable
} BlockData;

struct Term
{
	TermKind kind;
	union
	{
		ConstantData constant;
		BooleanData boolean;
		
		ArrayData array;
		StringData string;
		StructData structure;

		VarData var;

		RefData ref;
		DerefData deref;
		CastData cast;

		TypedData typed;
		ReturnData retur;

		BinOpData binOp;
		BinOpAssignData binAssignOp;
		UnOpData unOp;
		SubscriptData subscript;
		AccessData access;

		ConditionalData cond;
		LoopData loop;

		ApplicationData app;
		FunctionData fun;

		AssignmentData assignment;
		BlockData block;
	} data;
	SourceInfo info;

	Type* type;
};

// Statements

typedef struct
{
	identifier name;
	Type* type; // nullable
	bool mutable;
	Term* exp; // nullable
} DeclarationData;

struct Statement
{
	StatementKind kind;
	union
	{
		DeclarationData declaration;
		Term* unit_expression;
	} data;
	SourceInfo info;
};

// Whole Program

typedef struct
{
	DeclarationData* decls;
	size _declCount;
} Program;

// Allocation
Term* newTerm(Term t);
Type* newType(Type t);
Statement* newStatement(Statement s);
bool isEqualType(const Type* t1, const Type* t2);
bool isIntegralType(const Type* type);
bool isNumericType(const Type* type);
bool isSignedType(const Type* type);
bool isFloatType(const Type* type);

// Pretty printing
const char* termKindToString(TermKind kind);
void printType(const Type* t);
void printTerm(const Term* t);
void printStatement(const Statement* s);
void printProgram(const Program* p);

void printConstant(const ConstantData* t);
void printBoolean(const BooleanData* t);
void printArray(const ArrayData* a);
void printString(const StringData* s);
void printVar(const VarData* s);
void printCast(const CastData* c);
void printTyped(const TypedData* c);
void printBinaryOp(const BinOpData* b);
void printApplication(const ApplicationData* a);
void printFunction(const FunctionData* f);
void printAssignment(const AssignmentData* a);
void printBlock(const BlockData* b);

void printDeclaration(const DeclarationData* d);

#endif
#if __INCLUDE_LEVEL__ == 0

#include "log.c"
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static u64 identCount = 0;

identifier newIdent(void)
{
	identifier ident = (identifier){true, {.intData = identCount++}};
	return ident;
}

u64 getMaxId(void)
{
	return identCount;
}

identifier makeIdent(const char* content, size_t length)
{
	identifier id = {false, {.stringData = {content, length}}};
	return id;
}

bool isEqualIdent(const identifier* id1, const identifier* id2)
{
	if (id1->isAuto != id2->isAuto)
	{
		return false;
	}
	if (id1->isAuto)
	{
		return id1->data.intData == id2->data.intData;
	}
	else
	{
		if (id1->data.stringData.length != id2->data.stringData.length)
		{
			return false;
		}
		return memcmp(
		           id1->data.stringData.charData, id2->data.stringData.charData,
		           id1->data.stringData.length
		       ) == 0;
	}
}

u64 toAutoID(const identifier* id) { return id->data.intData; }

string toString(const identifier* id)
{
	return (string){id->data.stringData.charData, id->data.stringData.length};
}

void printIdent(identifier ident)
{
	if (ident.isAuto)
	{
		printf("auto#%" PRIuPTR, (uintptr_t)ident.data.intData);
	}
	else
	{
		printf("%.*s", (int)ident.data.stringData.length, ident.data.stringData.charData);
	}
}

bool isMemberTypesEqual(const MemberType* m1, const MemberType* m2)
{
	if (!isEqualIdent(&m1->name, &m2->name))
		return false;
	if (!isEqualType(m1->type, m2->type))
		return false;
	return true;
}

bool isIntegralType(const Type* type)
{
	switch (type->kind)
	{
	case I8:
	case I16:
	case I32:
	case I64:
	case U8:
	case U16:
	case U32:
	case U64:
		return true;

	default:
		return false;
	}
}

bool isNumericType(const Type* type)
{
	if (isIntegralType(type))
		return true;
	switch (type->kind)
	{
	case F32:
	case F64:
		return true;

	default:
		return false;
	}
}

bool isSignedType(const Type* type)
{
	switch (type->kind)
	{
		case I8:
		case I16:
		case I32:
		case I64:
			return true;

		default:
			return false;
	}
}

bool isFloatType(const Type* type)
{
	switch (type->kind)
	{
		case F32:
		case F64:
			return true;

		default:
			return false;
	}
}

bool isEqualType(const Type* t1, const Type* t2)
{
	size count;
	if (t1->kind != t2->kind)
		return false;
	switch (t1->kind)
	{
	case TYPE_VAR:
		return t1->data.typeVar == t2->data.typeVar;

	case BOOL:
	case I8:
	case I16:
	case I32:
	case I64:
	case U8:
	case U16:
	case U32:
	case U64:
	case F32:
	case F64:
		return true;
		
	case POINTER_TYPE:
		return isEqualType(
			t1->data.pointer.pointee,
			t2->data.pointer.pointee
		);

	case ARRAY_TYPE:
		if (t1->data.arr.elemCount != NULL && t2->data.arr.elemCount != NULL)
		{
			if (*t1->data.arr.elemCount != *t2->data.arr.elemCount)
				return false;
		}
		if (!isEqualType(t1->data.arr.elemType, t2->data.arr.elemType))
			return false;
		return true;

	case FUN_TYPE:
		count = t1->data.fun._paramCount;
		if (count != t2->data.fun._paramCount)
			return false;
		for (size i = 0; i < count; i++)
		{
			if (!isEqualType(&t1->data.fun.paramTypes[i], &t2->data.fun.paramTypes[i]))
				return false;
		}
		if (!isEqualType(t1->data.fun.retType, t2->data.fun.retType))
			return false;
		return true;

	case STRUCT_TYPE:
		if (t1->data.structure.isUnion != t2->data.structure.isUnion)
			return false;
		count = t1->data.structure._memberCount;
		if (count != t2->data.structure._memberCount)
			return false;
		for (size i = 0; i < count; i++)
		{
			if (!isMemberTypesEqual(
			        &t1->data.structure.memberTypes[i], &t2->data.structure.memberTypes[i]
			    ))
				return false;
		}
		return true;
	}
}

void printType(const Type* t)
{
    switch (t->kind)
	{
	case TYPE_VAR:
		printf("tvar#%ld", t->data.typeVar);
		break;
	case I8:
		printf("i8");
		break;
	case I16:
		printf("i16");
		break;
	case I32:
		printf("i32");
		break;
	case I64:
		printf("i64");
		break;
	case U8:
		printf("u8");
		break;
	case U16:
		printf("u16");
		break;
	case U32:
		printf("u32");
		break;
	case U64:
		printf("u64");
		break;
	case F32:
		printf("f32");
		break;
	case F64:
		printf("f64");
		break;
	case BOOL:
		printf("bool");
		break;

	case POINTER_TYPE:
		printf("(POINTER ");
		printType(t->data.pointer.pointee);
		printf(")");
		break;
		
	case ARRAY_TYPE:
		printf("(ARRAY_TYPE ");
		printType(t->data.arr.elemType);
		printf("of size %" PRIuPTR ")", *t->data.arr.elemCount);
		break;
		
	case FUN_TYPE:
		printf("(FUN_TYPE ");
		for (size i = 0; i < t->data.fun._paramCount; i++)
		{
			printType(&t->data.fun.paramTypes[i]);
			printf(" ");
		}
		printf("-> ");
		printType(t->data.fun.retType);
		printf(")");
		break;
		
	case STRUCT_TYPE:
		if (t->data.structure.isUnion)
		{
			printf("(UNION");
		}
		else
		{
			printf("(STRUCT");
		}
		for (size i = 0; i < t->data.structure._memberCount; i++)
		{
			printf(" ");
			MemberType m = t->data.structure.memberTypes[i];
			printIdent(m.name);
			printf(": ");
			printType(m.type);
		}
		printf(")");
	}
}

const char* termKindToString(TermKind kind)
{
	switch (kind)
	{
	case CONSTANT:
		return "CONSTANT";
	case BOOLEAN:
		return "BOOLEAN";
	case ARRAY:
		return "ARRAY";
	case STRUCTURE:
		return "STRUCTURE";
	case STRING:
		return "STRING";
	case VAR:
		return "VAR";
	case REF:
		return "REF";
	case DEREF:
		return "DEREF";
	case CAST:
		return "CAST";
	case TYPED:
		return "TYPED";
	case RETURN:
		return "RETURN";
	case BREAK:
		return "BREAK";
	case CONTINUE:
		return "CONTINUE";
	case UNARY_OP:
		return "UNARY_OP";
	case BINARY_OP:
		return "BINARY_OP";
	case BINARY_OP_ASSIGN:
		return "BINARY_OP_ASSIGN";
	case SUBSCRIPT:
		return "SUBSCRIPT";
	case ACCESS:
		return "ACCESS";
	case CONDITIONAL:
		return "CONDITIONAL";
	case LOOP:
		return "LOOP";
	case APPLICATION:
		return "APPLICATION";
	case FUNCTION:
		return "FUNCTION";
	case ASSIGNMENT:
		return "ASSIGNMENT";
	case BLOCK:
		return "BLOCK";
	}

	return "UNKNOWN_TERM";
}

void printConstant(const ConstantData* t)
{
	switch (t->numericType.kind)
	{
	case I8:
		printf("(CONSTANT %" PRId8 ")", t->data.i8Val);
		break;
	case I16:
		printf("(CONSTANT %" PRId16 ")", t->data.i16Val);
		break;
	case I32:
		printf("(CONSTANT %" PRId32 ")", t->data.i32Val);
		break;
	case I64:
		printf("(CONSTANT %" PRId64 ")", t->data.i64Val);
		break;
	case U8:
		printf("(CONSTANT %" PRIu8 ")", t->data.u8Val);
		break;
	case U16:
		printf("(CONSTANT %" PRIu16 ")", t->data.u16Val);
		break;
	case U32:
		printf("(CONSTANT %" PRIu32 ")", t->data.u32Val);
		break;
	case U64:
		printf("(CONSTANT %" PRIu64 ")", t->data.u64Val);
		break;
	case F32:
		printf("(CONSTANT %f)", t->data.f32Val);
		break;
	case F64:
		printf("(CONSTANT %lf)", t->data.f64Val);
		break;
	default:
		printf("Not a constant!");
		break;
	}
}

void printBoolean(const BooleanData* t)
{
	t->boolVal ? printf("true") : printf("false");
}

void printArray(const ArrayData* a)
{
	printf("(ARRAY ");
	for (size i = 0; i < a->_constantsCount; i++)
	{
		printConstant(&a->constants[i]);
	}
	printf(")");
}

void printStructure(const StructData* s)
{
	if (s->isUnion)
	{
		printf("(UNION ");
	}
	else
	{
		printf("(STRUCT ");
	}
	for (size i = 0; i < s->memberCount; i++)
	{
		Member m = s->members[i];
		printIdent(m.name);
		printf(": ");
		printTerm(m.term);
	}
	printf(")");
}

void printString(const StringData* s)
{
	printf("\"");
	printIdent(s->string);
	printf("\"");
}

void printVar(const VarData* s) { printIdent(s->name); }

void printCast(const CastData* c)
{
	printf("(CAST ");
	printTerm(c->exp);
	printf(" as ");
	printType(&c->type);
	printf(")");
}

void printTyped(const TypedData* c)
{
	printf("(TYPED ");
	printTerm(c->exp);
	printf(" as ");
	printType(&c->type);
	printf(")");
}

const char* binOpToString(BinaryOpKind b)
{
	switch (b)
	{
	case ADD:
		return "ADD";
	case SUBTRACT:
		return "SUBTRACT";
	case MULTIPLY:
		return "MULTIPLY";
	case DIVIDE:
		return "DIVIDE";
	case REMAINDER:
		return "REMAINDER";
	case EQUAL:
		return "EQUAL";
		break;
	case NOT_EQUAL:
		return "NOT-EQUAL";
		break;
	case LESS_THAN:
		return "LT";
		break;
	case LESS_OR_EQUAL:
		return "LE";
		break;
	case GREATER_THAN:
		return "GT";
		break;
	case GREATER_OR_EQUAL:
		return "GE";
		break;
	case AND:
		return "AND";
	case OR:
		return "OR";
	}
}

void printBinaryOp(const BinOpData* b)
{
	printf("(%s ", binOpToString(b->kind));
	printTerm(b->a);
	printf(" ");
	printTerm(b->b);
	printf(")");
}

void printBinaryOpAssign(const BinOpAssignData* b)
{
	printf("(%s-ASSIGN ", binOpToString(b->kind));
	printTerm(b->a);
	printf(" ");
	printTerm(b->value);
	printf(")");
}

void printUnaryOp(const UnOpData* u)
{
	printf("(");
	switch (u->kind)
	{
	case PRE_DECREMENT:
		printf("PRE-DECREMENT");
		break;
	case PRE_INCREMENT:
		printf("PRE-INCREMENT");
		break;
	case POST_INCREMENT:
		printf("POST-INCREMENT");
		break;
	case POST_DECREMENT:
		printf("POST-DECREMENT");
		break;
	case NOT:
		printf("NOT");
		break;
	case BIT_NOT:
		printf("BIT-NOT");
		break;
	case NEG:
		printf("-");
	}
	printf(" ");
	printTerm(u->t);
	printf(")");
}

void printApplication(const ApplicationData* a)
{
	printf("(APP (");
	printTerm(a->fun);
	printf(") ");
	for (size i = 0; i < a->_argCount; i++)
	{
		printTerm(&a->args[i]);
		if (i < a->_argCount - 1)
		{
			printf(" ");
		}
	}
	printf(")");
}

void printFunction(const FunctionData* f)
{
	printf("(FUNCTION ");
	printIdent(f->recBinder);
	printf("(");
	for (size i = 0; i < f->_formalCount; i++)
	{
		Formal form = f->formals[i];
		printIdent(form.name);
		if (form.type)
		{
			printf(": ");
			printType(form.type);
		}
		if (i + 1 < f->_formalCount)
		{
			printf(", ");
		}
	}
	printf(")");
	if (f->retType != NULL)
	{
		printf(" ");
		printType(f->retType);
	}
	printf(" -> ");
	printTerm(f->body);
	printf(")");
}

void printAssignment(const AssignmentData* a)
{
	printf("(ASSIGN ");
	printTerm(a->lvalue);
	printf(" ");
	printTerm(a->value);
	printf(")");
}

void printConditional(const ConditionalData* c)
{
	printf("(IF ");
	printTerm(c->cond);
	printf(" THEN ");
	printTerm(c->thenBranch);
	if (c->elseBranch == NULL)
	{
		printf(")");
	} else
	{
		printf(" ELSE ");
		printTerm(c->elseBranch);
		printf(")");
	}
}

void printLoop(const LoopData* l)
{
	printf("(LOOP ");
	printTerm(l->cond);
	printf(" ");
	printTerm(l->body);
	printf(")");
}

static int level = 0;

static void indent(int n)
{
	if (n == 0)
		return;
	printf("    ");
	indent(n - 1);
}

void printBlock(const BlockData* b)
{
	printf("(BLOCK \n");
	level++;
	for (size i = 0; i < b->_stmtCount; i++)
	{
		indent(level);
		printStatement(&b->stmts[i]);
		printf("\n");
	}
	if (b->exp != NULL)
	{
		indent(level);
		printTerm(b->exp);
		printf("\n");
	}
	level--;
	indent(level);
	printf(")");
}

void printTerm(const Term* t)
{
	bool hasType = t->type != NULL;
	if (hasType)
	{
		printf("(");
	}
	
	switch (t->kind)
	{
	case CONSTANT:
		printConstant(&t->data.constant);
		break;
	case BOOLEAN:
		printBoolean(&t->data.boolean);
		break;
	case ARRAY:
		printArray(&t->data.array);
		break;
	case STRUCTURE:
		printStructure(&t->data.structure);
		break;
	case STRING:
		printString(&t->data.string);
		break;
	case VAR:
		printVar(&t->data.var);
		break;
	case REF:
		printf("(REF ");
		printTerm(t->data.ref.exp);
		printf(")");
		break;
	case DEREF:
		printf("(DEREF ");
		printTerm(t->data.deref.exp);
		printf(")");
		break;
	case CAST:
		printCast(&t->data.cast);
		break;
	case TYPED:
		printTyped(&t->data.typed);
		break;
	case RETURN:
		printf("(RETURN ");
		printTerm(t->data.retur.exp);
		printf(")");
		break;
	case BREAK:
		printf("BREAK");
		break;
	case CONTINUE:
		printf("CONTINUE");
		break;
	case BINARY_OP:
		printBinaryOp(&t->data.binOp);
		break;
	case BINARY_OP_ASSIGN:
		printBinaryOpAssign(&t->data.binAssignOp);
		break;
	case UNARY_OP:
		printUnaryOp(&t->data.unOp);
		break;
	case APPLICATION:
		printApplication(&t->data.app);
		break;
	case FUNCTION:
		printFunction(&t->data.fun);
		break;
	case ASSIGNMENT:
		printAssignment(&t->data.assignment);
		break;
	case BLOCK:
		printBlock(&t->data.block);
		break;
	case SUBSCRIPT:
		printf("(ACCESS ");
		printTerm(t->data.subscript.term);
		printf("[");
		printTerm(t->data.subscript.index);
		printf("]");
		printf(")");
		break;
	case ACCESS:
		printf("(ACCESS ");
		printTerm(t->data.access.term);
		printf(" . ");
		printIdent(t->data.access.member);
		printf(")");
		break;
	case CONDITIONAL:
		printConditional(&t->data.cond);
		break;
	case LOOP:
		printLoop(&t->data.loop);
		break;
	}
	if (hasType)
	{
		printf(" : ");
		printType(t->type);
		printf(")");
	}
}

void printDeclaration(const DeclarationData* d)
{
	if (d->mutable)
	{
		printf("(VAR ");
	}
	else
	{
		printf("(VAR ");
	}
	printIdent(d->name);
	if (d->type)
	{
		printf(" : ");
		printType(d->type);
	}
	if (d->exp)
	{
		printf(" = ");
		printTerm(d->exp);
	}
	printf(")");
}

void printStatement(const Statement* stmt)
{
	switch (stmt->kind)
	{
	case DECLARATION:
		printDeclaration(&stmt->data.declaration);
		break;
	case UNIT_EXPRESSION:
		printTerm(stmt->data.unit_expression);
		break;
	}
}

void printProgram(const Program* p)
{
	for (size i = 0; i < p->_declCount; i++)
	{
		printDeclaration(&p->decls[i]);
		printf("\n");
	}
}

Term* newTerm(Term t)
{
	Term* ptr = malloc(sizeof(Term));
	if (ptr == NULL)
	{
		logFatal("Could not allocate enough memory to make an AST Node.");
	}
	*ptr = t;
	return ptr;
}

Type* newType(Type t)
{
	Type* ptr = malloc(sizeof(Type));
	if (ptr == NULL)
	{
		logFatal("Could not allocate enough memory to make an AST Node.");
	}
	*ptr = t;
	return ptr;
}

Statement* newStatement(Statement t)
{
	Statement* ptr = malloc(sizeof(Statement));
	if (ptr == NULL)
	{
		logFatal("Could not allocate enough memory to make an AST Node.");
	}
	*ptr = t;
	return ptr;
}

#endif
