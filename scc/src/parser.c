#ifndef PARSER_H
#define PARSER_H

#include "syntax.c"
#include <stdint.h>

Program parse(char* source);

#endif
#if __INCLUDE_LEVEL__ == 0

#include "dynarray.c"
#include "hashmap.c"
#include "lexer.c"
#include "log.c"
#include "stdlib.h"
#include "syntax.c"
#include <ctype.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define expect(x) expectElse(x, "Expected " #x ".")
#define UNREACHABLE                                                                                \
	{                                                                                              \
		printf("UNREACHABLE reached in file %s:%d.\n", __FILE__, __LINE__);                        \
		abort();                                                                                   \
	}

// internal functions
Token expectElse(TokenKind k, char* msg);
Term parseTerm(void);
DeclarationData parseDeclaration(void);
Statement parseStatement(void);
Type parseType(void);

typedef struct
{
	identifier newName;
	bool mutable;
} ScopeData;

typedef struct Scope
{
	struct Scope* parent;
	// String -> ScopeData
	Map* vars;
} Scope;

Scope* scope = NULL;

// String -> Type
Map typeDefinitions;

Token expectElse(TokenKind k, char* msg)
{
	Token t = nextToken();
	if (t.kind == k)
	{
		return t;
	}
	else
	{
		logFatal(msg);
	}
}

void enterScope(void)
{
	Scope* newScope = malloc(sizeof(Scope));

	if (newScope == NULL)
	{
		logFatal("Could not allocate enough memory for new scope.");
	}

	newScope->vars = malloc(sizeof(Map));
	if (newScope->vars == NULL)
	{
		free(newScope);
		logFatal("Could not allocate enough memory for scope map.");
	}

	mapInit(newScope->vars);
	newScope->parent = scope;
	scope = newScope;
}

void leaveScope(void)
{
	if (scope == NULL)
	{
		logFatal("Can't leave global scope exists.");
	}

	Scope* oldScope = scope;
	scope = scope->parent;

	mapFree(oldScope->vars);
	free(oldScope->vars);
	free(oldScope);
}

ScopeData* lookup(const char* varName, size len)
{
	for (Scope* current = scope; current != NULL; current = current->parent)
	{
		ScopeData* val = mapGet(current->vars, varName, len);

		if (val != NULL)
			return val;
	}

	return NULL;
}

identifier declare(const char* varName, size len, bool mutable)
{
	identifier ident;
	if (mapHas(scope->vars, varName, len))
	{
		logFatal("Invalid redeclaration of %.*s in same scope.", len, varName);
	}

	bool isGlobal = scope->parent == NULL;
	if (isGlobal)
	{
		ident = makeIdent(varName, len);
	}
	else
	{
		ident = newIdent();
	}
	ScopeData* newScopeData = malloc(sizeof(ScopeData));
	if (newScopeData == NULL)
	{
		logFatal("Could not allocate identifier.");
	}
	*newScopeData = (ScopeData){.newName = ident, .mutable = mutable};
	mapPut(scope->vars, varName, len, newScopeData);
	return ident;
}

void printScopeData(void* val)
{
	ScopeData* scopeData = (ScopeData*)val;
	printf("{name: ");
	printIdent(scopeData->newName);
	printf(", mutable: %s}", scopeData->mutable ? "true" : "false");
}
void printScope(void)
{
	printf("--- Scope ---\n");
	for (Scope* current = scope; current != NULL; current = current->parent)
	{
		mapPrint(current->vars, printScopeData);
	}
	printf("-------------\n");
}

enum NumericBase
{
	OCTAL = 8,
	DECIMAL = 10,
	HEXADECIMAL = 16
};

signed char parseDigit(char c, enum NumericBase base)
{
	switch (c)
	{
	case '0':
		return 0;
	case '1':
		return 1;
	case '2':
		return 2;
	case '3':
		return 3;
	case '4':
		return 4;
	case '5':
		return 5;
	case '6':
		return 6;
	case '7':
		return 7;
	case '8':
		if (base == OCTAL)
			logFatal("Invalid digit '%c'.", c);
		return 8;
	case '9':
		if (base == OCTAL)
			logFatal("Invalid digit '%c'.", c);
		return 9;
	case 'a':
	case 'A':
		if (base == OCTAL || base == DECIMAL)
			logFatal("Invalid digit '%c'.", c);
		return 10;
	case 'b':
	case 'B':
		if (base == OCTAL || base == DECIMAL)
			logFatal("Invalid digit %c.", c);
		return 11;
	case 'c':
	case 'C':
		if (base == OCTAL || base == DECIMAL)
			logFatal("Invalid digit '%c'.", c);
		return 12;
	case 'd':
	case 'D':
		if (base == OCTAL || base == DECIMAL)
			logFatal("Invalid digit '%c'.", c);
		return 13;
	case 'e':
	case 'E':
		if (base == OCTAL || base == DECIMAL)
			logFatal("Invalid digit '%c'.", c);
		return 14;
	case 'f':
	case 'F':
		if (base == OCTAL || base == DECIMAL)
			logFatal("Invalid digit '%c'.", c);
		return 15;
	default:
		logFatal("Invalid digit '%c'.", c);
	}
}

double parseFloatingPoint(const char* start, size len, enum NumericBase base)
{
	double ret = 0.0, commaPart = 0.0;
	int32_t commaPartSize = 0;
	bool fractional = false;
	char c;
	for (size i = 0; i < len; i++)
	{
		c = start[i];
		if (c == '.')
		{
			if (!fractional)
			{
				fractional = true;
				continue;
			}
			else
			{
				logFatal("Number contains more than one decimal separator '.'.");
			}
		}
		if (!fractional)
		{
			ret = ret * base + parseDigit(c, base);
		}
		else
		{
			commaPart = commaPart * base + parseDigit(c, base);
			commaPartSize++;
		}
	}
	if (!fractional)
	{
		return ret;
	}
	else
	{
		return ret + (commaPart / pow(base, commaPartSize));
	}
}

Term parseNumber(Token numToken)
{
	ConstantData con;

	char* start = numToken.start;
	size len = numToken.len;

	TypeKind type;
	enum NumericBase base = DECIMAL;

	int8_t i8Val = 0;
	int16_t i16Val = 0;
	int32_t i32Val = 0;
	int64_t i64Val = 0;

	uint8_t u8Val = 0;
	uint16_t u16Val = 0;
	uint32_t u32Val = 0;
	uint64_t u64Val = 0;

	float f32Val = 0.0;
	double f64Val = 0.0;

	// The default type for integer literals is i32,
	// for floating-point f32
	if (numToken.hasDot)
	{
		type = F32;
	}
	else
	{
		type = I32;
	}

	switch (start[1])
	{
	case 'o':
	case 'O':
		if (*start != '0')
			logFatal("Missing prefix 0 for octal constant.");
		base = OCTAL;
		start += 2;
		len -= 2;
		break;
	case 'd':
	case 'D':
		if (*start != '0')
			logFatal("Missing prefix 0 for explictly decimal constant.");
		base = DECIMAL;
		start += 2;
		len -= 2;
		break;
	case 'x':
	case 'X':
		if (*start != '0')
			logFatal("Missing prefix 0 for hexadecimal constant.");
		base = HEXADECIMAL;
		start += 2;
		len -= 2;
		break;
	}

	if (start == NULL || len <= 0)
	{
		logFatal("No number literals constructed by the lexer are empty.");
	}
	if (len > 2)
	{
		char last = start[len - 1];
		char pen = start[len - 2];
		char penpen = start[len - 3];
		switch (pen)
		{
		case 'i':
		case 'u':
			if (last == '8')
			{
				type = (pen == 'i') ? I8 : U8;
				len -= 2;
			}
			else
			{
				logFatal("Invalid numeric type %.*s", 2, start + (len - 2));
			}
			break;
		case '1':
			if (last == '6' && (penpen == 'i' || penpen == 'u'))
			{
				type = (penpen == 'i') ? I16 : U16;
				len -= 3;
			}
			else
			{
				logFatal("Invalid numeric type %.*s", 3, start + (len - 3));
			}
			break;
		case '3':
			if (last == '2')
			{
				len -= 3;
				switch (penpen)
				{
				case 'i':
					type = I32;
					break;
				case 'u':
					type = U32;
					break;
				case 'f':
					type = F32;
					break;
				default:
					logFatal("Invalid numeric type %.*s", 3, start + (len - 3));
					break;
				}
			}
			else
			{
				logFatal("Invalid numeric type %.*s", 3, start + (len - 3));
			}
			break;
		case '6':
			if (last == '4')
			{
				len -= 3;
				switch (penpen)
				{
				case 'i':
					type = I64;
					break;
				case 'u':
					type = U64;
					break;
				case 'f':
					type = F64;
					break;
				default:
					logFatal("Invalid numeric type %.*s", 3, start + (len - 3));
					break;
				}
			}
			else
			{
				logFatal("Invalid numeric type %.*s", 3, start + (len - 3));
			}
			break;
		}
	}
	char c;
	switch (type)
	{
	case I8:
		for (size i = 0; i < len; i++)
		{
			c = start[i];
			i8Val = i8Val * (int8_t)base + parseDigit(c, base);
		}
		con = (ConstantData){{.i8Val = i8Val}, {I8, {0}}};
		break;
	case I16:
		for (size i = 0; i < len; i++)
		{
			c = start[i];
			i16Val = i16Val * (int16_t)base + parseDigit(c, base);
		}
		con = (ConstantData){{.i16Val = i16Val}, {I16, {0}}};
		break;
	case I32:
		for (size i = 0; i < len; i++)
		{
			c = start[i];
			i32Val = i32Val * (int32_t)base + parseDigit(c, base);
		}
		con = (ConstantData){{.i32Val = i32Val}, {I32, {0}}};
		break;
	case I64:
		for (size i = 0; i < len; i++)
		{
			c = start[i];
			i64Val = i64Val * base + parseDigit(c, base);
		}
		con = (ConstantData){{.i64Val = i64Val}, {I64, {0}}};
		break;
	case U8:
		for (size i = 0; i < len; i++)
		{
			c = start[i];
			u8Val = u8Val * (uint8_t)base + (uint8_t)parseDigit(c, base);
		}
		con = (ConstantData){{.u8Val = u8Val}, {U8, {0}}};
		break;
	case U16:
		for (size i = 0; i < len; i++)
		{
			c = start[i];
			u16Val = u16Val * (uint16_t)base + (uint16_t)parseDigit(c, base);
		}
		con = (ConstantData){{.u16Val = u16Val}, {U16, {0}}};
		break;
	case U32:
		for (size i = 0; i < len; i++)
		{
			c = start[i];
			u32Val = u32Val * (uint32_t)base + (unsigned)parseDigit(c, base);
		}
		con = (ConstantData){{.u32Val = u32Val}, {U32, {0}}};
		break;
	case U64:
		for (size i = 0; i < len; i++)
		{
			c = start[i];
			u64Val = u64Val * (uint64_t)base + (unsigned)parseDigit(c, base);
		}
		con = (ConstantData){{.u64Val = u64Val}, {U64, {0}}};
		break;
	case F32:
		f32Val = (float)parseFloatingPoint(start, len, base);
		con = (ConstantData){{.f32Val = f32Val}, {F32, {0}}};
		break;
	case F64:
		f64Val = parseFloatingPoint(start, len, base);
		con = (ConstantData){{.f64Val = f64Val}, {F64, {0}}};
		break;
	default:
		logFatal("Unreachable");
	}

	return (Term){CONSTANT, {.constant = con}, {numToken.line}, newType((Type){type, {0}})};
}

Term parseAtom(void)
{
	Token tok = nextToken();
	ScopeData* scopeData;
	SourceInfo s = (SourceInfo){tok.line};
	switch (tok.kind)
	{
	case TOK_IDENTIFIER:
		scopeData = lookup(tok.start, tok.len);
		if (!scopeData)
		{
			logFatal("Undefined variable %.*s in line %d.", tok.len, tok.start, tok.line);
		}
		return (Term){VAR, {.var = {scopeData->newName}}, s, NULL};
	case TOK_NUMBER:
		return parseNumber(tok);
	case TOK_STRING:
		return (Term){STRING, {.string = {makeIdent(tok.start, tok.len)}}, s, NULL};
	default:
		logFatal("Not an atom: %d.", tok.kind);
		abort();
	}
}

bool startsAtom(TokenKind tok)
{
	switch (tok)
	{
	case TOK_IDENTIFIER:
	case TOK_NUMBER:
	case TOK_STRING:
		return true;
	default:
		return false;
	}
}

bool startsFactor(TokenKind tok)
{
	switch (tok)
	{
	case TOK_LEFT_PAREN:
	case TOK_STRUCT:
	case TOK_UNION:
		return true;
	default:
		if (startsAtom(tok))
		{
			return true;
		}
		return false;
	}
}

DynArray parseNonEmptyInitList(void)
{
	DynArray initList;
	initList = initArray(4, sizeof(Member));

	while (true)
	{
		Token name = expect(TOK_IDENTIFIER);

		expect(TOK_EQUAL);

		Term* value = newTerm(parseTerm());
		Member* memberPtr = malloc(sizeof(Member));
		*memberPtr = (Member){makeIdent(name.start, name.len), value};

		appendArray(&initList, memberPtr);

		if (peekToken().kind == TOK_COMMA)
		{
			nextToken();
		}
		else
		{
			break;
		}
	}
	return initList;
}

DynArray parseNonEmptyStructTypesList(void)
{
	DynArray initList;
	initList = initArray(4, sizeof(MemberType));

	while (true)
	{
		Token name = expect(TOK_IDENTIFIER);

		expect(TOK_COLON);

		Type* value = newType(parseType());
		MemberType* memberPtr = malloc(sizeof(MemberType));
		*memberPtr = (MemberType){makeIdent(name.start, name.len), value};

		appendArray(&initList, memberPtr);

		if (peekToken().kind == TOK_COMMA)
		{
			nextToken();
		}
		else
		{
			break;
		}
	}
	return initList;
}

DynArray parseNonEmptyArgList(void)
{
	DynArray initList;
	initList = initArray(4, sizeof(Term));
	while (true)
	{
		Term* valuePtr = newTerm(parseTerm());
		appendArray(&initList, valuePtr);

		if (peekToken().kind == TOK_COMMA)
		{
			nextToken();
		}
		else
		{
			break;
		}
	}
	return initList;
}

DynArray parseNonEmptyTypeList(void)
{
	DynArray initList;
	initList = initArray(4, sizeof(Type));
	while (true)
	{
		Type* valuePtr = newType(parseType());
		appendArray(&initList, valuePtr);

		if (peekToken().kind == TOK_COMMA)
		{
			nextToken();
		}
		else
		{
			break;
		}
	}
	return initList;
}

DynArray parseNonEmptyFormalsList(void)
{
	DynArray formals;
	Formal f;
	formals = initArray(4, sizeof(Formal));
	while (true)
	{
		Token ident = expect(TOK_IDENTIFIER);
		identifier newId = declare(ident.start, ident.len, true);
		Type* t = NULL;
		if (peekToken().kind == TOK_COLON)
		{
			nextToken();
			t = newType(parseType());
		}
		f = (Formal){newId, t};
		appendArray(&formals, (void*)&f);

		if (peekToken().kind == TOK_COMMA)
		{
			nextToken();
		}
		else
		{
			break;
		}
	}
	return formals;
}

Term parseFactor(void)
{
	DynArray initList;
	bool isUnion;
	Term t;

	Token tok = peekToken();
	SourceInfo s = (SourceInfo){tok.line};
	switch (tok.kind)
	{
	case TOK_LEFT_PAREN:
		nextToken();
		t = parseTerm();
		expect(TOK_RIGHT_PAREN);
		return t;
		break;
	case TOK_UNION:
	case TOK_STRUCT:
		isUnion = tok.kind == TOK_STRUCT ? false : true;
		nextToken();
		expect(TOK_LEFT_BRACE);
		if (peekToken().kind != TOK_RIGHT_BRACE)
		{
			initList = parseNonEmptyInitList();
			expect(TOK_RIGHT_BRACE);
			return (Term
			){STRUCTURE, {.structure = {isUnion, (Member*)initList.items, initList.count}}, s, NULL
			};
		}
		else
		{
			nextToken();
			return (Term){STRUCTURE, {.structure = {isUnion, NULL, 0}}, s, NULL};
		}
		break;
	default:
		if (startsAtom(tok.kind))
		{
			return parseAtom();
		}
		logFatal("Not a factor: %d.", tok.kind);
		abort();
		break;
	}
}

bool startsPostfix(TokenKind tok)
{
	if (startsFactor(tok))
	{
		return true;
	}
	return false;
}

Term parsePostfix(void)

{
	Term post;
	Token peek = peekToken();
	SourceInfo s = (SourceInfo){peek.line};

	post = parseFactor();

	DynArray list;
	while (true)
	{
		peek = peekToken();
		switch (peek.kind)
		{
		case TOK_LEFT_BRACE:
			nextToken();
			if (post.kind != VAR)
			{
				logFatal("Can only instantiate a type with a known specifier.");
			}
			string string = toString(&post.data.var.name);
			Type* type = (Type*)mapGet(&typeDefinitions, string.chars, string.length);
			if (type == NULL)
			{
				logFatal("Type not defined, but used in line %d.", post.info.line);
			}
			if (type->kind != STRUCT_TYPE)
			{
				logFatal("Type to be initialized not a structure, in line %d.", post.info.line);
			}
			if (peekToken().kind != TOK_RIGHT_BRACE)
			{
				DynArray inits = parseNonEmptyInitList();
				expect(TOK_RIGHT_BRACE);
				post = (Term
				){STRUCTURE,
				  {.structure = {type->data.structure.isUnion, (Member*)inits.items, inits.count}},
				  s,
				  NULL};
			}
			else
			{
				nextToken();
				post = (Term){STRUCTURE,
				              {.structure = {type->data.structure.isUnion, (Member*)NULL, 0}},
				              s,
				              NULL};
			}
			break;

		case TOK_LEFT_BRACKET:
			nextToken();
			Term* indexPtr = newTerm(parseTerm());
			expect(TOK_RIGHT_BRACKET);
			post = (Term){SUBSCRIPT, {.subscript = {newTerm(post), indexPtr}}, s, NULL};
			break;
		case TOK_LEFT_PAREN:
			nextToken();
			if (peekToken().kind == TOK_RIGHT_PAREN)
			{
				nextToken();
				post = (Term){APPLICATION, {.app = {newTerm(post), NULL, 0}}, s, NULL};
			}
			else
			{
				list = parseNonEmptyArgList();
				expect(TOK_RIGHT_PAREN);
				post = (Term
				){APPLICATION, {.app = {newTerm(post), (Term*)list.items, list.count}}, s, NULL
				};
			}
			break;
		// TODO: Where to add auto-dereferincg?
		case TOK_DOT:
			nextToken();
			Token ident = expect(TOK_IDENTIFIER);
			post = (Term
			){ACCESS, {.access = {newTerm(post), makeIdent(ident.start, ident.len)}}, s, NULL};
			break;
		case TOK_INCREMENT:
			nextToken();
			post = (Term){UNARY_OP, {.unOp = {POST_INCREMENT, newTerm(post)}}, s, NULL};
			break;
		case TOK_DECREMENT:
			nextToken();
			post = (Term){UNARY_OP, {.unOp = {POST_DECREMENT, newTerm(post)}}, s, NULL};
			break;
		case TOK_DOT_STAR:
			nextToken();
			post = (Term){DEREF, {.deref = {newTerm(post)}}, s, NULL};
			break;
		default:
			return post;
		}
	}
}

bool isModifiableLValue(const Term* t)
{
	switch (t->kind)
	{
	case VAR:
	case DEREF:
	case SUBSCRIPT:
		return true;
	default:
		return false;
	}
}

bool isPrefixOp(TokenKind k)
{
	switch (k)
	{

	case TOK_AMPERSAND:
	case TOK_INCREMENT:
	case TOK_DECREMENT:
	case TOK_MINUS:
	case TOK_PLUS:
	case TOK_TILDE:
	case TOK_EXCLAMATION:
		return true;
	default:
		return false;
	}
}

Term parsePrefix(void)
{
	SourceInfo s;
	Term* t;
	Token peek = peekToken();

	if (!isPrefixOp(peek.kind))
	{
		return parsePostfix();
	}
	nextToken();

	t = newTerm(parsePostfix());
	s = (SourceInfo){peek.line};

	switch (peek.kind)
	{
	case TOK_AMPERSAND:
		if (t->kind != VAR && t->kind != SUBSCRIPT && t->kind != DEREF)
		{
			logFatal(
			    "Only a variable or a subscript or a dereference is adressable, and can thus be "
			    "referenced."
			);
		}
		return (Term){REF, {.ref = {t}}, s, NULL};
	case TOK_INCREMENT:
		if (!isModifiableLValue(t))
		{
			logFatal("Non-modifiable value cannot be modfied.");
		}
		return (Term){UNARY_OP, {.unOp = {PRE_INCREMENT, t}}, s, NULL};
	case TOK_DECREMENT:
		if (!isModifiableLValue(t))
		{
			logFatal("Non-modifiable value cannot be modfied.");
		}
		return (Term){UNARY_OP, {.unOp = {PRE_DECREMENT, t}}, s, NULL};
	case TOK_MINUS:
		return (Term){UNARY_OP, {.unOp = {NEG, t}}, s, NULL};
	case TOK_PLUS:
		return parseTerm();
	case TOK_TILDE:
		return (Term){UNARY_OP, {.unOp = {BIT_NOT, t}}, s, NULL};
	case TOK_EXCLAMATION:
		return (Term){UNARY_OP, {.unOp = {NOT, t}}, s, NULL};
	default:
		logFatal("Unreachable");
	}
}

BinaryOpKind tokToBinOp(TokenKind t)
{
	switch (t)
	{
	case TOK_STAR:
		return MULTIPLY;
	case TOK_MINUS:
		return SUBTRACT;
	case TOK_PLUS:
		return ADD;
	case TOK_SLASH:
		return DIVIDE;
	case TOK_PERCENT:
		return REMAINDER;

	case TOK_EQ:
		return EQUAL;
	case TOK_NEQ:
		return NOT_EQUAL;
	case TOK_LT:
		return LESS_THAN;
	case TOK_LE:
		return LESS_OR_EQUAL;
	case TOK_GT:
		return GREATER_THAN;
	case TOK_GE:
		return GREATER_OR_EQUAL;

	case TOK_OR:
		return OR;
	case TOK_AND:
		return AND;

	// Non-existent binary op
	default:
		logFatal("%d is not a binary operator.", t);
	}
}

int precedence(TokenKind k)
{
	switch (k)
	{
	// binary-type
	case TOK_AS:
		return 7;
	case TOK_COLON:
		return 6;

	// multiplicative
	case TOK_STAR:
	case TOK_SLASH:
	case TOK_PERCENT:
		return 5;

	// additive
	case TOK_PLUS:
	case TOK_MINUS:
		return 4;

	// relational
	case TOK_EQ:
	case TOK_NEQ:
	case TOK_LT:
	case TOK_LE:
	case TOK_GE:
	case TOK_GT:
		return 3;

	// logical
	case TOK_AND:
		return 2;
	case TOK_OR:
		return 1;

	default:
		return -1;
	}
}

Term parseBinary(int minPrec)
{
	Term term = parsePrefix();
	Token next;
	int prec;
	next = peekToken();
	prec = precedence(next.kind);
	while (prec >= minPrec)
	{
		SourceInfo s = term.info;
		// parses left-to-right associatively
		switch (next.kind)
		{
		// Binary
		case TOK_PLUS:
		case TOK_MINUS:
		case TOK_STAR:
		case TOK_SLASH:
		case TOK_PERCENT:
		case TOK_EQ:
		case TOK_NEQ:
		case TOK_LT:
		case TOK_LE:
		case TOK_GE:
		case TOK_GT:
		case TOK_AND:
		case TOK_OR:
			nextToken();
			BinaryOpKind binOp = tokToBinOp(next.kind);
			term = (Term) {BINARY_OP, {.binOp = {binOp, newTerm(term), newTerm(parseBinary(prec + 1))}}, s, NULL};
			break;

		case TOK_AS:
			nextToken();
			term = (Term){CAST, {.cast = {newTerm(term), parseType()}}, s, NULL};
			break;

		case TOK_COLON:
			nextToken();
			term = (Term){TYPED, {.typed = {newTerm(term), parseType()}}, s, NULL};
			break;
		default:
			break;
		}
		next = peekToken();
		prec = precedence(next.kind);
	}
	return term;
}

Term parseBracedBlock(SourceInfo s)
{
	DynArray list;
	Token peek;

	if (peekToken().kind == TOK_RIGHT_BRACE)
	{
		return (Term){BLOCK, {.block = {.stmts = NULL, ._stmtCount = 0}}, s, NULL};
	}

	enterScope();

	list = initArray(16, sizeof(Statement));
	Term* trailingExp = NULL;
	do
	{
		peek = peekToken();
		if (peek.kind == TOK_VAL || peek.kind == TOK_VAR)
		{
			DeclarationData dec = parseDeclaration();
			Statement newS = (Statement){DECLARATION, {.declaration = dec}, s};
			appendArray(&list, (void*)&newS);
		}
		else
		{
			Term* expr = newTerm(parseTerm());
			peek = peekToken();
			if (peek.kind == TOK_SEMICOLON)
			{
				nextToken();
				Statement newS = (Statement){UNIT_EXPRESSION, {.unit_expression = expr}, s};
				appendArray(&list, (void*)&newS);
			}
			else if (peek.kind == TOK_RIGHT_BRACE)
			{
				trailingExp = expr;
				break;
			}
			else
			{
				logFatal("Expression in block must be followed by a semicolon or closing brace.");
			}
		}
		peek = peekToken();
	} while (peek.kind != TOK_RIGHT_BRACE);

	leaveScope();

	return (Term){BLOCK, {.block = {list.items, list.count, trailingExp}}, s, NULL};
}

// Needed to be a global varaible, so that when performing lambda-lifting,
// [parseControlFlow] can add a declaration for the new function.
DynArray declarations;

Term parseControlFlow(void)
{
	Token peek;
	Term *t1, *t2, *t3;
	DynArray list;

	peek = peekToken();
	SourceInfo s = (SourceInfo){peek.line};
	switch (peek.kind)
	{
	case TOK_IF:
		nextToken();
		t1 = newTerm(parseTerm());
		expect(TOK_THEN);
		t2 = newTerm(parseTerm());
		if (peekToken().kind == TOK_ELSE)
		{
			nextToken();
			t3 = newTerm(parseTerm());
		}
		else
		{
			t3 = NULL;
		}
		return (Term){CONDITIONAL, {.cond = {t1, t2, t3}}, s, NULL};
	case TOK_FOR:
		nextToken();
		t1 = newTerm(parseTerm());
		expect(TOK_DO);
		t2 = newTerm(parseTerm());
		return (Term){LOOP, {.loop = {t1, t2}}, s, NULL};

	// TODO: Add defer
	case TOK_BREAK:
		nextToken();
		return (Term){BREAK, {0}, s, NULL};
	case TOK_CONTINUE:
		nextToken();
		return (Term){CONTINUE, {0}, s, NULL};
	case TOK_RETURN:
		nextToken();
		t1 = newTerm(parseTerm());
		return (Term){RETURN, {.retur = {t1}}, s, NULL};

	case TOK_FUN:
		nextToken();

		Scope* oldScope = scope;
		for (; scope->parent != NULL; scope = scope->parent)
		{
		}
		enterScope();

		peek = peekToken();
		identifier recIdent;
		if (peek.kind == TOK_LEFT_PAREN)
		{
			recIdent = newIdent();
		}
		else if (peek.kind == TOK_IDENTIFIER)
		{
			nextToken();
			recIdent = declare(peek.start, peek.len, false);
		}
		else
		{
			recIdent = newIdent();
			logFatal("Unexpected Token %d.", peek.kind);
		}

		bool isEmpty;
		expect(TOK_LEFT_PAREN);
		if (peekToken().kind != TOK_RIGHT_PAREN)
		{
			isEmpty = false;
			list = parseNonEmptyFormalsList();
			expect(TOK_RIGHT_PAREN);
		}
		else
		{
			nextToken();
			isEmpty = true;
		}

		Type* type = NULL;
		if (peekToken().kind == TOK_ARROW)
		{
			nextToken();
		}
		else
		{
			type = newType(parseType());
			expect(TOK_ARROW);
		}
		t1 = newTerm(parseTerm());

		identifier lambdaIdent = newIdent();
		Term* lambda =
		    newTerm((Term){FUNCTION,
		                   {.fun =
		                        {recIdent, isEmpty ? NULL : list.items,
		                         isEmpty ? 0 : list.count, type, t1}},
		                   s,
		                   NULL});
		DeclarationData decl = {lambdaIdent, NULL, false, lambda};
		appendArray(&declarations, (void*)&decl);

		leaveScope();
		scope = oldScope;
		return (Term){VAR, {.var = {lambdaIdent}}, s, NULL};

	case TOK_LEFT_BRACE:
		nextToken();
		Term t = parseBracedBlock(s);
		expect(TOK_RIGHT_BRACE);
		return t;

	default:
		return parseBinary(0);
	}
}

Term parseTerm(void)
{
	Term lvalue, *value;

	lvalue = parseControlFlow();
	if (peekToken().kind == TOK_EQUAL)
	{
		nextToken();

		if (!isModifiableLValue(&lvalue))
		{
			logFatal("Lvalue is not assignable");
		}

		value = newTerm(parseTerm());
		return (Term){ASSIGNMENT, {.assignment = {newTerm(lvalue), value}}, lvalue.info, NULL};
	}
	else
	{
		return lvalue;
	}
}

DeclarationData parseDeclaration(void)
{
	bool mutable = nextToken().kind == TOK_VAL ? false : true;
	Token ident = expectElse(TOK_IDENTIFIER, "Declaration keyword must be followed by identfier.");
	identifier newId = declare(ident.start, ident.len, mutable);
	Type* typePtr = NULL;
	if (peekToken().kind == TOK_COLON)
	{
		nextToken();
		typePtr = newType(parseType());
	}
	Term* exp = NULL;
	if (peekToken().kind == TOK_EQUAL)
	{
		nextToken();
		exp = newTerm(parseTerm());
	}
	expectElse(TOK_SEMICOLON, "Unterminated declaration.");
	DeclarationData d = (DeclarationData){
	    .mutable = mutable,
	    .name = newId,
	    .type = typePtr,
	    .exp = exp,
	};
	printDeclaration(&d);
	printf("\n");
	return d;
}

#define equal(a, lit) memcmp(a.start, lit, (size_t)a.len) == 0

Type parseType(void)
{
	bool isEmptyFun;

	Token tok = nextToken();
	Term num;
	DynArray list;
	switch (tok.kind)
	{
	case TOK_IDENTIFIER:
		if (equal(tok, "i8"))
			return (Type){I8, {0}};
		else if (equal(tok, "i16"))
			return (Type){I16, {0}};
		else if (equal(tok, "i32"))
			return (Type){I32, {0}};
		else if (equal(tok, "i64"))
			return (Type){I64, {0}};

		else if (equal(tok, "u8"))
			return (Type){U8, {0}};
		else if (equal(tok, "u16"))
			return (Type){U16, {0}};
		else if (equal(tok, "u32"))
			return (Type){U32, {0}};
		else if (equal(tok, "u64"))
			return (Type){U64, {0}};

		else if (equal(tok, "f32"))
			return (Type){F32, {0}};
		else if (equal(tok, "f64"))
			return (Type){F64, {0}};

		Type def = *(Type*)mapGet(&typeDefinitions, tok.start, tok.len);
		return def;
		break;

	case TOK_LEFT_BRACKET:
		num = parseNumber(nextToken());
		expect(TOK_LEFT_BRACKET);
		int* x = (int*)malloc(sizeof(int));
		// TODO: The element count of an array shall be of type usize
		*x = (int)num.data.constant.data.i32Val;
		return (Type){ARRAY_TYPE, {.arr = {newType(parseType()), (size*)x}}};

	case TOK_UNION:
	case TOK_STRUCT:
		expect(TOK_LEFT_BRACE);
		bool isUnion = tok.kind == TOK_UNION ? true : false;
		if (peekToken().kind != TOK_RIGHT_BRACE)
		{
			list = parseNonEmptyStructTypesList();
			expect(TOK_RIGHT_BRACE);
			return (Type){STRUCT_TYPE, {.structure = {(MemberType*)list.items, list.count, isUnion}}
			};
		}
		else
		{
			nextToken();
			return (Type){STRUCT_TYPE, {.structure = {NULL, 0, isUnion}}};
		}

	case TOK_LEFT_PAREN:

		if (peekToken().kind != TOK_RIGHT_PAREN)
		{
			isEmptyFun = false;
			list = parseNonEmptyTypeList();
			expect(TOK_RIGHT_PAREN);
		}
		else
		{
			isEmptyFun = true;
			nextToken();
		}
		expect(TOK_ARROW);
		Type* retType = newType(parseType());
		return (Type
		){FUN_TYPE,
		  {.fun = {isEmptyFun ? NULL : (Type*)list.items, isEmptyFun ? 0 : list.count, retType}}};

	default:
		logFatal("Unrecognized type: %s", tok.start);
		abort();
	}
}

Program parse(char* source)
{
	Type* type = NULL;
	SourceInfo s;

	mapInit(&typeDefinitions);

	initLexer(source);
	enterScope();

	Token peek = peekToken();
	declarations = initArray(16, sizeof(DeclarationData));
	while (peek.kind != TOK_EOF)
	{
		s = (SourceInfo){peek.line};
		DeclarationData newDecl = {0};

		switch (peek.kind)
		{
		case TOK_VAL:
		case TOK_VAR:
			newDecl = parseDeclaration();
			appendArray(&declarations, (void*)&newDecl);
			break;

		case TOK_FUN:
			nextToken();
			Token identTok = expect(TOK_IDENTIFIER);
			identifier ident = declare(identTok.start, identTok.len, false);

			bool isEmpty;
			DynArray formals;
			expect(TOK_LEFT_PAREN);
			if (peekToken().kind != TOK_RIGHT_PAREN)
			{
				isEmpty = false;
				formals = parseNonEmptyFormalsList();
				expect(TOK_RIGHT_PAREN);
			}
			else
			{
				nextToken();
				isEmpty = true;
			}

			if (peekToken().kind == TOK_LEFT_BRACE)
			{
				nextToken();
			}
			else
			{
				type = newType(parseType());
				expect(TOK_LEFT_BRACE);
			}
			Term t = parseBracedBlock(s);
			expect(TOK_RIGHT_BRACE);

			Term fun = (Term){FUNCTION,
			                  {.fun =
			                       {ident,
			                        isEmpty ? NULL : formals.items,
			                        isEmpty ? 0 : formals.count,
			                        type,
			                        newTerm(t)}},
			                  s,
			                  NULL};

			newDecl = (DeclarationData){ident, NULL, false, newTerm(fun)};
			appendArray(&declarations, (void*)&newDecl);
			break;

		case TOK_TYPE:
			nextToken();
			Token identType = nextToken();
			if (nextToken().kind != TOK_EQUAL)
			{
				logFatal("Type definitions misses equal sign after identifier.");
			}
			type = newType(parseType());
			expect(TOK_SEMICOLON);
			mapPut(&typeDefinitions, identType.start, identType.len, (void*)type);
			break;

		default:
			logFatal("Expected start of declaration at line %d, but got %d", peek.line, peek.kind);
			break;
		}
		peek = peekToken();
		printScope();
	}
	leaveScope();
	return (Program){(DeclarationData*)declarations.items, declarations.count};
}

#endif
