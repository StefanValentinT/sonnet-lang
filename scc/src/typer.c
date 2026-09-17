#ifndef TYPER_H
#define TYPER_H

#include "syntax.c"

void type(Program* term, u64 maxId);

#endif
#if __INCLUDE_LEVEL__ == 0

#include <assert.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#include "hashmap.c"
#include "log.c"

size maxTypeVar = 0;
Type freshTypeVar(void) { return (Type){TYPE_VAR, {.typeVar = maxTypeVar++}}; }
Type* newTypeVar(void) { return newType(freshTypeVar()); }

static struct
{
	Type** subst;
	size count;
	size capac;
} substitutions;

void insertSubstitution(size type_var, Type* replacement)
{
	if (type_var >= substitutions.capac)
	{
		size new_capac = substitutions.capac ? substitutions.capac * 2 : 16;

		while (new_capac <= type_var)
			new_capac *= 2;

		substitutions.subst =
		    realloc(substitutions.subst, new_capac * sizeof(*substitutions.subst));

		for (size i = substitutions.capac; i < new_capac; ++i)
		{
			substitutions.subst[i] = NULL;
		}

		substitutions.capac = new_capac;
	}

	substitutions.subst[type_var] = replacement;

	if (type_var >= substitutions.count)
		substitutions.count = type_var + 1;
}

Type* getSubstitution(size type_var)
{
	if (type_var < substitutions.count)
	{
		return substitutions.subst[type_var];
	}
	else
	{
		return NULL;
	}
}

typedef struct
{
	bool hasConstraint;
	Type* type;
} TypeConstraint;

TypeConstraint* newEmptyConstraint(void)
{
	TypeConstraint* temp = malloc(sizeof(TypeConstraint));
	if (temp == NULL)
	{
		logFatal("Could not allocate enough memory for typer.");
	}
	memset(temp, 0, sizeof(TypeConstraint));
	return temp;
}

static TypeConstraint* autoConstraints;
static Map externIdentifierConstraints;

void initConstraints(u64 maxId)
{
	autoConstraints = calloc(maxId, sizeof(TypeConstraint));
	if (autoConstraints == NULL)
	{
		logFatal("Could not allocate enough memory for typer.");
	}
	mapInit(&externIdentifierConstraints);
}

TypeConstraint* getConstraint(identifier* id)
{
	if (id->isAuto)
	{
		return &autoConstraints[id->data.intData];
	}
	else
	{
		TypeConstraint* maybe = mapGet(
		    &externIdentifierConstraints, id->data.stringData.charData, id->data.stringData.length
		);
		if (maybe == NULL)
		{
			TypeConstraint* empty = newEmptyConstraint();
			mapPut(
			    &externIdentifierConstraints, id->data.stringData.charData,
			    id->data.stringData.length, empty
			);
			return empty;
		}
		else
		{
			return maybe;
		}
	}
}

bool insertConstraint(identifier* id, Type* type)
{
	bool constrainTypes(Type * a, Type * b);
	TypeConstraint* constraint = getConstraint(id);

	if (!constraint->hasConstraint)
	{
		constraint->hasConstraint = true;
		constraint->type = type;
		return true;
	}

	return constrainTypes(constraint->type, type);
}

Type* resolveType(Type* type)
{
	if (type->kind == TYPE_VAR)
	{
		Type* replacement = getSubstitution(type->data.typeVar);

		if (replacement == NULL)
			return type;

		Type* resolved = resolveType(replacement);

		insertSubstitution(type->data.typeVar, resolved);

		return resolved;
	}

	switch (type->kind)
	{
	case POINTER_TYPE:
		type->data.pointer.pointee = resolveType(type->data.pointer.pointee);
		break;

	case ARRAY_TYPE:
		type->data.arr.elemType = resolveType(type->data.arr.elemType);
		break;

	case FUN_TYPE:
		for (size i = 0; i < type->data.fun._paramCount; ++i)
		{
			type->data.fun.paramTypes[i] = *resolveType(&type->data.fun.paramTypes[i]);
		}

		type->data.fun.retType = resolveType(type->data.fun.retType);

		break;

	case STRUCT_TYPE:
		for (size i = 0; i < type->data.structure._memberCount; ++i)
		{
			type->data.structure.memberTypes[i].type =
			    resolveType(type->data.structure.memberTypes[i].type);
		}
		break;

	default:
		break;
	}

	return type;
}

bool occursIn(size var, Type* type)
{
	type = resolveType(type);

	switch (type->kind)
	{
	case TYPE_VAR:
		return type->data.typeVar == var;

	case POINTER_TYPE:
		return occursIn(var, type->data.pointer.pointee);

	case ARRAY_TYPE:
		return occursIn(var, type->data.arr.elemType);

	case FUN_TYPE:
		for (size i = 0; i < type->data.fun._paramCount; ++i)
		{
			if (occursIn(var, &type->data.fun.paramTypes[i]))
				return true;
		}

		return occursIn(var, type->data.fun.retType);

	case STRUCT_TYPE:
		for (size i = 0; i < type->data.structure._memberCount; ++i)
		{
			if (occursIn(var, type->data.structure.memberTypes[i].type))
				return true;
		}

		return false;

	default:
		return false;
	}
}

bool constrainTypes(Type* a, Type* b)
{
	a = resolveType(a);
	b = resolveType(b);

	if (a->kind == TYPE_VAR)
	{
		if (b->kind == TYPE_VAR && a->data.typeVar == b->data.typeVar)
			return true;
		if (occursIn(a->data.typeVar, b))
			return false;
		insertSubstitution(a->data.typeVar, b);
		return true;
	}

	if (b->kind == TYPE_VAR)
	{
		if (occursIn(b->data.typeVar, a))
			return false;
		insertSubstitution(b->data.typeVar, a);
		return true;
	}

	if (a->kind != b->kind)
		return false;

	switch (a->kind)
	{
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
	case BOOL:
		return true;

	case POINTER_TYPE:
		return constrainTypes(a->data.pointer.pointee, b->data.pointer.pointee);

	case ARRAY_TYPE:
		if (a->data.arr.elemCount != NULL && b->data.arr.elemCount != NULL &&
		    *a->data.arr.elemCount != *b->data.arr.elemCount)
			return false;

		return constrainTypes(a->data.arr.elemType, b->data.arr.elemType);

	case FUN_TYPE:
		if (a->data.fun._paramCount != b->data.fun._paramCount)
			return false;

		for (size i = 0; i < a->data.fun._paramCount; ++i)
		{
			if (!constrainTypes(&a->data.fun.paramTypes[i], &b->data.fun.paramTypes[i]))
				return false;
		}

		return constrainTypes(a->data.fun.retType, b->data.fun.retType);

	case STRUCT_TYPE:
		if (a->data.structure.isUnion != b->data.structure.isUnion)
			return false;

		if (a->data.structure._memberCount != b->data.structure._memberCount)
			return false;

		for (size i = 0; i < a->data.structure._memberCount; ++i)
		{
			MemberType* ma = &a->data.structure.memberTypes[i];

			MemberType* mb = &b->data.structure.memberTypes[i];

			if (!isEqualIdent(&ma->name, &mb->name))
				return false;

			if (!constrainTypes(ma->type, mb->type))
				return false;
		}

		return true;

	default:
		return false;
	}
}

bool constrainTypeTo(Type* toConstrain, Type target)
{
	toConstrain = resolveType(toConstrain);

	if (toConstrain->kind != TYPE_VAR)
		return toConstrain->kind == target.kind;

	if (target.kind == TYPE_VAR && toConstrain->data.typeVar == target.data.typeVar)
		return true;

	if (occursIn(toConstrain->data.typeVar, &target))
		return false;

	insertSubstitution(toConstrain->data.typeVar, newType(target));
	return true;
}

const Type boolType = (Type){BOOL, {0}};
const Type unitType = (Type){STRUCT_TYPE, {.structure = {NULL, 0, false}}};

Type* typeTerm(Term* term)
{
	static Type* currentReturnType = NULL;

	Type* result = NULL;
	TypeConstraint* constraint;
	Type* functionType;
	switch (term->kind)
	{
	case CONSTANT:
		result = &term->data.constant.numericType;
		break;

	case BOOLEAN:
		result = term->type;
		break;

	case VAR:
		constraint = getConstraint(&term->data.var.name);

		if (constraint->hasConstraint)
		{
			result = constraint->type;
		}
		else
		{
			Type* type = newTypeVar();
			constraint->hasConstraint = true;
			constraint->type = type;
			result = type;
		}
		break;

	case FUNCTION:
	{
	}
		Type* prevReturnType = currentReturnType;

		Type* params = malloc(sizeof(Type) * term->data.fun._formalCount);
		if (params == NULL)
		{
			logFatal("Could not allocate enough memory for typer.");
		}

		for (size i = 0; i < term->data.fun._formalCount; ++i)
		{
			Formal* formal = &term->data.fun.formals[i];

			if (formal->type != NULL)
			{
				params[i] = *formal->type;
			}
			else
			{
				params[i] = freshTypeVar();
			}

			insertConstraint(&formal->name, newType(params[i]));
		}

		Type* returnType;
		if (term->data.fun.retType != NULL)
		{
			returnType = term->data.fun.retType;
		}
		else
		{
			returnType = newTypeVar();
		}
		currentReturnType = returnType;

		functionType = newType((Type){.kind = FUN_TYPE,
		                              .data.fun = {
		                                  .paramTypes = params,
		                                  ._paramCount = term->data.fun._formalCount,
		                                  .retType = returnType
		                              }});

		if (!insertConstraint(&term->data.fun.recBinder, functionType))
			logFatal("Conflicting recursive function binding.");

		Type* bodyType = typeTerm(term->data.fun.body);
		if (!constrainTypes(bodyType, returnType))
			logFatal("Function body does not match return type.");

		resolveType(functionType);

		currentReturnType = prevReturnType;

		result = functionType;
		break;

	case APPLICATION:
		functionType = typeTerm(term->data.app.fun);
		if (functionType->kind != FUN_TYPE)
		{
			logFatal("Can not apply a non-function value.");
		}
		if (term->data.app._argCount != functionType->data.fun._paramCount)
		{
			logFatal("Function of fixed arity can not be applied to any other number of arguments."
			);
		}
		for (size i = 0; i < term->data.app._argCount; i++)
		{
			if (!constrainTypes(
			        typeTerm(&term->data.app.args[i]), &functionType->data.fun.paramTypes[i]
			    ))
			{
				logFatal("Incompatible argument and parameter types.");
			}
		}

		result = functionType->data.fun.retType;

		break;

	// case ARRAY:
	case STRUCTURE: {}
		MemberType* members = malloc(sizeof(Member) * term->data.structure.memberCount);
		for (size i = 0; i < term->data.structure.memberCount; i++)
		{
			Member m = term->data.structure.members[i];
			members[i] = (MemberType){m.name, typeTerm(m.term)};
		}
		result = newType((Type){STRUCT_TYPE, {.structure = {members, term->data.structure.memberCount, term->data.structure.isUnion}}});
		break;
	// case STRING:
	// case REF:
	// case DEREF:
	// case CAST:
	// case TYPED:
	case RETURN:
	{
	}
		Type* retExpType = typeTerm(term->data.retur.exp);
		if (currentReturnType == NULL || !constrainTypes(retExpType, currentReturnType))
		{
			logFatal("Return type not equal to the function's return type.");
		}
		result = newType(unitType);
		break;

	case BREAK:
		result = newType(unitType);
		break;

	case CONTINUE:
		result = newType(unitType);
		break;

	// case UNARY_OP:
	case BINARY_OP:
	{
	}
		// var a; var b; var x = a + b; x = 10i32; does NOT work right now

		Type* leftType = typeTerm(term->data.binOp.a);
		Type* rightType = typeTerm(term->data.binOp.b);
		leftType = resolveType(leftType);
		rightType = resolveType(rightType);

		switch (term->data.binOp.kind)
		{
		case ADD:
			if (isNumericType(leftType) && isNumericType(rightType))
			{
				if (!constrainTypes(leftType, rightType))
				{
					logFatal("Operands of + must have the same numeric type.");
				}
				result = resolveType(leftType);
			}
			else if (leftType->kind == POINTER_TYPE && isIntegralType(rightType))
			{
				result = leftType;
			}
			else if (isIntegralType(leftType) && rightType->kind == POINTER_TYPE)
			{
				result = rightType;
			}
			break;

		case SUBTRACT:
		case MULTIPLY:
		case DIVIDE:
			if (!constrainTypes(leftType, rightType))
			{
				logFatal("Operands of binary operator must have the same numeric type.");
			}
			leftType = resolveType(leftType);
			rightType = resolveType(rightType);
			if (!isNumericType(leftType) || !isNumericType(rightType))
			{
				logFatal("Expected numeric operands for binary operator.");
			}
			result = leftType;
			break;

		case LESS_THAN:
		case LESS_OR_EQUAL:
		case GREATER_THAN:
		case GREATER_OR_EQUAL:
			if (!isNumericType(leftType) || !isNumericType(rightType))
			{
				logFatal("Expected numeric operands for relational operator.");
			}
			if (!constrainTypes(leftType, rightType))
			{
				logFatal("Operands of binary operator must have the same numeric type.");
			}
			result = newType(boolType);
			break;

		case REMAINDER:
			if (!isIntegralType(leftType) || !isIntegralType(rightType))
			{
				logFatal("Expected integral operands for binary operator %.");
			}
			if (!constrainTypes(leftType, rightType))
			{
				logFatal("Operands of binary operator % must have the same integral type.");
			}
			result = resolveType(leftType);
			break;

		case AND:
		case OR:
			if (leftType->kind != BOOL || rightType->kind != BOOL)
			{
				logFatal("Operands of logical operator must have boolean type.");
			}
			result = leftType;
			break;

		case EQUAL:
		case NOT_EQUAL:
			if (!constrainTypes(leftType, rightType))
			{
				logFatal("Operands of equality comparison must have the same type.");
			}
			leftType = resolveType(leftType);
			if (!isNumericType(leftType) && leftType->kind != BOOL &&
			    leftType->kind != POINTER_TYPE)
			{
				logFatal("Types can not be compared for equality.");
			}
			result = newType(boolType);
			break;
		}

		break;

	// case BINARY_OP_ASSIGN:
	// case SUBSCRIPT:
	// case ACCESS:
	case CONDITIONAL:
	{
	}
		Type* condType = typeTerm(term->data.cond.cond);
		if (!constrainTypeTo(condType, boolType))
		{
			logFatal("Condition must have boolean type.");
		}
		Type* thenType = typeTerm(term->data.cond.thenBranch);
		if (term->data.cond.elseBranch != NULL)
		{
			Type* elseType = typeTerm(term->data.cond.elseBranch);
			if (!constrainTypes(thenType, elseType))
			{
				logFatal("Branches of conditional have different types.");
			}
		}
		result = thenType;
		break;

	// case LOOP:

	case ASSIGNMENT: {}
		Type* valType = typeTerm(term->data.assignment.value);
		Type* lType = typeTerm(term->data.assignment.lvalue);
		constrainTypes(lType, valType);
		result = newType(unitType);
		break;
		
	case BLOCK:
		for (size i = 0; i < term->data.block._stmtCount; i++)
		{
			if (term->data.block.stmts[i].kind == DECLARATION)
			{
				Type* type = typeTerm(term->data.block.stmts[i].data.declaration.exp);
				if (term->data.block.stmts[i].data.declaration.type != NULL)
				{
					if (!constrainTypes(type, term->data.block.stmts[i].data.declaration.type))
					{
						logFatal("Declaration type does not match the type of the expression.");
					}
				}
				if (!insertConstraint(&term->data.block.stmts[i].data.declaration.name, type))
				{
					logFatal("Conflicting declaration type.");
				}
				term->data.block.stmts[i].data.declaration.type = type;
			}
			else
			{
				Type* type = typeTerm(term->data.block.stmts[i].data.unit_expression);
				if (!constrainTypeTo(type, unitType))
				{
					logFatal("Expression in block does not return unit, possibly ignoring a value."
					);
				}
				term->data.block.stmts[i].data.unit_expression->type = type;
			}
		}
		if (term->data.block.exp != NULL)
		{
			result = typeTerm(term->data.block.exp);
		}
		else
		{
			result = newType(unitType);
		}
		break;

	default:
		logFatal("Typing of %s not yet implemented.", termKindToString(term->kind));
		break;
	}
	if (result == NULL)
	{
		logFatal("Could not type %s.", termKindToString(term->kind));
	}
	term->type = resolveType(result);
	return (Type*)term->type;
}

void type(Program* prog, u64 maxId)
{
	initConstraints(maxId);

	for (size i = 0; i < prog->_declCount; i++)
	{
		Type* type;
		if (prog->decls[i].exp != NULL)
		{
			type = typeTerm(prog->decls[i].exp);
		} else
		{
			type = newTypeVar();
		}
		if (prog->decls[i].type != NULL)
		{
			if (!constrainTypes(type, prog->decls[i].type))
			{
				logFatal("Declaration type does not match the type of the expression.");
			}
		}
		if (!insertConstraint(&prog->decls[i].name, type))
		{
			logFatal("Conflicting declaration type.");
		}
		prog->decls[i].type = type;
	}
	for (size i = 0; i < prog->_declCount; ++i)
	{
		if (prog->decls[i].exp != NULL)
			prog->decls[i].exp->type = resolveType((Type*)prog->decls[i].exp->type);

		TypeConstraint* c = getConstraint(&prog->decls[i].name);

		if (c->hasConstraint)
			c->type = resolveType(c->type);

		if (prog->decls[i].type != NULL)
			prog->decls[i].type = resolveType(prog->decls[i].type);
	}
}

#endif
