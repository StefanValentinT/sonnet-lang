use std::collections::HashMap;

use crate::ast::ast_type::*;
use crate::ast::typed_ast::*;
use crate::ast::untyped_ast::*;
use crate::stdlib::builtin_functions;

#[derive(Clone)]
struct SymbolEntry {
    ty: Type,
}

type SymbolTable = HashMap<String, SymbolEntry>;

pub fn typecheck(program: Program) -> TypedProgram {
    let mut symbols = SymbolTable::new();
    let Program::Program(funcs) = program;

    for (name, fun) in builtin_functions() {
        let fun_ty = Type::FunType {
            params: fun.params.iter().map(|(_, t)| t.clone()).collect(),
            ret: Box::new(fun.ret_type.clone()),
        };
        symbols.insert(name, SymbolEntry { ty: fun_ty });
    }

    for f in &funcs {
        declare_function(f, &mut symbols);
    }

    let mut typed_funs = Vec::new();
    for f in funcs {
        typed_funs.push(typecheck_fun_decl(f, &symbols));
    }

    TypedProgram {
        functions: typed_funs,
    }
}

fn declare_function(decl: &FunDecl, symbols: &mut SymbolTable) {
    let param_types = decl.params.iter().map(|(_, t)| t.clone()).collect();
    let fun_ty = Type::FunType {
        params: param_types,
        ret: Box::new(decl.ret_type.clone()),
    };

    if let Some(old) = symbols.get(&decl.name) {
        if old.ty != fun_ty {
            panic!("Conflicting declarations of function {}", decl.name);
        }
    }

    symbols.insert(decl.name.clone(), SymbolEntry { ty: fun_ty });
}

fn typecheck_fun_decl(decl: FunDecl, globals: &SymbolTable) -> TypedFunDecl {
    let SymbolEntry { ty, .. } = globals.get(&decl.name).unwrap().clone();

    let (param_types, ret_ty) = match ty {
        Type::FunType { params, ret } => (params, *ret),
        _ => unreachable!(),
    };

    let mut locals = globals.clone();

    if decl.exec_time == ExecTime::CompileTime && decl.params.len() > 0 {
        panic!(
            "Compile time function must be constant, thus can not be parametric over any input the compiler can not substitute."
        );
    }

    let typed_params: Vec<(String, Type)> = decl
        .params
        .into_iter()
        .zip(param_types.into_iter())
        .map(|((name, _), ty)| {
            locals.insert(name.clone(), SymbolEntry { ty: ty.clone() });
            (name, ty)
        })
        .collect();

    let body = decl
        .body
        .map(|b| TypedBlock::Block(typecheck_block(&b, &mut locals, &ret_ty)));

    TypedFunDecl {
        name: decl.name,
        params: typed_params,
        ret_type: ret_ty,
        body,
        exec_time: decl.exec_time,
    }
}

fn typecheck_block(
    block: &Block,
    symbols: &mut SymbolTable,
    expected_ret: &Type,
) -> Vec<TypedBlockItem> {
    let Block::Block(items) = block;

    items
        .iter()
        .map(|item| match item {
            BlockItem::D(d) => TypedBlockItem::D(typecheck_decl(d, symbols)),
            BlockItem::S(s) => TypedBlockItem::S(typecheck_stmt(s, symbols, expected_ret)),
        })
        .collect()
}

fn typecheck_decl(decl: &Decl, symbols: &mut SymbolTable) -> TypedDecl {
    match decl {
        Decl::Variable(v) => {
            let init_expr = match &v.initializer {
                Initializer::InitExpr(expr) => expr,
            };
            let init = typecheck_expr(&init_expr, symbols);
            if init.ty != v.var_type {
                panic!(
                    "Type mismatch in variable '{}': expected {:?}, got {:?}",
                    v.name, v.var_type, init.ty
                );
            }

            symbols.insert(
                v.name.clone(),
                SymbolEntry {
                    ty: v.var_type.clone(),
                },
            );

            TypedDecl::Variable(TypedVarDecl {
                name: v.name.clone(),
                init_expr: init,
                var_type: v.var_type.clone(),
            })
        }
    }
}

fn typecheck_stmt(stmt: &Stmt, symbols: &mut SymbolTable, expected_ret: &Type) -> TypedStmt {
    match stmt {
        Stmt::Return(expr) => {
            let typed = typecheck_expr(expr, symbols);
            if &typed.ty != expected_ret {
                panic!(
                    "Return type mismatch: expected {:?}, got {:?}",
                    expected_ret, typed.ty
                );
            }
            TypedStmt::Return(typed)
        }

        Stmt::Expression(expr) => TypedStmt::Expr(typecheck_expr(expr, symbols)),

        Stmt::Null => TypedStmt::Null,

        Stmt::Compound(block) => {
            let mut inner = symbols.clone();
            let stmts = typecheck_block(block, &mut inner, expected_ret)
                .into_iter()
                .filter_map(|i| match i {
                    TypedBlockItem::S(s) => Some(s),
                    _ => None,
                })
                .collect();
            TypedStmt::Block(stmts)
        }

        Stmt::While {
            condition,
            body,
            label,
        } => {
            let cond = typecheck_expr(condition, symbols);
            if cond.ty != Type::I32 {
                panic!("While condition must be I32");
            }
            let body = Box::new(typecheck_stmt(body, symbols, expected_ret));
            TypedStmt::While {
                condition: cond,
                body,
                label: label.clone(),
            }
        }

        Stmt::Break { label } => TypedStmt::Break {
            label: label.clone(),
        },

        Stmt::Continue { label } => TypedStmt::Continue {
            label: label.clone(),
        },
    }
}

fn convert_by_assignment(e: TypedExpr, target: &Type) -> TypedExpr {
    if &e.ty == target {
        e
    } else {
        panic!(
            "Cannot convert type {:?} to {:?} as if by assignment",
            e.ty, target
        );
    }
}

fn typecheck_expr(expr: &Expr, symbols: &mut SymbolTable) -> TypedExpr {
    match &expr.kind {
        ExprKind::Constant(Const::I32(v)) => TypedExpr {
            ty: Type::I32,
            kind: TypedExprKind::Constant(Const::I32(*v)),
        },
        ExprKind::Constant(Const::I64(v)) => TypedExpr {
            ty: Type::I64,
            kind: TypedExprKind::Constant(Const::I64(*v)),
        },
        ExprKind::Constant(Const::F64(v)) => TypedExpr {
            ty: Type::F64,
            kind: TypedExprKind::Constant(Const::F64(*v)),
        },
        ExprKind::Constant(Const::Char(v)) => TypedExpr {
            ty: Type::Char,
            kind: TypedExprKind::Constant(Const::Char(*v)),
        },
        ExprKind::Constant(Const::Unit) => TypedExpr {
            ty: Type::Unit,
            kind: TypedExprKind::Constant(Const::Unit),
        },
        ExprKind::Var(name) => {
            let entry = symbols
                .get(name)
                .unwrap_or_else(|| panic!("Undefined variable {}", name));
            TypedExpr {
                ty: entry.ty.clone(),
                kind: TypedExprKind::Var(name.clone()),
            }
        }
        ExprKind::Unary(op, e) => {
            let typed_inner = typecheck_expr(e, symbols);

            match (&op, &typed_inner.ty) {
                (UnaryOp::Negate, Type::Pointer { .. })
                | (UnaryOp::Complement, Type::Pointer { .. }) => {
                    panic!("Cannot apply {:?} to a pointer type", op);
                }
                _ => {}
            }

            TypedExpr {
                ty: typed_inner.ty.clone(),
                kind: TypedExprKind::Unary {
                    op: op.clone(),
                    expr: Box::new(typed_inner),
                },
            }
        }
        ExprKind::Binary(op, lhs, rhs) => {
            let l = typecheck_expr(lhs, symbols);
            let r = typecheck_expr(rhs, symbols);

            match (&l.ty, &r.ty) {
                (Type::Pointer { .. }, Type::Pointer { .. }) => match op {
                    BinaryOp::Equal | BinaryOp::NotEqual | BinaryOp::And | BinaryOp::Or => {}
                    _ => panic!("Illegal operation on pointer types: {:?}", op),
                },
                _ if l.ty == r.ty => {}
                _ => panic!("Binary op type mismatch: {:?} vs {:?}", l.ty, r.ty),
            }

            TypedExpr {
                ty: match op {
                    BinaryOp::Equal | BinaryOp::NotEqual | BinaryOp::And | BinaryOp::Or => {
                        Type::I32
                    }
                    _ => l.ty.clone(),
                },
                kind: TypedExprKind::Binary {
                    op: op.clone(),
                    lhs: Box::new(l),
                    rhs: Box::new(r),
                },
            }
        }
        ExprKind::Assign(lhs, rhs) => {
            let l = typecheck_expr(lhs, symbols);

            if !is_lvalue(lhs) {
                panic!("Left-hand side of assignment is not an lvalue");
            }

            let r = typecheck_expr(rhs, symbols);
            let r_conv = convert_by_assignment(r, &l.ty);

            TypedExpr {
                ty: l.ty.clone(),
                kind: TypedExprKind::Assign {
                    lhs: Box::new(l),
                    rhs: Box::new(r_conv),
                },
            }
        }
        ExprKind::IfThenElse(cond, t, e) => {
            let c = typecheck_expr(cond, symbols);
            let t = typecheck_expr(t, symbols);
            let e = typecheck_expr(e, symbols);

            if t.ty != e.ty {
                panic!(
                    "Conditional branches must have same type, got {:?} vs {:?}",
                    t.ty, e.ty
                );
            }

            TypedExpr {
                ty: t.ty.clone(),
                kind: TypedExprKind::IfThenElse {
                    cond: Box::new(c),
                    then_expr: Box::new(t),
                    else_expr: Box::new(e),
                },
            }
        }
        ExprKind::FunctionCall(name, args) => {
            let entry = symbols
                .get(name)
                .unwrap_or_else(|| panic!("Undefined function {}", name));

            let (params, ret) = match &entry.ty {
                Type::FunType { params, ret } => (params.clone(), *ret.clone()),
                _ => panic!("{} is not a function", name),
            };

            if params.len() != args.len() {
                panic!("Wrong number of arguments to {}", name);
            }

            let typed_args: Vec<_> = args.iter().map(|a| typecheck_expr(a, symbols)).collect();

            for (arg, param_ty) in typed_args.iter().zip(params.iter()) {
                if &arg.ty != param_ty {
                    panic!(
                        "Argument type mismatch in call to {}: expected {:?}, got {:?}",
                        name, param_ty, arg.ty
                    );
                }
            }

            TypedExpr {
                ty: ret,
                kind: TypedExprKind::FunctionCall {
                    name: name.clone(),
                    args: typed_args,
                },
            }
        }
        ExprKind::Cast { expr, target } => {
            let typed_inner = typecheck_expr(expr, symbols);

            match (&typed_inner.ty, target) {
                (Type::Pointer { .. }, Type::I32 | Type::I64 | Type::F64)
                | (Type::I32 | Type::I64 | Type::F64, Type::Pointer { .. }) => {
                    panic!(
                        "Invalid cast from {:?} to pointer/non-pointer {:?}",
                        typed_inner.ty, target
                    );
                }
                _ => {}
            }

            TypedExpr {
                ty: target.clone(),
                kind: TypedExprKind::Cast {
                    expr: Box::new(typed_inner),
                    target: target.clone(),
                },
            }
        }
        ExprKind::Dereference(inner) => {
            let typed_inner = typecheck_expr(inner, symbols);

            match &typed_inner.ty {
                Type::Pointer { referenced } => TypedExpr {
                    ty: (**referenced).clone(),
                    kind: TypedExprKind::Dereference(Box::new(typed_inner)),
                },
                _ => panic!("Cannot dereference non-pointer."),
            }
        }
        ExprKind::AddrOf(inner) => {
            if !is_lvalue(inner) {
                panic!("Can't take the address of a non-lvalue!");
            }

            let typed_inner = typecheck_expr(inner, symbols);

            TypedExpr {
                ty: Type::Pointer {
                    referenced: Box::new(typed_inner.ty.clone()),
                },
                kind: TypedExprKind::AddrOf(Box::new(typed_inner)),
            }
        }

        ExprKind::ArrayLiteral(exprs) => {
            if exprs.is_empty() {
                panic!("Cannot infer type of empty array literal");
            }

            let typed_exprs: Vec<_> = exprs.iter().map(|e| typecheck_expr(e, symbols)).collect();

            let first_ty = &typed_exprs[0].ty;
            for e in &typed_exprs[1..] {
                if &e.ty != first_ty {
                    panic!(
                        "Array literal elements must have the same type, got {:?} and {:?}",
                        first_ty, e.ty
                    );
                }
            }

            TypedExpr {
                ty: Type::Array {
                    element_type: Box::new(first_ty.clone()),
                    size: typed_exprs.len() as i32,
                },
                kind: TypedExprKind::ArrayLiteral(typed_exprs),
            }
        }

        ExprKind::ArrayIndex(array_expr, index_expr) => {
            let typed_array = typecheck_expr(array_expr, symbols);
            let typed_index = typecheck_expr(index_expr, symbols);

            if typed_index.ty != Type::I32 {
                panic!("Array index must be I32, got {:?}", typed_index.ty);
            }

            let element_ty = match &typed_array.ty {
                Type::Array { element_type, .. } => (**element_type).clone(),
                Type::Slice { element_type } => (**element_type).clone(),
                other => panic!("Cannot index non-array/slice type {:?}", other),
            };

            TypedExpr {
                ty: element_ty,
                kind: TypedExprKind::ArrayIndex(Box::new(typed_array), Box::new(typed_index)),
            }
        }
        ExprKind::SliceFromArray(inner) => {
            let typed_inner = typecheck_expr(inner, symbols);

            let element_ty = match &typed_inner.ty {
                Type::Array { element_type, .. } => (**element_type).clone(),
                Type::Slice { element_type } => (**element_type).clone(),
                other => panic!("slice() expects an array or slice, got {:?}", other),
            };

            TypedExpr {
                ty: Type::Slice {
                    element_type: Box::new(element_ty),
                },
                kind: TypedExprKind::SliceFromArray(Box::new(typed_inner)),
            }
        }
        ExprKind::SliceLen(inner) => {
            let typed_inner = typecheck_expr(inner, symbols);

            match typed_inner.ty {
                Type::Slice { .. } => TypedExpr {
                    ty: Type::I32,
                    kind: TypedExprKind::SliceLen(Box::new(typed_inner)),
                },
                other => panic!("len() expects a slice, got {:?}", other),
            }
        }
    }
}

fn is_lvalue(expr: &Expr) -> bool {
    matches!(
        expr.kind,
        ExprKind::Var(_) | ExprKind::Dereference(_) | ExprKind::ArrayIndex(_, _)
    )
}
