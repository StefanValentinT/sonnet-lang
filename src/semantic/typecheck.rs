use std::collections::HashMap;

use crate::ast::ast_type::*;
use crate::ast::untyped_ast::*;
use crate::stdlib::*;

#[derive(Clone)]
struct SymbolEntry {
    ty: Type,
}

type SymbolTable = HashMap<String, SymbolEntry>;

pub fn typecheck(program: Program) -> Program {
    let mut symbols = SymbolTable::new();

    let funcs = match program {
        Program::Program(funcs) => funcs,
    };

    for fun in builtin_function_decls() {
        let fun_ty = Type::FunType {
            params: fun.params.iter().map(|(_, t)| t.clone()).collect(),
            ret: Box::new(fun.ret_type.clone()),
        };
        symbols.insert(fun.name, SymbolEntry { ty: fun_ty });
    }

    for f in &funcs {
        declare_function(f, &mut symbols);
    }

    let typed_funs: Vec<FunDecl> = funcs
        .into_iter()
        .map(|f| typecheck_fun_decl(f, &symbols))
        .collect();

    Program::Program(typed_funs)
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

fn typecheck_fun_decl(decl: FunDecl, globals: &SymbolTable) -> FunDecl {
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

    let typed_params: Vec<(String, Option<Type>)> = decl
        .params
        .into_iter()
        .zip(param_types.into_iter())
        .map(|((name, _), ty)| {
            locals.insert(
                name.clone(),
                SymbolEntry {
                    ty: ty.clone().expect("Function param type missing"),
                },
            );
            (name, ty)
        })
        .collect();

    let body = decl.body.map(|b| {
        let typed_block = typecheck_block(&b, &mut locals);

        let Block::Block(_, last_expr) = &typed_block;
        if last_expr.ty != ret_ty.clone() {
            panic!(
                "Function {} expected return type {:?}, got {:?}",
                decl.name, ret_ty, last_expr.ty
            );
        }

        typed_block
    });

    FunDecl {
        name: decl.name,
        params: typed_params,
        ret_type: ret_ty,
        body,
        exec_time: decl.exec_time,
    }
}
fn typecheck_block(block: &Block, symbols: &mut SymbolTable) -> Block {
    let Block::Block(items, last_expr) = block;
    let mut local_symbols = symbols.clone();

    let typed_items: Vec<BlockItem> = items
        .iter()
        .map(|item| match item {
            BlockItem::D(d) => BlockItem::D(typecheck_decl(d, &mut local_symbols)),
            BlockItem::UnitExpr(s) => {
                let e = typecheck_expr(s, &mut local_symbols);
                if e.ty != Some(Type::Unit) {
                    panic!("Value not bound in compound!");
                }
                BlockItem::UnitExpr(e)
            }
        })
        .collect();

    let typed_last = typecheck_expr(last_expr, &mut local_symbols);

    Block::Block(typed_items, typed_last)
}

fn typecheck_decl(decl: &Decl, symbols: &mut SymbolTable) -> Decl {
    match decl {
        Decl::Variable(v) => {
            let init_expr = match &v.initializer {
                Initializer::InitExpr(expr) => expr,
            };
            let init = typecheck_expr(&init_expr, symbols);
            if init.ty != v.var_type.clone() {
                panic!(
                    "Type mismatch in variable '{}': expected {:?}, got {:?}",
                    v.name, v.var_type, init.ty
                );
            }

            symbols.insert(
                v.name.clone(),
                SymbolEntry {
                    ty: v.var_type.clone().unwrap(),
                },
            );

            Decl::Variable(VarDecl {
                name: v.name.clone(),
                initializer: Initializer::InitExpr(init),
                var_type: v.var_type.clone(),
            })
        }
    }
}

fn convert_by_assignment(e: Expr, target: &Type) -> Expr {
    if e.ty == Some(target.clone()) {
        e
    } else {
        panic!(
            "Cannot convert type {:?} to {:?} as if by assignment",
            e.ty, target
        );
    }
}

fn typecheck_expr(expr: &Expr, symbols: &mut SymbolTable) -> Expr {
    match &expr.kind {
        ExprKind::Constant(Const::I32(v)) => Expr {
            ty: Some(Type::I32),
            kind: ExprKind::Constant(Const::I32(*v)),
        },
        ExprKind::Constant(Const::I64(v)) => Expr {
            ty: Some(Type::I64),
            kind: ExprKind::Constant(Const::I64(*v)),
        },
        ExprKind::Constant(Const::F64(v)) => Expr {
            ty: Some(Type::F64),
            kind: ExprKind::Constant(Const::F64(*v)),
        },
        ExprKind::Constant(Const::Char(v)) => Expr {
            ty: Some(Type::Char),
            kind: ExprKind::Constant(Const::Char(*v)),
        },
        ExprKind::Constant(Const::Unit) => Expr {
            ty: Some(Type::Unit),
            kind: ExprKind::Constant(Const::Unit),
        },
        ExprKind::Compound(block) => {
            let typed_block = typecheck_block(block, symbols);
            let Block::Block(_, last) = &typed_block;

            Expr {
                ty: last.ty.clone(),
                kind: ExprKind::Compound(Box::new(typed_block)),
            }
        }

        ExprKind::Var(name) => {
            let entry = symbols
                .get(name)
                .unwrap_or_else(|| panic!("Undefined variable {}", name));
            Expr {
                ty: Some(entry.ty.clone()),
                kind: ExprKind::Var(name.clone()),
            }
        }
        ExprKind::Unary(op, e) => {
            let typed_inner = typecheck_expr(e, symbols);

            match (&op, &typed_inner.ty) {
                (UnaryOp::Negate, Some(Type::Pointer { .. }))
                | (UnaryOp::Complement, Some(Type::Pointer { .. })) => {
                    panic!("Cannot apply {:?} to a pointer type", op);
                }
                _ => {}
            }

            Expr {
                ty: typed_inner.ty.clone(),
                kind: ExprKind::Unary(op.clone(), Box::new(typed_inner)),
            }
        }
        ExprKind::Binary(op, lhs, rhs) => {
            let l = typecheck_expr(lhs, symbols);
            let r = typecheck_expr(rhs, symbols);

            match (&l.ty, &r.ty) {
                (Some(Type::Pointer { .. }), Some(Type::Pointer { .. })) => match op {
                    BinaryOp::Equal | BinaryOp::NotEqual | BinaryOp::And | BinaryOp::Or => {}
                    _ => panic!("Illegal operation on pointer types: {:?}", op),
                },
                _ if l.ty == r.ty => {}
                _ => panic!("Binary op type mismatch: {:?} vs {:?}", l.ty, r.ty),
            }

            Expr {
                ty: match op {
                    BinaryOp::Equal | BinaryOp::NotEqual | BinaryOp::And | BinaryOp::Or => {
                        Some(Type::I32)
                    }
                    _ => l.ty.clone(),
                },
                kind: ExprKind::Binary(op.clone(), Box::new(l), Box::new(r)),
            }
        }
        ExprKind::Assign(lhs, rhs) => {
            let l = typecheck_expr(lhs, symbols);

            if !is_lvalue(lhs) {
                panic!("Left-hand side of assignment is not an lvalue");
            }

            let r = typecheck_expr(rhs, symbols);
            let r_conv = convert_by_assignment(r, &(l.ty.clone().unwrap()));

            Expr {
                ty: Some(Type::Unit),
                kind: ExprKind::Assign(Box::new(l), Box::new(r_conv)),
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

            Expr {
                ty: t.ty.clone(),
                kind: ExprKind::IfThenElse(Box::new(c), Box::new(t), Box::new(e)),
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
                if arg.ty != param_ty.clone() {
                    panic!(
                        "Argument type mismatch in call to {}: expected {:?}, got {:?}",
                        name, param_ty, arg.ty
                    );
                }
            }

            Expr {
                ty: ret,
                kind: ExprKind::FunctionCall(name.clone(), typed_args),
            }
        }
        ExprKind::Cast { expr, target } => {
            let typed_inner = typecheck_expr(expr, symbols);

            match (&typed_inner.ty, target) {
                (Some(Type::Pointer { .. }), Type::I32 | Type::I64 | Type::F64)
                | (Some(Type::I32) | Some(Type::I64) | Some(Type::F64), Type::Pointer { .. }) => {
                    panic!(
                        "Invalid cast from {:?} to pointer/non-pointer {:?}",
                        typed_inner.ty, target
                    );
                }
                _ => {}
            }

            Expr {
                ty: Some(target.clone()),
                kind: ExprKind::Cast {
                    expr: Box::new(typed_inner),
                    target: target.clone(),
                },
            }
        }
        ExprKind::Dereference(inner) => {
            let typed_inner = typecheck_expr(inner, symbols);

            match &typed_inner.ty {
                Some(Type::Pointer { referenced }) => Expr {
                    ty: Some((**referenced).clone()),
                    kind: ExprKind::Dereference(Box::new(typed_inner)),
                },
                _ => panic!("Cannot dereference non-pointer."),
            }
        }
        ExprKind::AddrOf(inner) => {
            if !is_lvalue(inner) {
                panic!("Can't take the address of a non-lvalue!");
            }

            let typed_inner = typecheck_expr(inner, symbols);

            Expr {
                ty: Some(Type::Pointer {
                    referenced: Box::new(typed_inner.ty.clone().unwrap()),
                }),
                kind: ExprKind::AddrOf(Box::new(typed_inner)),
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

            Expr {
                ty: Some(Type::Array {
                    element_type: Box::new(first_ty.as_ref().unwrap().clone()),
                    size: typed_exprs.len() as i32,
                }),
                kind: ExprKind::ArrayLiteral(typed_exprs),
            }
        }

        ExprKind::ArrayIndex(array_expr, index_expr) => {
            let typed_array = typecheck_expr(array_expr, symbols);
            let typed_index = typecheck_expr(index_expr, symbols);

            if typed_index.ty != Some(Type::I32) {
                panic!("Array index must be I32, got {:?}", typed_index.ty);
            }

            let element_ty = match &typed_array.ty {
                Some(Type::Array { element_type, .. }) => (**element_type).clone(),
                other => panic!("Cannot index non-array/slice type {:?}", other),
            };

            Expr {
                ty: Some(element_ty),
                kind: ExprKind::ArrayIndex(Box::new(typed_array), Box::new(typed_index)),
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
