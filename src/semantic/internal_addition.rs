use crate::ast::untyped_ast::*;
use crate::stdlib::{builtin_function_decls, builtin_function_names, is_stdlib_fun};
use std::collections::HashSet;

pub fn internal_addition_pass(program: Program) -> Program {
    let Program::Program(mut funcs) = program;

    let builtins: HashSet<String> = builtin_function_names()
        .into_iter()
        .map(|s| s.to_string())
        .collect();

    let mut internal_decls = builtin_function_decls();
    for decl in &mut internal_decls {
        decl.name = format!("internal#{}", decl.name);
    }

    for func in &mut funcs {
        rename_in_func(func, &builtins);
    }

    internal_decls.extend(funcs);
    Program::Program(internal_decls)
}

fn rename_in_func(func: &mut FunDecl, builtins: &HashSet<String>) {
    if let Some(ref mut block) = func.body {
        rename_in_block(block, builtins);
    }
}
fn rename_in_block(block: &mut Block, builtins: &HashSet<String>) {
    let Block::Block(items, last_expr) = block;
    for item in items {
        match item {
            BlockItem::D(Decl::Variable(v)) => {
                let Initializer::InitExpr(e) = &mut v.initializer;
                rename_in_expr(e, builtins);
            }
            BlockItem::UnitExpr(e) => rename_in_expr(e, builtins),
        }
    }
    rename_in_expr(last_expr, builtins);
}
fn rename_in_expr(expr: &mut Expr, builtins: &HashSet<String>) {
    match &mut expr.kind {
        ExprKind::FunctionCall(name, args) => {
            if is_stdlib_fun(name) {
                *name = format!("internal#{}", name);
            }
            for arg in args {
                rename_in_expr(arg, builtins);
            }
        }
        ExprKind::Unary(_, inner) => rename_in_expr(inner, builtins),
        ExprKind::Binary(_, left, right) => {
            rename_in_expr(left, builtins);
            rename_in_expr(right, builtins);
        }
        ExprKind::Assign(left, right) => {
            rename_in_expr(left, builtins);
            rename_in_expr(right, builtins);
        }
        ExprKind::IfThenElse(c, t, e) => {
            rename_in_expr(c, builtins);
            rename_in_expr(t, builtins);
            rename_in_expr(e, builtins);
        }
        ExprKind::Compound(block) => rename_in_block(block, builtins),
        ExprKind::Cast { expr, .. } => rename_in_expr(expr, builtins),
        ExprKind::Dereference(inner) => rename_in_expr(inner, builtins),
        ExprKind::AddrOf(inner) => rename_in_expr(inner, builtins),
        ExprKind::ArrayLiteral(exprs) => {
            for e in exprs {
                rename_in_expr(e, builtins);
            }
        }
        ExprKind::ArrayIndex(a, i) => {
            rename_in_expr(a, builtins);
            rename_in_expr(i, builtins);
        }
        _ => {}
    }
}
