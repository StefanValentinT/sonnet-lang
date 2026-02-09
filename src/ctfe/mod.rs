use crate::{
    ast::{
        ast_type::{Const, ExecTime, Type},
        untyped_ast::*,
    },
    stdlib::is_stdlib_fun,
};

//mod interpreter;
//use interpreter::*;

use std::collections::HashSet;

pub fn perform_ctfe_pass(mut program: Program) -> Result<Program, String> {
    /*
    let num_functions = program.functions.len();
    for i in 0..num_functions {
        let fun = program.functions[i].clone();

        if !is_compile_time_function(&fun) {
            continue;
        }

        let mut visited = HashSet::new();
        let lifted_funcs = lift_called(&fun, &program, &mut visited);

        let mut ctfe_fun = fun.clone();
        ctfe_fun.name = "main".to_string();

        let mut sub_program = TypedProgram {
            functions: lifted_funcs,
        };
        sub_program.functions.push(ctfe_fun);

        let result = resolve_program(&sub_program).map_err(|e| format!("{:?}", e))?;

        let new_body = match fun.ret_type.clone() {
            Type::Unit => {
                TypedBlock::Block(vec![TypedBlockItem::S(TypedStmt::Return(TypedExpr {
                    ty: Type::Unit,
                    kind: TypedExprKind::Constant(Const::Unit),
                }))])
            }
            _ => TypedBlock::Block(vec![TypedBlockItem::S(TypedStmt::Return(result))]),
        };

        let orig_fun = &mut program.functions[i];
        orig_fun.body = Some(new_body);
        orig_fun.exec_time = ExecTime::Runtime;
        orig_fun.params.clear();
    }*/

    Ok(program)
} /*
fn lift_called(
caller: &TypedFunDecl,
program: &TypedProgram,
visited: &mut HashSet<String>,
) -> Vec<TypedFunDecl> {
if visited.contains(&caller.name) {
return vec![];
}
visited.insert(caller.name.clone());

let mut called_names = Vec::new();

if let Some(body) = &caller.body {
if let TypedBlock::Block(items) = body {
for item in items {
if let TypedBlockItem::S(stmt) = item {
collect_calls_stmt(stmt, &mut called_names);
}
}
}
}

let mut lifted = Vec::new();

for name in called_names {
if let Some(f) = program
.functions
.iter()
.find(|f| f.name == name && !is_stdlib_fun(f.clone()))
{
lifted.push((*f).clone());
lifted.extend(lift_called(f, program, visited));
}
}

lifted
}

fn collect_calls_stmt(stmt: &TypedStmt, out: &mut Vec<String>) {
match stmt {
TypedStmt::Return(expr) | TypedStmt::Expr(expr) => collect_calls_expr(expr, out),
TypedStmt::Block(stmts) => {
for s in stmts {
collect_calls_stmt(s, out);
}
}
TypedStmt::While {
condition, body, ..
} => {
collect_calls_expr(condition, out);
collect_calls_stmt(body, out);
}
_ => {}
}
}

fn collect_calls_expr(expr: &TypedExpr, out: &mut Vec<String>) {
match &expr.kind {
TypedExprKind::FunctionCall { name, args } => {
out.push(name.clone());
for arg in args {
collect_calls_expr(arg, out);
}
}
TypedExprKind::Unary { expr, .. } => collect_calls_expr(expr, out),
TypedExprKind::Binary { lhs, rhs, .. } => {
collect_calls_expr(lhs, out);
collect_calls_expr(rhs, out);
}
TypedExprKind::Assign { lhs, rhs } => {
collect_calls_expr(lhs, out);
collect_calls_expr(rhs, out);
}
TypedExprKind::IfThenElse {
cond,
then_expr,
else_expr,
} => {
collect_calls_expr(cond, out);
collect_calls_expr(then_expr, out);
collect_calls_expr(else_expr, out);
}
TypedExprKind::Dereference(e)
| TypedExprKind::AddrOf(e)
| TypedExprKind::SliceFromArray(e)
| TypedExprKind::SliceLen(e)
| TypedExprKind::Cast { expr: e, .. } => collect_calls_expr(e, out),
TypedExprKind::ArrayLiteral(exprs) => {
for e in exprs {
collect_calls_expr(e, out);
}
}
TypedExprKind::ArrayIndex(a, b) => {
collect_calls_expr(a, out);
collect_calls_expr(b, out);
}
_ => {}
}
}

pub fn is_compile_time_function(func: &TypedFunDecl) -> bool {
matches!(func.exec_time, ExecTime::CompileTime)
}
*/
