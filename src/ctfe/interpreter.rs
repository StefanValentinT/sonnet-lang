use std::collections::HashMap;

use crate::ast::ast_type::{BinaryOp, Const, ExecTime, Type};
use crate::ast::typed_ast::{
    TypedBlock, TypedBlockItem, TypedDecl, TypedExpr, TypedExprKind, TypedFunDecl, TypedProgram,
    TypedStmt, TypedVarDecl,
};

/* ================= Errors ================= */
#[derive(Debug)]
pub enum EvalError {
    TypeMismatch(String),
    UndefinedVariable(String),
    Unsupported(String),
    DivisionByZero,
    RuntimeCall(String),
    InvalidLValue,
}

/* ================= Control Flow ================= */
#[derive(Debug)]
enum ControlFlow {
    None,
    Break,
    Continue,
    Return(TypedExpr),
}

/* ================= Values ================= */
#[derive(Clone, Debug)]
enum Value {
    Expr(TypedExpr),
    Address(usize),
}

/* ================= Environment ================= */
#[derive(Default)]
struct Scope {
    vars: HashMap<String, Value>,
}

#[derive(Default)]
struct Env {
    scopes: Vec<Scope>,
    memory: HashMap<usize, TypedExpr>,
    next_addr: usize,
}

impl Env {
    fn new() -> Self {
        let mut env = Self::default();
        env.push_scope();
        env
    }

    fn push_scope(&mut self) {
        self.scopes.push(Scope::default());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn define(&mut self, name: String, val: Value) {
        self.scopes.last_mut().unwrap().vars.insert(name, val);
    }

    fn assign(&mut self, name: &str, val: Value) -> Result<(), EvalError> {
        for scope in self.scopes.iter_mut().rev() {
            if scope.vars.contains_key(name) {
                scope.vars.insert(name.to_string(), val);
                return Ok(());
            }
        }
        Err(EvalError::UndefinedVariable(name.into()))
    }

    fn get(&self, name: &str) -> Option<Value> {
        for scope in self.scopes.iter().rev() {
            if let Some(v) = scope.vars.get(name) {
                return Some(v.clone());
            }
        }
        None
    }

    fn alloc(&mut self, value: TypedExpr) -> usize {
        let addr = self.next_addr;
        self.next_addr += 1;
        self.memory.insert(addr, value);
        addr
    }

    fn load_mem(&self, addr: usize) -> Result<Value, EvalError> {
        self.memory
            .get(&addr)
            .cloned()
            .map(Value::Expr)
            .ok_or(EvalError::InvalidLValue)
    }

    fn store_mem(&mut self, addr: usize, value: TypedExpr) -> Result<(), EvalError> {
        self.memory.insert(addr, value);
        Ok(())
    }
}

/* ================= Entry ================= */
pub fn resolve_program(program: &TypedProgram) -> Result<TypedExpr, EvalError> {
    let main = program
        .functions
        .iter()
        .find(|f| f.name == "main")
        .ok_or(EvalError::Unsupported("no main".into()))?;

    eval_function(main, program)
}

/* ================= Functions ================= */
fn eval_function(fun: &TypedFunDecl, program: &TypedProgram) -> Result<TypedExpr, EvalError> {
    if fun.exec_time != ExecTime::CompileTime {
        return Err(EvalError::RuntimeCall(fun.name.clone()));
    }

    let mut env = Env::new();
    let body = fun
        .body
        .as_ref()
        .ok_or(EvalError::Unsupported("extern".into()))?;

    eval_block(body, &mut env, program)?.ok_or(EvalError::Unsupported("no return".into()))
}

/* ================= Blocks ================= */
fn eval_block(
    block: &TypedBlock,
    env: &mut Env,
    program: &TypedProgram,
) -> Result<Option<TypedExpr>, EvalError> {
    if let TypedBlock::Block(items) = block {
        env.push_scope();
        for item in items {
            let cf = match item {
                TypedBlockItem::D(decl) => {
                    eval_decl(decl, env, program)?;
                    ControlFlow::None
                }
                TypedBlockItem::S(stmt) => eval_stmt(stmt, env, program)?,
            };

            if let ControlFlow::Return(v) = cf {
                env.pop_scope();
                return Ok(Some(v));
            }
        }
        env.pop_scope();
    }
    Ok(None)
}

/* ================= Decls ================= */
fn eval_decl(decl: &TypedDecl, env: &mut Env, program: &TypedProgram) -> Result<(), EvalError> {
    match decl {
        TypedDecl::Variable(TypedVarDecl {
            name, init_expr, ..
        }) => {
            let val = eval_expr(init_expr, env, program)?;
            env.define(name.clone(), Value::Expr(val));
        }
    }
    Ok(())
}

/* ================= Statements ================= */
fn eval_stmt(
    stmt: &TypedStmt,
    env: &mut Env,
    program: &TypedProgram,
) -> Result<ControlFlow, EvalError> {
    match stmt {
        TypedStmt::Expr(expr) => {
            eval_expr(expr, env, program)?;
            Ok(ControlFlow::None)
        }
        TypedStmt::Return(expr) => {
            let val = eval_expr(expr, env, program)?;
            Ok(ControlFlow::Return(val))
        }
        TypedStmt::Block(stmts) => {
            env.push_scope();
            for s in stmts {
                let cf = eval_stmt(s, env, program)?;
                match cf {
                    ControlFlow::None => {}
                    _ => {
                        env.pop_scope();
                        return Ok(cf);
                    }
                }
            }
            env.pop_scope();
            Ok(ControlFlow::None)
        }
        TypedStmt::While {
            condition, body, ..
        } => {
            loop {
                let c = eval_expr(condition, env, program)?;
                if extract_i32(&c)? == 0 {
                    break;
                }
                match eval_stmt(body, env, program)? {
                    ControlFlow::None => {}
                    ControlFlow::Break => break,
                    ControlFlow::Continue => continue,
                    ControlFlow::Return(v) => return Ok(ControlFlow::Return(v)),
                }
            }
            Ok(ControlFlow::None)
        }
        TypedStmt::Break { .. } => Ok(ControlFlow::Break),
        TypedStmt::Continue { .. } => Ok(ControlFlow::Continue),
        TypedStmt::Null => Ok(ControlFlow::None),
    }
}

/* ================= Expressions ================= */
fn eval_expr(
    expr: &TypedExpr,
    env: &mut Env,
    program: &TypedProgram,
) -> Result<TypedExpr, EvalError> {
    match &expr.kind {
        TypedExprKind::Constant(_) => Ok(expr.clone()),
        TypedExprKind::Var(name) => match env.get(name) {
            Some(Value::Expr(e)) => Ok(e),
            Some(Value::Address(a)) => match env.load_mem(a)? {
                Value::Expr(e) => Ok(e),
                _ => Err(EvalError::TypeMismatch("expected scalar".into())),
            },
            None => Err(EvalError::UndefinedVariable(name.clone())),
        },
        TypedExprKind::Assign { lhs, rhs } => {
            let value = eval_expr(rhs, env, program)?;
            match &lhs.kind {
                TypedExprKind::Var(name) => {
                    env.assign(name, Value::Expr(value.clone()))?;
                    Ok(value)
                }
                TypedExprKind::Dereference(ptr) => {
                    let addr = extract_addr(&eval_expr(ptr, env, program)?)?;
                    env.store_mem(addr, value.clone())?;
                    Ok(value)
                }
                _ => Err(EvalError::InvalidLValue),
            }
        }
        TypedExprKind::Binary { op, lhs, rhs } => {
            let l = eval_expr(lhs, env, program)?;
            let r = eval_expr(rhs, env, program)?;
            eval_binary(op.clone(), l, r)
        }
        TypedExprKind::Unary { op: _, expr: inner } => {
            let v = eval_expr(inner, env, program)?;
            let i = extract_i32(&v)?;
            Ok(TypedExpr {
                ty: Type::I32,
                kind: TypedExprKind::Constant(Const::I32(-i)),
            })
        }
        TypedExprKind::IfThenElse {
            cond,
            then_expr,
            else_expr,
        } => {
            let c = eval_expr(cond, env, program)?;
            if extract_i32(&c)? != 0 {
                eval_expr(then_expr, env, program)
            } else {
                eval_expr(else_expr, env, program)
            }
        }
        TypedExprKind::AddrOf(inner) => {
            if let TypedExprKind::Var(name) = &inner.kind {
                let val = env
                    .get(name)
                    .ok_or(EvalError::UndefinedVariable(name.clone()))?;
                let addr = match val {
                    Value::Expr(e) => env.alloc(e),
                    Value::Address(a) => a,
                };
                Ok(TypedExpr {
                    ty: expr.ty.clone(),
                    kind: TypedExprKind::Constant(Const::I64(addr as i64)),
                })
            } else {
                Err(EvalError::InvalidLValue)
            }
        }
        TypedExprKind::Dereference(ptr) => {
            let addr = extract_addr(&eval_expr(ptr, env, program)?)?;
            match env.load_mem(addr)? {
                Value::Expr(e) => Ok(e),
                _ => Err(EvalError::TypeMismatch("expected scalar".into())),
            }
        }
        TypedExprKind::FunctionCall { name, args } => {
            let fun = program
                .functions
                .iter()
                .find(|f| &f.name == name)
                .ok_or(EvalError::UndefinedVariable(name.clone()))?;

            if fun.exec_time != ExecTime::CompileTime {
                return Err(EvalError::RuntimeCall(name.clone()));
            }

            let mut local = Env::new();
            for ((pname, _), arg) in fun.params.iter().zip(args.iter()) {
                let v = eval_expr(arg, env, program)?;
                local.define(pname.clone(), Value::Expr(v));
            }

            eval_function(fun, program)
        }
        TypedExprKind::SliceFromArray(_) | TypedExprKind::SliceLen(_) => {
            Err(EvalError::Unsupported("slice ops in CTFE".into()))
        }
        TypedExprKind::Cast {
            expr: inner,
            target,
        } => {
            let v = eval_expr(inner, env, program)?;
            cast_value(v, target.clone())
        }
        TypedExprKind::ArrayLiteral(_) | TypedExprKind::ArrayIndex(_, _) => {
            Err(EvalError::Unsupported("array in CTFE".into()))
        }
    }
}

/* ================= Helpers ================= */
fn extract_i32(expr: &TypedExpr) -> Result<i32, EvalError> {
    match &expr.kind {
        TypedExprKind::Constant(Const::I32(v)) => Ok(*v),
        TypedExprKind::Constant(Const::I64(v)) => Ok(*v as i32),
        TypedExprKind::Constant(Const::F64(v)) => Ok(*v as i32),
        TypedExprKind::Constant(Const::Char(c)) => Ok(*c as i32),
        _ => Err(EvalError::TypeMismatch("numeric".into())),
    }
}

fn extract_addr(expr: &TypedExpr) -> Result<usize, EvalError> {
    match &expr.kind {
        TypedExprKind::Constant(Const::I64(v)) => Ok(*v as usize),
        TypedExprKind::Constant(Const::I32(v)) => Ok(*v as usize),
        _ => Err(EvalError::TypeMismatch("pointer".into())),
    }
}

fn eval_binary(op: BinaryOp, lhs: TypedExpr, rhs: TypedExpr) -> Result<TypedExpr, EvalError> {
    let a = extract_i32(&lhs)?;
    let b = extract_i32(&rhs)?;

    let r = match op {
        BinaryOp::Add => a + b,
        BinaryOp::Subtract => a - b,
        BinaryOp::Multiply => a * b,
        BinaryOp::Divide => {
            if b == 0 {
                return Err(EvalError::DivisionByZero);
            }
            a / b
        }
        _ => return Err(EvalError::Unsupported("binary op".into())),
    };

    Ok(TypedExpr {
        ty: Type::I32,
        kind: TypedExprKind::Constant(Const::I32(r)),
    })
}

fn cast_value(expr: TypedExpr, target: Type) -> Result<TypedExpr, EvalError> {
    let v = extract_i32(&expr)?;
    Ok(TypedExpr {
        ty: target,
        kind: TypedExprKind::Constant(Const::I32(v)),
    })
}
