use crate::ast::untyped_ast::*;
use std::collections::HashMap;

pub struct Context {
    pub templates: HashMap<String, Vec<FunDecl>>,
    pub locals: HashMap<String, Type>,
    pub monomorphized: HashMap<String, FunDecl>,
}

impl Context {
    fn new() -> Self {
        Self {
            templates: HashMap::new(),
            locals: HashMap::new(),
            monomorphized: HashMap::new(),
        }
    }

    fn add_template(&mut self, decl: FunDecl) {
        self.templates
            .entry(decl.name.clone())
            .or_default()
            .push(decl);
    }
}

pub fn typecheck(program: Program) -> Program {
    let mut ctx = Context::new();
    let Program::Program(funcs) = program;

    for f in funcs {
        ctx.add_template(f);
    }

    let template_names: Vec<String> = ctx.templates.keys().cloned().collect();
    for name in template_names {
        let overloads = ctx.templates.get(&name).unwrap().clone();
        for overload in overloads {
            if is_concrete_signature(&overload) {
                let mangled = mangle_name(&overload.name, &overload.params);
                if !ctx.monomorphized.contains_key(&mangled) {
                    typecheck_fun_instantiation(overload, HashMap::new(), &mut ctx);
                }
            }
        }
    }

    Program::Program(ctx.monomorphized.into_values().collect())
}

fn is_concrete_signature(f: &FunDecl) -> bool {
    f.params
        .iter()
        .all(|(_, t)| t.is_some() && !has_type_vars(t.as_ref().unwrap()))
}

fn has_type_vars(ty: &Type) -> bool {
    match ty {
        Type::TypeVar(_) => true,
        Type::Pointer { referenced } => has_type_vars(referenced),
        Type::Array { element_type, .. } => has_type_vars(element_type),
        _ => false,
    }
}

fn mangle_name(base_name: &str, params: &[(String, Option<Type>)]) -> String {
    let mut name = base_name.to_string();
    if !params.is_empty() {
        name.push('#')
    };
    for (_, ty) in params {
        if let Some(t) = ty {
            let s = format!("[{}]", t);
            name.push_str(&s);
        } else {
            name.push_str("_unknown");
        }
    }
    name
}

fn typecheck_fun_instantiation(
    decl: FunDecl,
    subst: HashMap<String, Type>,
    ctx: &mut Context,
) -> String {
    let mangled_name = if subst.is_empty() {
        mangle_name(&decl.name, &decl.params)
    } else {
        let resolved_params: Vec<(String, Option<Type>)> = decl
            .params
            .iter()
            .map(|(n, t)| (n.clone(), t.as_ref().map(|ty| apply_subst(ty, &subst))))
            .collect();
        mangle_name(&decl.name, &resolved_params)
    };

    if ctx.monomorphized.contains_key(&mangled_name) {
        return mangled_name;
    }
    let old_locals = std::mem::take(&mut ctx.locals);

    let typed_params: Vec<(String, Option<Type>)> = decl
        .params
        .iter()
        .map(|(n, t)| {
            let concrete_ty = t
                .as_ref()
                .map(|ty| apply_subst(ty, &subst))
                .expect("Inference failed");
            ctx.locals.insert(n.clone(), concrete_ty.clone());
            (n.clone(), Some(concrete_ty))
        })
        .collect();

    let ret_ty = decl
        .ret_type
        .as_ref()
        .map(|t| apply_subst(t, &subst))
        .unwrap_or(Type::Unit);
    ctx.monomorphized.insert(
        mangled_name.clone(),
        FunDecl {
            name: mangled_name.clone(),
            params: typed_params.clone(),
            ret_type: Some(ret_ty.clone()),
            body: None,
            exec_time: decl.exec_time,
        },
    );

    let body = decl.body.as_ref().map(|b| {
        let checked_block = typecheck_block(b, ctx);
        let Block::Block(_, last_expr) = &checked_block;
        if last_expr.ty.as_ref().unwrap() != &ret_ty {
            panic!("Mismatched return type in {}", mangled_name);
        }
        checked_block
    });

    ctx.monomorphized.get_mut(&mangled_name).unwrap().body = body;
    ctx.locals = old_locals;
    mangled_name
}

fn typecheck_block(block: &Block, ctx: &mut Context) -> Block {
    let Block::Block(items, last_expr) = block;
    let saved_locals = ctx.locals.clone();

    let typed_items = items
        .iter()
        .map(|item| match item {
            BlockItem::D(Decl::Variable(v)) => {
                let init = typecheck_expr( match &v.initializer {
                    Initializer::InitExpr(e) => e,},ctx,);
                let declared_ty = v.var_type.clone();
                let init_ty = init.ty.clone().expect("Initializer must have a type");

                if let Some(ref annotated_ty) = declared_ty {
                    if annotated_ty != &init_ty {
                        panic!(
                            "Type Mismatch in Decl: Variable '{}' declared as {}, but initialized with {}",
                            v.name, annotated_ty, init_ty
                        );
                    }
                }

                let var_ty = declared_ty.unwrap_or(init_ty);
                ctx.locals.insert(v.name.clone(), var_ty.clone());
                BlockItem::D(Decl::Variable(VarDecl {
                                name: v.name.clone(),
                                initializer: Initializer::InitExpr(init),
                                var_type: Some(var_ty),
                            }))
                        }
            BlockItem::UnitExpr(e) => BlockItem::UnitExpr(typecheck_expr(e, ctx)),
        })
        .collect();

    let typed_last = typecheck_expr(last_expr, ctx);
    ctx.locals = saved_locals;
    Block::Block(typed_items, typed_last)
}

fn typecheck_expr(expr: &Expr, ctx: &mut Context) -> Expr {
    match &expr.kind {
        ExprKind::Constant(c) => {
            let ty = match c {
                Const::I32(_) => Type::I32,
                Const::I64(_) => Type::I64,
                Const::F64(_) => Type::F64,
                Const::Char(_) => Type::Char,
                Const::Unit => Type::Unit,
            };
            Expr {
                ty: Some(ty),
                kind: expr.kind.clone(),
            }
        }

        ExprKind::Var(name) => {
            let ty = ctx.locals.get(name).cloned().unwrap_or_else(|| {
                panic!(
                    "Compiler Error: Undefined variable '{}'. Ensure identifier resolution ran.",
                    name
                )
            });
            Expr {
                ty: Some(ty),
                kind: ExprKind::Var(name.clone()),
            }
        }

        ExprKind::Binary(op, left, right) => {
            let l = typecheck_expr(left, ctx);
            let r = typecheck_expr(right, ctx);
            let lt = l.ty.as_ref().unwrap();
            let rt = r.ty.as_ref().unwrap();

            if lt != rt {
                panic!(
                    "Type Mismatch: Binary op {:?} expected same types, got {:?} and {:?}",
                    op, lt, rt
                );
            }

            let res_ty = match op {
                BinaryOp::Equal
                | BinaryOp::NotEqual
                | BinaryOp::LessThan
                | BinaryOp::GreaterThan
                | BinaryOp::LessOrEqual
                | BinaryOp::GreaterOrEqual => Type::I32,
                _ => lt.clone(),
            };

            Expr {
                ty: Some(res_ty),
                kind: ExprKind::Binary(op.clone(), Box::new(l), Box::new(r)),
            }
        }

        ExprKind::Unary(op, inner) => {
            let i = typecheck_expr(inner, ctx);
            let res_ty = i.ty.clone();
            Expr {
                ty: res_ty,
                kind: ExprKind::Unary(op.clone(), Box::new(i)),
            }
        }

        ExprKind::Assign(left, right) => {
            let l = typecheck_expr(left, ctx);
            if !is_lvalue(&l) {
                panic!("Left-hand side of assignment is not an lvalue");
            }
            let r = typecheck_expr(right, ctx);

            if l.ty != r.ty {
                panic!(
                    "Type Mismatch in Assignment: Cannot assign {:?} to {:?}",
                    r.ty, l.ty
                );
            }

            Expr {
                ty: Some(Type::Unit),
                kind: ExprKind::Assign(Box::new(l), Box::new(r)),
            }
        }

        ExprKind::IfThenElse(cond, then_b, else_b) => {
            let c = typecheck_expr(cond, ctx);
            let t = typecheck_expr(then_b, ctx);
            let e = typecheck_expr(else_b, ctx);

            if t.ty != e.ty {
                panic!(
                    "Type Mismatch: If branches must return same type. Got {:?} and {:?}",
                    t.ty, e.ty
                );
            }

            Expr {
                ty: t.ty.clone(),
                kind: ExprKind::IfThenElse(Box::new(c), Box::new(t), Box::new(e)),
            }
        }

        ExprKind::Compound(block) => {
            let checked_block = typecheck_block(block, ctx);
            let Block::Block(_, last) = &checked_block;
            Expr {
                ty: last.ty.clone(),
                kind: ExprKind::Compound(Box::new(checked_block)),
            }
        }

        ExprKind::FunctionCall(name, args) => {
            let typed_args: Vec<Expr> = args.iter().map(|a| typecheck_expr(a, ctx)).collect();
            let arg_tys: Vec<Type> = typed_args.iter().map(|a| a.ty.clone().unwrap()).collect();

            let (target_decl, final_subst) = {
                let templates = ctx
                    .templates
                    .get(name)
                    .unwrap_or_else(|| panic!("Function '{}' not found in templates.", name));

                let mut matched = None;
                for template in templates {
                    if template.params.len() != arg_tys.len() {
                        continue;
                    }
                    let mut subst = HashMap::new();
                    if template
                        .params
                        .iter()
                        .zip(&arg_tys)
                        .all(|((_, p_opt), a_ty)| {
                            let p_ty = p_opt.clone().unwrap_or(Type::TypeVar("_infer".to_string()));
                            unify(&p_ty, a_ty, &mut subst)
                        })
                    {
                        matched = Some((template.clone(), subst));
                        break;
                    }
                }
                matched.unwrap_or_else(|| {
                    panic!(
                        "No matching overload found for function '{}' with args {:?}",
                        name, arg_tys
                    )
                })
            };

            let mangled =
                typecheck_fun_instantiation(target_decl.clone(), final_subst.clone(), ctx);
            let ret_ty = apply_subst(
                target_decl.ret_type.as_ref().unwrap_or(&Type::Unit),
                &final_subst,
            );

            Expr {
                ty: Some(ret_ty),
                kind: ExprKind::FunctionCall(mangled, typed_args),
            }
        }

        ExprKind::Dereference(inner) => {
            let i = typecheck_expr(inner, ctx);
            let res_ty = match i.ty.as_ref().unwrap() {
                Type::Pointer { referenced } => *referenced.clone(),
                other => panic!("Cannot dereference non-pointer type: {:?}", other),
            };
            Expr {
                ty: Some(res_ty),
                kind: ExprKind::Dereference(Box::new(i)),
            }
        }

        ExprKind::AddrOf(inner) => {
            if !is_lvalue(inner) {
                panic!("Can't take the address of a non-lvalue!");
            }
            let typed_inner = typecheck_expr(inner, ctx);
            let res_ty = Type::Pointer {
                referenced: Box::new(typed_inner.ty.clone().unwrap()),
            };
            Expr {
                ty: Some(res_ty),
                kind: ExprKind::AddrOf(Box::new(typed_inner)),
            }
        }

        ExprKind::Cast {
            expr: inner,
            target,
        } => {
            let i = typecheck_expr(inner, ctx);
            Expr {
                ty: Some(target.clone()),
                kind: ExprKind::Cast {
                    expr: Box::new(i),
                    target: target.clone(),
                },
            }
        }

        ExprKind::ArrayIndex(arr, idx) => {
            let a = typecheck_expr(arr, ctx);
            let i = typecheck_expr(idx, ctx);

            if i.ty != Some(Type::I32) && i.ty != Some(Type::I64) {
                panic!("Array index must be an integer, got {:?}", i.ty);
            }

            let res_ty = match a.ty.as_ref().unwrap() {
                Type::Array { element_type, .. } => *element_type.clone(),
                Type::Pointer { referenced } => *referenced.clone(),
                _ => panic!("Cannot index into non-array/non-pointer type {:?}", a.ty),
            };
            Expr {
                ty: Some(res_ty),
                kind: ExprKind::ArrayIndex(Box::new(a), Box::new(i)),
            }
        }

        ExprKind::ArrayLiteral(elements) => {
            if elements.is_empty() {
                panic!("Empty array literals are not supported without explicit type.");
            }
            let typed_elms: Vec<Expr> = elements.iter().map(|e| typecheck_expr(e, ctx)).collect();
            let el_ty = typed_elms[0].ty.clone().unwrap();

            for (idx, e) in typed_elms.iter().enumerate() {
                if e.ty.as_ref().unwrap() != &el_ty {
                    panic!(
                        "Array literal element {} mismatch: expected {:?}, got {:?}",
                        idx, el_ty, e.ty
                    );
                }
            }

            let size = typed_elms.len() as i32;
            Expr {
                ty: Some(Type::Array {
                    element_type: Box::new(el_ty),
                    size,
                }),
                kind: ExprKind::ArrayLiteral(typed_elms),
            }
        }
    }
}

fn unify(expected: &Type, actual: &Type, subst: &mut HashMap<String, Type>) -> bool {
    match (expected, actual) {
        (Type::TypeVar(name), ty) => {
            if let Some(existing) = subst.get(name) {
                existing == ty
            } else {
                subst.insert(name.clone(), ty.clone());
                true
            }
        }
        (Type::Pointer { referenced: r1 }, Type::Pointer { referenced: r2 }) => {
            unify(r1, r2, subst)
        }
        (a, b) => a == b,
    }
}

fn apply_subst(ty: &Type, subst: &HashMap<String, Type>) -> Type {
    match ty {
        Type::TypeVar(n) => subst.get(n).cloned().unwrap_or(ty.clone()),
        Type::Pointer { referenced } => Type::Pointer {
            referenced: Box::new(apply_subst(referenced, subst)),
        },
        Type::Array { element_type, size } => Type::Array {
            element_type: Box::new(apply_subst(element_type, subst)),
            size: *size,
        },
        _ => ty.clone(),
    }
}

fn is_lvalue(expr: &Expr) -> bool {
    matches!(
        expr.kind,
        ExprKind::Var(_) | ExprKind::Dereference(_) | ExprKind::ArrayIndex(_, _)
    )
}
