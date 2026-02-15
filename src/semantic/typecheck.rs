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
    for (_, ty) in params {
        if let Some(t) = ty {
            let s = format!("_{}", t)
                .replace(" ", "")
                .replace("(", "")
                .replace(")", "")
                .replace(",", "_")
                .replace("->", "_to_")
                .replace("[", "arr_")
                .replace("]", "_")
                .replace(";", "_x_");
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

    let body = decl.body.as_ref().map(|b| {
        let checked_block = typecheck_block(b, ctx);
        let Block::Block(_, last_expr) = &checked_block;
        if last_expr.ty.as_ref().unwrap() != &ret_ty {
            panic!("Mismatched return type in {}", mangled_name);
        }
        checked_block
    });

    ctx.monomorphized.insert(
        mangled_name.clone(),
        FunDecl {
            name: mangled_name.clone(),
            params: typed_params,
            ret_type: Some(ret_ty),
            body,
            exec_time: decl.exec_time,
        },
    );

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
                let init = typecheck_expr(
                    match &v.initializer {
                        Initializer::InitExpr(e) => e,
                    },
                    ctx,
                );
                let var_ty = v
                    .var_type
                    .clone()
                    .or(init.ty.clone())
                    .expect("Cannot infer var type");
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
            let ty = ctx.locals.get(name).cloned().expect("Undefined variable");
            Expr {
                ty: Some(ty),
                kind: ExprKind::Var(name.clone()),
            }
        }
        ExprKind::FunctionCall(name, args) => {
            let typed_args: Vec<Expr> = args.iter().map(|a| typecheck_expr(a, ctx)).collect();
            let arg_tys: Vec<Type> = typed_args.iter().map(|a| a.ty.clone().unwrap()).collect();

            let (target_decl, final_subst) = {
                let templates = ctx.templates.get(name).expect("Function not found");
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
                matched.expect("No matching overload found")
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
        _ => todo!("Implement remaining expr kinds (Binary, Unary, etc) using similar logic"),
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
