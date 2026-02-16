use crate::ast::untyped_ast::*;
use crate::gen_names::make_temporary;
use std::collections::HashMap;

#[derive(Clone, Debug)]
struct MapEntry {
    unique_name: String,
    from_current_scope: bool,
}

pub fn identifier_resolution_pass(program: Program) -> Program {
    match program {
        Program::Program(funcs) => {
            let mut global_map: HashMap<String, MapEntry> = HashMap::new();

            for f in &funcs {
                if let Some(prev) = global_map.get(&f.name) {
                    if prev.from_current_scope {
                        panic!("Duplicate function definition: {}", f.name);
                    }
                }

                global_map.insert(
                    f.name.clone(),
                    MapEntry {
                        unique_name: f.name.clone(),
                        from_current_scope: true,
                    },
                );
            }

            let funcs = funcs
                .into_iter()
                .map(|f| resolve_fun_decl(f, &mut global_map))
                .collect();

            Program::Program(funcs)
        }
    }
}

fn resolve_block(block: Block, identifier_map: &mut HashMap<String, MapEntry>) -> Block {
    let Block::Block(items, last_expr) = block;
    let mut local_map = identifier_map.clone();
    let new_items = items
        .into_iter()
        .map(|item| resolve_block_item(item, &mut local_map))
        .collect();

    let new_last_expr = resolve_expr(last_expr, &mut local_map);
    Block::Block(new_items, new_last_expr)
}

fn resolve_block_item(
    item: BlockItem,
    identifier_map: &mut HashMap<String, MapEntry>,
) -> BlockItem {
    match item {
        BlockItem::D(decl) => BlockItem::D(resolve_decl(decl, identifier_map)),
        BlockItem::UnitExpr(stmt) => BlockItem::UnitExpr(resolve_stmt(stmt, identifier_map)),
    }
}

fn resolve_decl(decl: Decl, identifier_map: &mut HashMap<String, MapEntry>) -> Decl {
    match decl {
        Decl::Variable(var_decl) => Decl::Variable(resolve_var_decl(var_decl, identifier_map)),
    }
}

fn resolve_var_decl(var_decl: VarDecl, identifier_map: &mut HashMap<String, MapEntry>) -> VarDecl {
    let VarDecl {
        name,
        initializer,
        var_type,
    } = var_decl;
    let init_expr = match initializer {
        Initializer::InitExpr(expr) => expr,
    };
    if let Some(prev) = identifier_map.get(&name) {
        if prev.from_current_scope {
            panic!("Duplicate variable declaration: {}", name);
        }
    }

    let unique_name = make_temporary();
    identifier_map.insert(
        name.clone(),
        MapEntry {
            unique_name: unique_name.clone(),
            from_current_scope: true,
        },
    );

    let resolved_expr = Initializer::InitExpr(resolve_expr(init_expr, identifier_map));

    VarDecl {
        name: unique_name,
        initializer: resolved_expr,
        var_type,
    }
}

fn resolve_fun_decl(decl: FunDecl, identifier_map: &mut HashMap<String, MapEntry>) -> FunDecl {
    let mut local_map = copy_identifier_map(identifier_map);

    let new_params: Vec<(String, Option<Type>)> = decl
        .params
        .into_iter()
        .map(|(pname, ptype)| {
            let unique_name = resolve_param(pname, &mut local_map);
            (unique_name, ptype)
        })
        .collect();

    let new_body = decl.body.map(|b| resolve_block(b, &mut local_map));

    FunDecl {
        name: decl.name,
        params: new_params,
        body: new_body,
        ret_type: decl.ret_type,
        exec_time: decl.exec_time,
    }
}

fn resolve_param(param_name: String, identifier_map: &mut HashMap<String, MapEntry>) -> String {
    if let Some(prev) = identifier_map.get(&param_name) {
        if prev.from_current_scope {
            panic!("Duplicate parameter name: {}", param_name);
        }
    }

    let unique_name = make_temporary();
    identifier_map.insert(
        param_name.clone(),
        MapEntry {
            unique_name: unique_name.clone(),
            from_current_scope: true,
        },
    );

    unique_name
}

fn resolve_stmt(stmt: Expr, identifier_map: &mut HashMap<String, MapEntry>) -> Expr {
    resolve_expr(stmt, identifier_map)
}

fn resolve_expr(expr: Expr, identifier_map: &mut HashMap<String, MapEntry>) -> Expr {
    let ty = expr.ty.clone();
    match expr.kind {
        ExprKind::Constant(_) => expr,
        ExprKind::Var(v) => {
            if let Some(entry) = identifier_map.get(&v) {
                Expr {
                    ty,
                    kind: ExprKind::Var(entry.unique_name.clone()),
                }
            } else {
                panic!("Undeclared variable: {}", v);
            }
        }
        ExprKind::Compound(block) => {
            let Block::Block(items, last_expr) = *block;

            let mut local_map = identifier_map.clone();

            let new_items = items
                .into_iter()
                .map(|item| resolve_block_item(item, &mut local_map))
                .collect::<Vec<_>>();

            let new_last_expr = resolve_expr(last_expr, &mut local_map);

            Expr {
                ty: new_last_expr.ty.clone(),
                kind: ExprKind::Compound(Box::new(Block::Block(new_items, new_last_expr))),
            }
        }
        ExprKind::Unary(op, inner) => {
            let inner = resolve_expr(*inner, identifier_map);
            Expr {
                ty,
                kind: ExprKind::Unary(op, Box::new(inner)),
            }
        }
        ExprKind::Binary(op, left, right) => {
            let left = resolve_expr(*left, identifier_map);
            let right = resolve_expr(*right, identifier_map);
            Expr {
                ty,
                kind: ExprKind::Binary(op, Box::new(left), Box::new(right)),
            }
        }
        ExprKind::Assign(left, right) => {
            let left = resolve_expr(*left, identifier_map);
            let right = resolve_expr(*right, identifier_map);
            Expr {
                ty,
                kind: ExprKind::Assign(Box::new(left), Box::new(right)),
            }
        }
        ExprKind::IfThenElse(cond, then_expr, else_expr) => {
            let cond = resolve_expr(*cond, identifier_map);
            let then_expr = resolve_expr(*then_expr, identifier_map);
            let else_expr = resolve_expr(*else_expr, identifier_map);
            Expr {
                ty,
                kind: ExprKind::IfThenElse(
                    Box::new(cond),
                    Box::new(then_expr),
                    Box::new(else_expr),
                ),
            }
        }
        ExprKind::FunctionCall(name, args) => {
            if let Some(entry) = identifier_map.get(&name) {
                let func_unique_name = entry.unique_name.clone();
                let new_args = args
                    .into_iter()
                    .map(|a| resolve_expr(a, identifier_map))
                    .collect();
                Expr {
                    ty,
                    kind: ExprKind::FunctionCall(func_unique_name, new_args),
                }
            } else {
                panic!("Undeclared function: {}", name);
            }
        }
        ExprKind::Cast {
            expr: inner,
            target,
        } => {
            let inner = resolve_expr(*inner, identifier_map);
            Expr {
                ty: Some(target.clone()),
                kind: ExprKind::Cast {
                    expr: Box::new(inner),
                    target,
                },
            }
        }
        ExprKind::Dereference(inner) => {
            let inner_resolved = resolve_expr(*inner, identifier_map);
            Expr {
                ty: ty,
                kind: ExprKind::Dereference(Box::new(inner_resolved)),
            }
        }
        ExprKind::AddrOf(inner) => {
            let inner_resolved = resolve_expr(*inner, identifier_map);
            Expr {
                ty: ty,
                kind: ExprKind::AddrOf(Box::new(inner_resolved)),
            }
        }
        ExprKind::ArrayLiteral(exprs) => {
            let resolved_exprs = exprs
                .into_iter()
                .map(|e| resolve_expr(e, identifier_map))
                .collect();
            Expr {
                ty,
                kind: ExprKind::ArrayLiteral(resolved_exprs),
            }
        }
        ExprKind::ArrayIndex(array_expr, index_expr) => {
            let array_resolved = resolve_expr(*array_expr, identifier_map);
            let index_resolved = resolve_expr(*index_expr, identifier_map);
            Expr {
                ty,
                kind: ExprKind::ArrayIndex(Box::new(array_resolved), Box::new(index_resolved)),
            }
        }
        ExprKind::TypeExpr(t) => Expr {
            ty,
            kind: ExprKind::TypeExpr(t),
        },
    }
}

fn copy_identifier_map(identifier_map: &HashMap<String, MapEntry>) -> HashMap<String, MapEntry> {
    identifier_map
        .iter()
        .map(|(k, v)| {
            (
                k.clone(),
                MapEntry {
                    unique_name: v.unique_name.clone(),
                    from_current_scope: false,
                },
            )
        })
        .collect()
}
