use crate::ast::untyped_ast::*;
use crate::gen_names::make_loop_label;

pub fn loop_labeling_pass(program: Program) -> Program {
    match program {
        Program::Program(funcs) => {
            let funcs = funcs.into_iter().map(label_func_decl).collect();
            Program::Program(funcs)
        }
    }
}

fn label_func_decl(func: FunDecl) -> FunDecl {
    let FunDecl {
        name,
        params,
        body,
        ret_type,
        exec_time,
    } = func;

    let body = body.map(|b| label_block(b, None));

    FunDecl {
        name,
        params,
        body,
        ret_type,
        exec_time,
    }
}
fn label_block(block: Block, current_label: Option<String>) -> Block {
    let Block::Block(items, expr) = block;

    let new_items = items
        .into_iter()
        .map(|item| label_block_item(item, current_label.clone()))
        .collect::<Vec<_>>();
    let new_expr = match expr.kind {
        ExprKind::Compound(inner_block) => {
            let labeled_block = label_block(*inner_block, current_label.clone());
            Expr {
                ty: expr.ty,
                kind: ExprKind::Compound(Box::new(labeled_block)),
            }
        }
        _ => expr,
    };

    Block::Block(new_items, new_expr)
}

fn label_block_item(item: BlockItem, current_label: Option<String>) -> BlockItem {
    match item {
        BlockItem::D(d) => BlockItem::D(d),
        BlockItem::S(s) => BlockItem::S(label_statement(s, current_label)),
    }
}

pub fn label_statement(stmt: Stmt, current_label: Option<String>) -> Stmt {
    match stmt {
        Stmt::Break { .. } => {
            let label = current_label
                .as_ref()
                .expect("Semantic Error: 'break' statement outside of loop")
                .clone();
            Stmt::Break { label }
        }
        Stmt::Continue { .. } => {
            let label = current_label
                .as_ref()
                .expect("Semantic Error: 'continue' statement outside of loop")
                .clone();
            Stmt::Continue { label }
        }
        Stmt::While {
            condition, body, ..
        } => {
            let new_label = make_loop_label();
            let labeled_body = Box::new(label_statement(*body, Some(new_label.clone())));
            Stmt::While {
                condition,
                body: labeled_body,
                label: new_label,
            }
        }
        Stmt::Expression(_) | Stmt::Null => stmt,
    }
}
