use crate::ast::ast_type::*;
use crate::ast::typed_ast::*;
use crate::gen_names::*;

#[derive(Debug)]
pub enum TacProgram {
    Program(Vec<TacFuncDef>),
}

#[derive(Debug)]
pub enum TacFuncDef {
    Function {
        name: String,
        params: Vec<(String, Type)>,
        ret_type: Type,
        body: Vec<TacInstruction>,
    },
}

#[derive(Debug)]
pub enum TacInstruction {
    Return(Option<TacVal>),
    Truncate {
        src: TacVal,
        dest: TacVal,
    },
    SignExtend {
        src: TacVal,
        dest: TacVal,
    },
    F64ToI32 {
        src: TacVal,
        dest: TacVal,
    },
    F64ToI64 {
        src: TacVal,
        dest: TacVal,
    },
    I32ToF64 {
        src: TacVal,
        dest: TacVal,
    },
    I64ToF64 {
        src: TacVal,
        dest: TacVal,
    },
    Unary {
        op: TacUnaryOp,
        src: TacVal,
        dest: TacVal,
    },
    Binary {
        op: TacBinaryOp,
        src1: TacVal,
        src2: TacVal,
        dest: TacVal,
    },
    Copy {
        src: TacVal,
        dest: TacVal,
    },
    Jump {
        target: String,
    },
    JumpIfZero {
        condition: TacVal,
        target: String,
    },
    JumpIfNotZero {
        condition: TacVal,
        target: String,
    },
    Label(String),
    FunCall {
        fun_name: String,
        args: Vec<TacVal>,
        dest: TacVal,
    },
    GetAddress {
        src: TacVal,
        dest: TacVal,
    },
    Load {
        src_ptr: TacVal,
        dest: TacVal,
    },
    Store {
        src: TacVal,
        dest_ptr: TacVal,
    },
    AddPtr {
        ptr: TacVal,
        index: TacVal,
        scale: i32,
        dest: TacVal,
    },
    CopyToOffset {
        src: TacVal,
        dest: TacVal,
        offset: i32,
    },
    SetField {
        struct_var: TacVal,
        field_index: i32,
        src: TacVal,
    },
}

#[derive(Debug, Clone)]
pub enum TacVal {
    Constant(TacConst),
    Var(String, Type),
}

enum ExpResult {
    PlainOperand(TacVal),
    DereferencedPointer(TacVal),
}

#[derive(Debug, Clone)]
pub enum TacConst {
    I32(i32),
    I64(i64),
    F64(f64),
    Char(char),
}

#[derive(Debug, PartialEq)]
pub enum TacUnaryOp {
    Complement,
    Negate,
    Not,
}

#[derive(Debug)]
pub enum TacBinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    Equal,
    NotEqual,
    LessThan,
    LessOrEqual,
    GreaterThan,
    GreaterOrEqual,
}

pub fn gen_tac(program: TypedProgram) -> TacProgram {
    let tac_funcs = program.functions.into_iter().map(func_to_tac).collect();
    TacProgram::Program(tac_funcs)
}

fn func_to_tac(func: TypedFunDecl) -> TacFuncDef {
    let mut instructions = Vec::new();

    if let Some(body) = func.body.clone() {
        block_to_tac(body, &mut instructions);
    }

    TacFuncDef::Function {
        name: func.name,
        params: func.params,
        ret_type: func.ret_type,
        body: instructions,
    }
}

fn block_to_tac(block: TypedBlock, instructions: &mut Vec<TacInstruction>) {
    match block {
        TypedBlock::Block(items) => {
            for item in items {
                match item {
                    TypedBlockItem::S(stmt) => stmt_to_tac(stmt, instructions),
                    TypedBlockItem::D(decl) => decl_to_tac(decl, instructions),
                }
            }
        }
    }
}

fn decl_to_tac(decl: TypedDecl, instructions: &mut Vec<TacInstruction>) {
    match decl {
        TypedDecl::Variable(v) => {
            let val = expr_to_tac(v.init_expr, instructions);
            instructions.push(TacInstruction::Copy {
                src: val,
                dest: TacVal::Var(v.name, v.var_type),
            });
        }
    }
}

fn stmt_to_tac(stmt: TypedStmt, instructions: &mut Vec<TacInstruction>) {
    match stmt {
        TypedStmt::Return(expr) => {
            if expr.ty == Type::Unit {
                expr_to_tac(expr, instructions);
                instructions.push(TacInstruction::Return(None));
            } else {
                let val = expr_to_tac(expr, instructions);
                instructions.push(TacInstruction::Return(Some(val)));
            }
        }
        TypedStmt::Expr(expr) => {
            expr_to_tac(expr, instructions);
        }
        TypedStmt::Block(stmts) => {
            for s in stmts {
                stmt_to_tac(s, instructions);
            }
        }
        TypedStmt::While {
            condition,
            body,
            label,
        } => {
            let start_label = label.clone();
            let end_label = make_loop_label();

            instructions.push(TacInstruction::Label(start_label.clone()));
            let cond_val = expr_to_tac(condition, instructions);
            instructions.push(TacInstruction::JumpIfZero {
                condition: cond_val,
                target: end_label.clone(),
            });

            stmt_to_tac(*body, instructions);

            instructions.push(TacInstruction::Jump {
                target: start_label,
            });
            instructions.push(TacInstruction::Label(end_label));
        }
        TypedStmt::Break { label } => {
            instructions.push(TacInstruction::Jump { target: label });
        }
        TypedStmt::Continue { label } => {
            instructions.push(TacInstruction::Jump { target: label });
        }
        TypedStmt::Null => {}
    }
}
fn expr_to_exp_result(expr: TypedExpr, instructions: &mut Vec<TacInstruction>) -> ExpResult {
    match expr.kind {
        TypedExprKind::Dereference(inner) => {
            let ptr = expr_to_tac_and_convert(*inner, instructions);
            ExpResult::DereferencedPointer(ptr)
        }
        TypedExprKind::AddrOf(inner) => {
            let inner_res = expr_to_exp_result(*inner, instructions);
            match inner_res {
                ExpResult::PlainOperand(obj) => {
                    let dst = TacVal::Var(make_temporary(), expr.ty);
                    instructions.push(TacInstruction::GetAddress {
                        src: obj,
                        dest: dst.clone(),
                    });
                    ExpResult::PlainOperand(dst)
                }
                ExpResult::DereferencedPointer(ptr) => ExpResult::PlainOperand(ptr),
            }
        }
        _ => ExpResult::PlainOperand(expr_to_tac(expr, instructions)),
    }
}

fn expr_to_tac_and_convert(expr: TypedExpr, instructions: &mut Vec<TacInstruction>) -> TacVal {
    match expr_to_exp_result(expr.clone(), instructions) {
        ExpResult::PlainOperand(val) => val,
        ExpResult::DereferencedPointer(ptr) => {
            let dst = TacVal::Var(make_temporary(), get_type_of_expr(&expr));
            instructions.push(TacInstruction::Load {
                src_ptr: ptr,
                dest: dst.clone(),
            });
            dst
        }
    }
}

fn get_type_of_expr(expr: &TypedExpr) -> Type {
    expr.ty.clone()
}

fn assign_expr_to_tac(
    lhs: TypedExpr,
    rhs: TypedExpr,
    instructions: &mut Vec<TacInstruction>,
) -> TacVal {
    let lval_res = expr_to_exp_result(lhs, instructions);
    let rval_val = expr_to_tac_and_convert(rhs, instructions);

    match lval_res {
        ExpResult::PlainOperand(obj) => {
            instructions.push(TacInstruction::Copy {
                src: rval_val.clone(),
                dest: obj.clone(),
            });
            rval_val
        }
        ExpResult::DereferencedPointer(ptr) => {
            instructions.push(TacInstruction::Store {
                src: rval_val.clone(),
                dest_ptr: ptr.clone(),
            });
            rval_val
        }
    }
}

fn expr_to_tac(expr: TypedExpr, instructions: &mut Vec<TacInstruction>) -> TacVal {
    match expr.kind {
        TypedExprKind::Constant(Const::I32(v)) => TacVal::Constant(TacConst::I32(v)),
        TypedExprKind::Constant(Const::I64(v)) => TacVal::Constant(TacConst::I64(v)),
        TypedExprKind::Constant(Const::F64(v)) => TacVal::Constant(TacConst::F64(v)),
        TypedExprKind::Constant(Const::Unit) => {
            panic!("Internal error: Unit constant used as value in TAC generation");
        }
        TypedExprKind::Constant(Const::Char(v)) => TacVal::Constant(TacConst::Char(v)),
        TypedExprKind::Var(name) => TacVal::Var(name, expr.ty),
        TypedExprKind::Unary { op, expr: inner } => {
            let src = expr_to_tac(*inner, instructions);
            let dst = TacVal::Var(make_temporary(), expr.ty);
            instructions.push(TacInstruction::Unary {
                op: convert_unop(op),
                src,
                dest: dst.clone(),
            });
            dst
        }
        TypedExprKind::Binary { op, lhs, rhs } => {
            if matches!(op, BinaryOp::And | BinaryOp::Or) {
                return short_circuit_logic(op, *lhs, *rhs, instructions);
            }
            let src1 = expr_to_tac(*lhs, instructions);
            let src2 = expr_to_tac(*rhs, instructions);

            if let BinaryOp::Divide | BinaryOp::Remainder = op {
                if let TacVal::Constant(TacConst::I32(0))
                | TacVal::Constant(TacConst::I64(0))
                | TacVal::Constant(TacConst::F64(0.0)) = src2
                {
                    panic!(
                        "Compile-time error: Division or remainder by zero detected in expression {:?} / {:?}",
                        src1, src2
                    );
                }
            }

            let dst = TacVal::Var(make_temporary(), expr.ty);
            instructions.push(TacInstruction::Binary {
                op: convert_binop(op),
                src1,
                src2,
                dest: dst.clone(),
            });
            dst
        }
        TypedExprKind::IfThenElse {
            cond,
            then_expr,
            else_expr,
        } => {
            let result = TacVal::Var(make_temporary(), expr.ty);
            let else_label = make_cond_else();
            let end_label = make_cond_end();

            let c = expr_to_tac(*cond, instructions);
            instructions.push(TacInstruction::JumpIfZero {
                condition: c,
                target: else_label.clone(),
            });

            let v1 = expr_to_tac(*then_expr, instructions);
            instructions.push(TacInstruction::Copy {
                src: v1,
                dest: result.clone(),
            });
            instructions.push(TacInstruction::Jump {
                target: end_label.clone(),
            });

            instructions.push(TacInstruction::Label(else_label));
            let v2 = expr_to_tac(*else_expr, instructions);
            instructions.push(TacInstruction::Copy {
                src: v2,
                dest: result.clone(),
            });
            instructions.push(TacInstruction::Label(end_label));

            result
        }
        TypedExprKind::FunctionCall { name, args } => {
            let arg_vals = args
                .into_iter()
                .map(|a| expr_to_tac(a, instructions))
                .collect();
            let dst = TacVal::Var(make_temporary(), expr.ty);
            instructions.push(TacInstruction::FunCall {
                fun_name: name,
                args: arg_vals,
                dest: dst.clone(),
            });
            dst
        }
        TypedExprKind::Cast {
            expr: inner,
            target,
        } => {
            let src = expr_to_tac(*inner, instructions);
            let dst = TacVal::Var(make_temporary(), target.clone());

            match (&src, &target) {
                (TacVal::Var(_, Type::I64) | TacVal::Constant(TacConst::I64(_)), Type::I32) => {
                    instructions.push(TacInstruction::Truncate {
                        src,
                        dest: dst.clone(),
                    });
                }
                (TacVal::Var(_, Type::I32) | TacVal::Constant(TacConst::I32(_)), Type::I64) => {
                    instructions.push(TacInstruction::SignExtend {
                        src,
                        dest: dst.clone(),
                    });
                }

                (TacVal::Var(_, Type::F64) | TacVal::Constant(TacConst::F64(_)), Type::I32) => {
                    instructions.push(TacInstruction::F64ToI32 {
                        src,
                        dest: dst.clone(),
                    });
                }
                (TacVal::Var(_, Type::F64) | TacVal::Constant(TacConst::F64(_)), Type::I64) => {
                    instructions.push(TacInstruction::F64ToI64 {
                        src,
                        dest: dst.clone(),
                    });
                }

                (TacVal::Var(_, Type::I32) | TacVal::Constant(TacConst::I32(_)), Type::F64) => {
                    instructions.push(TacInstruction::I32ToF64 {
                        src,
                        dest: dst.clone(),
                    });
                }
                (TacVal::Var(_, Type::I64) | TacVal::Constant(TacConst::I64(_)), Type::F64) => {
                    instructions.push(TacInstruction::I64ToF64 {
                        src,
                        dest: dst.clone(),
                    });
                }

                (TacVal::Var(_, t1), t2) if t1 == t2 => {
                    instructions.push(TacInstruction::Copy {
                        src,
                        dest: dst.clone(),
                    });
                }
                (TacVal::Var(_, Type::Char) | TacVal::Constant(TacConst::Char(_)), Type::I32) => {
                    instructions.push(TacInstruction::Copy {
                        src,
                        dest: dst.clone(),
                    });
                }

                (TacVal::Var(_, Type::I32) | TacVal::Constant(TacConst::I32(_)), Type::Char) => {
                    instructions.push(TacInstruction::Copy {
                        src,
                        dest: dst.clone(),
                    });
                }

                _ => panic!("Unsupported cast {:?} → {:?}", src, target),
            }

            dst
        }
        TypedExprKind::Dereference(inner) => expr_to_tac_and_convert(
            TypedExpr {
                ty: expr.ty,
                kind: TypedExprKind::Dereference(inner),
            },
            instructions,
        ),
        TypedExprKind::AddrOf(inner) => expr_to_tac_and_convert(
            TypedExpr {
                ty: expr.ty,
                kind: TypedExprKind::AddrOf(inner),
            },
            instructions,
        ),
        TypedExprKind::Assign { lhs, rhs } => assign_expr_to_tac(*lhs, *rhs, instructions),
        TypedExprKind::ArrayLiteral(elems) => {
            let base = TacVal::Var(make_temporary(), expr.ty.clone());
            emit_array_init(&base, &expr.ty, elems, 0, instructions);
            base
        }

        TypedExprKind::ArrayIndex(array, index) => {
            let array_val = expr_to_tac(*array, instructions);
            let index_val = expr_to_tac(*index, instructions);

            let element_type = match array_val_type(&array_val) {
                Type::Array { element_type, .. } => element_type,
                Type::Slice { element_type } => element_type,
                _ => unreachable!("Expected array or slice"),
            };

            let elem_size = sizeof(&element_type);

            let base_ptr = match array_val_type(&array_val) {
                Type::Array { .. } => array_val.clone(),
                Type::Slice { .. } => {
                    let ptr = TacVal::Var(
                        make_temporary(),
                        Type::Pointer {
                            referenced: element_type.clone(),
                        },
                    );
                    instructions.push(TacInstruction::Load {
                        src_ptr: TacVal::Var(
                            match &array_val {
                                TacVal::Var(name, _) => name.clone(),
                                _ => panic!("Slice must be a var"),
                            },
                            array_val_type(&array_val),
                        ),
                        dest: ptr.clone(),
                    });
                    ptr
                }
                _ => unreachable!("Expected array or slice"),
            };

            let ptr = TacVal::Var(
                make_temporary(),
                Type::Pointer {
                    referenced: element_type.clone(),
                },
            );

            instructions.push(TacInstruction::AddPtr {
                ptr: base_ptr,
                index: index_val,
                scale: elem_size,
                dest: ptr.clone(),
            });

            match &*element_type {
                Type::Array { .. } => ptr,
                _ => {
                    let dst = TacVal::Var(make_temporary(), *element_type);
                    instructions.push(TacInstruction::Load {
                        src_ptr: ptr,
                        dest: dst.clone(),
                    });
                    dst
                }
            }
        }

        TypedExprKind::SliceFromArray(inner) => {
            let array_val = expr_to_tac(*inner, instructions);

            let (element_type, length_val) = match array_val_type(&array_val) {
                Type::Array { element_type, size } => (element_type, Some(size)),
                Type::Slice { element_type } => (element_type, None),
                _ => panic!("SliceFromArray expects Array or Slice"),
            };

            let slice_ty = expr.ty.clone();
            let slice_val = TacVal::Var(make_temporary(), slice_ty.clone());

            let ptr_field = TacVal::Var(
                make_temporary(),
                Type::Pointer {
                    referenced: element_type.clone(),
                },
            );
            let len_field = TacVal::Var(make_temporary(), Type::I32);

            instructions.push(TacInstruction::GetAddress {
                src: array_val.clone(),
                dest: ptr_field.clone(),
            });

            let len_src = if let Some(size) = length_val {
                TacVal::Constant(TacConst::I32(size))
            } else {
                TacVal::Var(
                    match &array_val {
                        TacVal::Var(name, _) => name.clone(),
                        _ => panic!("Slice must be a var"),
                    },
                    Type::I32,
                )
            };

            instructions.push(TacInstruction::Copy {
                src: len_src,
                dest: len_field.clone(),
            });

            instructions.push(TacInstruction::SetField {
                struct_var: slice_val.clone(),
                field_index: 0,
                src: ptr_field,
            });
            instructions.push(TacInstruction::SetField {
                struct_var: slice_val.clone(),
                field_index: 1,
                src: len_field,
            });

            slice_val
        }

        TypedExprKind::SliceLen(slice_expr) => {
            let slice_val = expr_to_tac(*slice_expr, instructions);

            let len_val = TacVal::Var(make_temporary(), Type::I32);

            instructions.push(TacInstruction::Load {
                src_ptr: TacVal::Var(
                    match &slice_val {
                        TacVal::Var(name, _) => name.clone(),
                        _ => panic!("Slice must be a var"),
                    },
                    array_val_type(&slice_val),
                ),
                dest: len_val.clone(),
            });

            len_val
        }
    }
}

fn emit_array_init(
    base: &TacVal,
    ty: &Type,
    elems: Vec<TypedExpr>,
    base_offset: i32,
    instructions: &mut Vec<TacInstruction>,
) {
    let Type::Array { element_type, .. } = ty else {
        unreachable!()
    };
    let elem_size = sizeof(element_type);
    for (i, elem) in elems.into_iter().enumerate() {
        let offset = base_offset + (i as i32) * elem_size;
        if let Type::Array { .. } = &**element_type {
            if let TypedExprKind::ArrayLiteral(inner) = elem.kind {
                emit_array_init(base, element_type, inner, offset, instructions);
                continue;
            }
        }
        let val = expr_to_tac(elem, instructions);
        instructions.push(TacInstruction::CopyToOffset {
            src: val,
            dest: base.clone(),
            offset,
        });
    }
}

fn short_circuit_logic(
    op: BinaryOp,
    expr1: TypedExpr,
    expr2: TypedExpr,
    instructions: &mut Vec<TacInstruction>,
) -> TacVal {
    let result_name = make_temporary();
    let result = TacVal::Var(result_name.clone(), Type::I32);
    match op {
        BinaryOp::And => {
            let false_label = make_and_false();
            let end_label = make_and_end();

            let v1 = expr_to_tac(expr1, instructions);
            instructions.push(TacInstruction::JumpIfZero {
                condition: v1.clone(),
                target: false_label.clone(),
            });
            let v2 = expr_to_tac(expr2, instructions);
            instructions.push(TacInstruction::JumpIfZero {
                condition: v2.clone(),
                target: false_label.clone(),
            });

            instructions.push(TacInstruction::Copy {
                src: TacVal::Constant(TacConst::I32(1)),
                dest: result.clone(),
            });
            instructions.push(TacInstruction::Jump {
                target: end_label.clone(),
            });
            instructions.push(TacInstruction::Label(false_label));
            instructions.push(TacInstruction::Copy {
                src: TacVal::Constant(TacConst::I32(0)),
                dest: result.clone(),
            });
            instructions.push(TacInstruction::Label(end_label));
        }
        BinaryOp::Or => {
            let true_label = make_or_true();
            let end_label = make_or_end();

            let v1 = expr_to_tac(expr1, instructions);
            instructions.push(TacInstruction::JumpIfNotZero {
                condition: v1,
                target: true_label.clone(),
            });
            let v2 = expr_to_tac(expr2, instructions);
            instructions.push(TacInstruction::JumpIfNotZero {
                condition: v2,
                target: true_label.clone(),
            });

            instructions.push(TacInstruction::Copy {
                src: TacVal::Constant(TacConst::I32(0)),
                dest: result.clone(),
            });
            instructions.push(TacInstruction::Jump {
                target: end_label.clone(),
            });
            instructions.push(TacInstruction::Label(true_label));
            instructions.push(TacInstruction::Copy {
                src: TacVal::Constant(TacConst::I32(1)),
                dest: result.clone(),
            });
            instructions.push(TacInstruction::Label(end_label));
        }
        _ => unreachable!(),
    }
    result
}

fn convert_unop(op: UnaryOp) -> TacUnaryOp {
    match op {
        UnaryOp::Negate => TacUnaryOp::Negate,
        UnaryOp::Complement => TacUnaryOp::Complement,
        UnaryOp::Not => TacUnaryOp::Not,
    }
}

fn convert_binop(op: BinaryOp) -> TacBinaryOp {
    match op {
        BinaryOp::Add => TacBinaryOp::Add,
        BinaryOp::Subtract => TacBinaryOp::Subtract,
        BinaryOp::Multiply => TacBinaryOp::Multiply,
        BinaryOp::Divide => TacBinaryOp::Divide,
        BinaryOp::Remainder => TacBinaryOp::Remainder,
        BinaryOp::Equal => TacBinaryOp::Equal,
        BinaryOp::NotEqual => TacBinaryOp::NotEqual,
        BinaryOp::LessThan => TacBinaryOp::LessThan,
        BinaryOp::LessOrEqual => TacBinaryOp::LessOrEqual,
        BinaryOp::GreaterThan => TacBinaryOp::GreaterThan,
        BinaryOp::GreaterOrEqual => TacBinaryOp::GreaterOrEqual,
        BinaryOp::And | BinaryOp::Or => unreachable!(),
    }
}

fn array_val_type(val: &TacVal) -> Type {
    match val {
        TacVal::Var(_, ty) => ty.clone(),
        _ => panic!("Expected array base to be a variable"),
    }
}

fn sizeof(ty: &Type) -> i32 {
    match ty {
        Type::I32 => 4,
        Type::I64 => 8,
        Type::F64 => 8,
        Type::Char => 4,
        Type::Pointer { .. } => 8,
        Type::Array { element_type, size } => size * sizeof(element_type),
        Type::Slice { .. } => 16,
        _ => panic!("Unsupported type for sizeof"),
    }
}
