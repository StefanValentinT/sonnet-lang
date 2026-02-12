use std::collections::HashSet;

use crate::{
    ast::ast_type::Type,
    tac::{TacBinaryOp, TacConst, TacFuncDef, TacInstruction, TacProgram, TacUnaryOp, TacVal},
};

pub fn emit_llvm(program: TacProgram) -> String {
    let TacProgram::Program(funcs) = program;

    let mut defined = HashSet::new();
    for f in &funcs {
        let TacFuncDef::Function { name, .. } = f;
        defined.insert(name.clone());
    }

    let mut reg_counter = 0;
    let mut out = String::new();

    for f in funcs {
        out.push_str(&emit_function(
            &f,
            &defined,
            &mut HashSet::new(),
            &mut reg_counter,
        ));
        out.push('\n');
    }

    out.push_str(&llvm_c_stdlib());

    out
}

fn emit_function(
    func: &TacFuncDef,
    defined: &HashSet<String>,
    _externs: &mut HashSet<String>,
    reg_counter: &mut usize,
) -> String {
    *reg_counter = 0;

    let TacFuncDef::Function {
        name,
        params,
        ret_type,
        body,
    } = func;

    if body.is_empty() {
        return format!(
            "declare {} @{}({})\n",
            llvm_type(ret_type),
            name,
            params
                .iter()
                .map(|(_, ty)| llvm_type(ty))
                .collect::<Vec<_>>()
                .join(", ")
        );
    }

    let locals = collect_locals(body, params);

    let mut out = String::new();
    let ret_llvm_ty = llvm_type(ret_type);

    out.push_str(&format!(
        "define {} @{}({}) {{\n",
        ret_llvm_ty,
        name,
        params
            .iter()
            .map(|(p, ty)| format!("{} %arg_{}", llvm_type(ty), p))
            .collect::<Vec<_>>()
            .join(", ")
    ));

    out.push_str("entry:\n");

    for (v, ty) in &locals {
        if !params.iter().any(|(p, _)| p == v) && *ty != Type::Unit {
            out.push_str(&format!("  %{} = alloca {}\n", v, llvm_type(ty)));
        }
    }

    for (p, ty) in params {
        let llvm_ty = llvm_type(ty);
        out.push_str(&format!(
            "  %{} = alloca {}\n  store {} %arg_{}, {}* %{}\n",
            p, llvm_ty, llvm_ty, p, llvm_ty, p
        ));
    }
    let mut terminated = false;

    for instr in body {
        if let TacInstruction::Label(l) = instr {
            if !terminated {
                out.push_str(&format!("  br label %{}\n", l));
            }
            out.push_str(&format!("{}:\n", l));
            terminated = false;
            continue;
        }

        out.push_str(&emit_instr(
            instr,
            defined,
            &mut HashSet::new(),
            reg_counter,
        ));

        terminated = matches!(
            instr,
            TacInstruction::Return(_)
                | TacInstruction::Jump { .. }
                | TacInstruction::JumpIfZero { .. }
                | TacInstruction::JumpIfNotZero { .. }
        );
    }

    if !terminated {
        match ret_type {
            Type::Unit => out.push_str("  ret void\n"),
            _ => out.push_str("  unreachable\n"),
        }
    }
    out.push_str("}\n");
    out
}
fn llvm_c_stdlib() -> String {
    r#"
declare i32 @putchar(i32)
declare i32 @getchar()

declare i32 @printf(i8*, ...)
declare i32 @snprintf(i8*, i64, i8*, ...)
declare i8* @malloc(i64)
declare void @free(i8*)

%Slice = type { i8*, i64 }

declare %Slice @show_i32(i32)
declare %Slice @show_i64(i64)
declare %Slice @show_f64(double)
declare %Slice @show_char(i32)
declare %Slice @show_unit()

"#
    .into()
}

fn emit_instr(
    instr: &TacInstruction,
    defined: &HashSet<String>,
    externs: &mut HashSet<String>,
    reg_counter: &mut usize,
) -> String {
    match instr {
        TacInstruction::Return(v_opt) => match v_opt {
            None => "  ret void\n".to_string(),
            Some(TacVal::Var(_, Type::Unit)) => "  ret void\n".to_string(),
            Some(val) => {
                let (load, val_str, ty) = load_val(val, reg_counter);
                format!("{}  ret {} {}\n", load, ty, val_str)
            }
        },
        TacInstruction::Copy { src, dest } => {
            let (load, val, ty) = load_val(src, reg_counter);
            format!(
                "{}  store {} {}, {}* %{}\n",
                load,
                ty,
                val,
                ty,
                var_name(dest)
            )
        }
        TacInstruction::Unary { op, src, dest } => {
            let (load, val, ty) = load_val(src, reg_counter);
            let r = fresh_reg(reg_counter);

            let op_ir = match op {
                TacUnaryOp::Negate => format!("sub {} 0, {}", ty, val),
                TacUnaryOp::Complement => format!("xor {} {}, -1", ty, val),
                TacUnaryOp::Not => {
                    let c = fresh_reg(reg_counter);
                    return format!(
                        "{}  {} = icmp eq {} {}, 0\n  {} = zext i1 {} to i32\n  store i32 {}, i32* %{}\n",
                        load,
                        c,
                        ty,
                        val,
                        r,
                        c,
                        r,
                        var_name(dest)
                    );
                }
            };

            format!(
                "{}  {} = {}\n  store {} {}, {}* %{}\n",
                load,
                r,
                op_ir,
                ty,
                r,
                ty,
                var_name(dest)
            )
        }
        TacInstruction::Binary {
            op,
            src1,
            src2,
            dest,
        } => {
            let (a_load, a_val, ty) = load_val(src1, reg_counter);
            let (b_load, b_val, _) = load_val(src2, reg_counter);
            let r = fresh_reg(reg_counter);

            let mut ir = String::new();
            ir.push_str(&a_load);
            ir.push_str(&b_load);

            match op {
                TacBinaryOp::Add | TacBinaryOp::Subtract | TacBinaryOp::Multiply => {
                    if ty == "double" {
                        let op_name = match op {
                            TacBinaryOp::Add => "fadd",
                            TacBinaryOp::Subtract => "fsub",
                            TacBinaryOp::Multiply => "fmul",
                            _ => unreachable!(),
                        };
                        ir.push_str(&format!(
                            "  {} = {} double {}, {}\n  store double {}, double* %{}\n",
                            r,
                            op_name,
                            a_val,
                            b_val,
                            r,
                            var_name(dest)
                        ));
                    } else {
                        let intrinsic_ty = if ty == "i64" {
                            match op {
                                TacBinaryOp::Add => "llvm.sadd.with.overflow.i64",
                                TacBinaryOp::Subtract => "llvm.ssub.with.overflow.i64",
                                TacBinaryOp::Multiply => "llvm.smul.with.overflow.i64",
                                _ => unreachable!(),
                            }
                        } else {
                            match op {
                                TacBinaryOp::Add => "llvm.sadd.with.overflow.i32",
                                TacBinaryOp::Subtract => "llvm.ssub.with.overflow.i32",
                                TacBinaryOp::Multiply => "llvm.smul.with.overflow.i32",
                                _ => unreachable!(),
                            }
                        };

                        let res_struct = fresh_reg(reg_counter);
                        let res_val = fresh_reg(reg_counter);
                        let overflow = fresh_reg(reg_counter);
                        let ok_label = format!("ok{}", reg_counter);
                        let trap_label = format!("trap{}", reg_counter);

                        ir.push_str(&format!(
                            "  {} = call {{ {}, i1 }} @{}({} {}, {} {})\n\
             {} = extractvalue {{ {}, i1 }} {}, 0\n\
             {} = extractvalue {{ {}, i1 }} {}, 1\n\
             br i1 {}, label %{}, label %{}\n",
                            res_struct,
                            ty,
                            intrinsic_ty,
                            ty,
                            a_val,
                            ty,
                            b_val,
                            res_val,
                            ty,
                            res_struct,
                            overflow,
                            ty,
                            res_struct,
                            overflow,
                            trap_label,
                            ok_label
                        ));

                        ir.push_str(&format!(
                            "{}:\n  call void @llvm.trap()\n  unreachable\n",
                            trap_label
                        ));

                        ir.push_str(&format!(
                            "{}:\n  store {} {}, {}* %{}\n",
                            ok_label,
                            ty,
                            res_val,
                            ty,
                            var_name(dest)
                        ));
                    }
                }
                TacBinaryOp::Divide | TacBinaryOp::Remainder => {
                    if ty == "double" {
                        let op_name = match op {
                            TacBinaryOp::Divide => "fdiv",
                            TacBinaryOp::Remainder => "frem",
                            _ => unreachable!(),
                        };
                        ir.push_str(&format!(
                            "  {} = {} double {}, {}\n  store double {}, double* %{}\n",
                            r,
                            op_name,
                            a_val,
                            b_val,
                            r,
                            var_name(dest)
                        ));
                    } else {
                        let op_name = match op {
                            TacBinaryOp::Divide => "sdiv nsw",
                            TacBinaryOp::Remainder => "srem",
                            _ => unreachable!(),
                        };
                        ir.push_str(&format!(
                            "  {} = {} {} {}, {}\n  store {} {}, {}* %{}\n",
                            r,
                            op_name,
                            ty,
                            a_val,
                            b_val,
                            ty,
                            r,
                            ty,
                            var_name(dest)
                        ));
                    }
                }

                TacBinaryOp::Equal
                | TacBinaryOp::NotEqual
                | TacBinaryOp::LessThan
                | TacBinaryOp::LessOrEqual
                | TacBinaryOp::GreaterThan
                | TacBinaryOp::GreaterOrEqual => {
                    let r_icmp = fresh_reg(reg_counter);
                    let cmp_instr = if ty == "double" {
                        let pred = match op {
                            TacBinaryOp::Equal => "oeq",
                            TacBinaryOp::NotEqual => "one",
                            TacBinaryOp::LessThan => "olt",
                            TacBinaryOp::LessOrEqual => "ole",
                            TacBinaryOp::GreaterThan => "ogt",
                            TacBinaryOp::GreaterOrEqual => "oge",
                            _ => unreachable!(),
                        };
                        format!("{} = fcmp {} double {}, {}", r_icmp, pred, a_val, b_val)
                    } else {
                        let pred = match op {
                            TacBinaryOp::Equal => "eq",
                            TacBinaryOp::NotEqual => "ne",
                            TacBinaryOp::LessThan => "slt",
                            TacBinaryOp::LessOrEqual => "sle",
                            TacBinaryOp::GreaterThan => "sgt",
                            TacBinaryOp::GreaterOrEqual => "sge",
                            _ => unreachable!(),
                        };
                        format!("{} = icmp {} {} {}, {}", r_icmp, pred, ty, a_val, b_val)
                    };
                    let zext = fresh_reg(reg_counter);
                    ir.push_str(&format!(
                        "  {}\n  {} = zext i1 {} to i32\n  store i32 {}, i32* %{}\n",
                        cmp_instr,
                        zext,
                        r_icmp,
                        zext,
                        var_name(dest)
                    ));
                }
            }

            ir
        }
        TacInstruction::Jump { target } => format!("  br label %{}\n", target),
        TacInstruction::JumpIfZero { condition, target } => {
            let (load, v, ty) = load_val(condition, reg_counter);
            let r = fresh_reg(reg_counter);
            let cont = format!("cont{}", reg_counter);

            format!(
                "{}  {} = icmp eq {} {}, 0\n  br i1 {}, label %{}, label %{}\n{}:\n",
                load, r, ty, v, r, target, cont, cont
            )
        }
        TacInstruction::JumpIfNotZero { condition, target } => {
            let (load, v, ty) = load_val(condition, reg_counter);
            let r = fresh_reg(reg_counter);
            let cont = format!("cont{}", reg_counter);

            format!(
                "{}  {} = icmp ne {} {}, 0\n  br i1 {}, label %{}, label %{}\n{}:\n",
                load, r, ty, v, r, target, cont, cont
            )
        }
        TacInstruction::FunCall {
            fun_name,
            args,
            dest,
        } => {
            if !defined.contains(fun_name) {
                externs.insert(fun_name.clone());
            }

            let mut ir = String::new();
            let mut arg_list = vec![];

            for a in args {
                let (load, v, ty) = load_val(a, reg_counter);
                ir.push_str(&load);
                arg_list.push(format!("{} {}", ty, v));
            }

            match dest {
                TacVal::Var(_, Type::Unit) => {
                    ir.push_str(&format!(
                        "  call void @{}({})\n",
                        fun_name,
                        arg_list.join(", ")
                    ));
                }
                TacVal::Var(name, ty) => {
                    let r = fresh_reg(reg_counter);
                    let llvm_ty = llvm_type(ty);

                    ir.push_str(&format!(
                        "  {} = call {} @{}({})\n  store {} {}, {}* %{}\n",
                        r,
                        llvm_ty,
                        fun_name,
                        arg_list.join(", "),
                        llvm_ty,
                        r,
                        llvm_ty,
                        name
                    ));
                }
                _ => unreachable!(),
            }

            ir
        }
        TacInstruction::Label(label) => format!("{}:\n", label),
        TacInstruction::Truncate { src, dest } => {
            let (load, v, src_ty) = load_val(src, reg_counter);
            let dst_ty = llvm_type(match dest {
                TacVal::Var(_, ty) => ty,
                _ => unreachable!(),
            });

            let r = fresh_reg(reg_counter);

            format!(
                "{}  {} = trunc {} {} to {}\n  store {} {}, {}* %{}\n",
                load,
                r,
                src_ty,
                v,
                dst_ty,
                dst_ty,
                r,
                dst_ty,
                var_name(dest)
            )
        }
        TacInstruction::SignExtend { src, dest } => {
            let (load, v, src_ty) = load_val(src, reg_counter);
            let dst_ty = llvm_type(match dest {
                TacVal::Var(_, ty) => ty,
                _ => unreachable!(),
            });

            let r = fresh_reg(reg_counter);

            format!(
                "{}  {} = sext {} {} to {}\n  store {} {}, {}* %{}\n",
                load,
                r,
                src_ty,
                v,
                dst_ty,
                dst_ty,
                r,
                dst_ty,
                var_name(dest)
            )
        }
        TacInstruction::F64ToI32 { src, dest } => {
            let (load, v, _) = load_val(src, reg_counter);
            let r = fresh_reg(reg_counter);

            format!(
                "{}  {} = fptosi double {} to i32\n  store i32 {}, i32* %{}\n",
                load,
                r,
                v,
                r,
                var_name(dest)
            )
        }
        TacInstruction::F64ToI64 { src, dest } => {
            let (load, v, _) = load_val(src, reg_counter);
            let r = fresh_reg(reg_counter);

            format!(
                "{}  {} = fptosi double {} to i64\n  store i64 {}, i64* %{}\n",
                load,
                r,
                v,
                r,
                var_name(dest)
            )
        }
        TacInstruction::I32ToF64 { src, dest } => {
            let (load, v, _) = load_val(src, reg_counter);
            let r = fresh_reg(reg_counter);

            format!(
                "{}  {} = sitofp i32 {} to double\n  store double {}, double* %{}\n",
                load,
                r,
                v,
                r,
                var_name(dest)
            )
        }
        TacInstruction::I64ToF64 { src, dest } => {
            let (load, v, _) = load_val(src, reg_counter);
            let r = fresh_reg(reg_counter);

            format!(
                "{}  {} = sitofp i64 {} to double\n  store double {}, double* %{}\n",
                load,
                r,
                v,
                r,
                var_name(dest)
            )
        }
        TacInstruction::GetAddress { src, dest } => {
            let llvm_ty = llvm_type(&match src {
                TacVal::Var(_, t) => t.clone(),
                _ => panic!("GetAddress source must be a variable"),
            });
            let dst_ty = llvm_type(&match dest {
                TacVal::Var(_, t) => t.clone(),
                _ => unreachable!(),
            });

            let r = fresh_reg(reg_counter);
            format!(
                "  {} = getelementptr {}, {}* %{}, i32 0\n  store {} {}, {}* %{}\n",
                r,
                llvm_ty,
                llvm_ty,
                var_name(src),
                dst_ty,
                r,
                dst_ty,
                var_name(dest)
            )
        }
        TacInstruction::Load { src_ptr, dest } => {
            let (load_ir, v, ty) = load_val(src_ptr, reg_counter);
            let dst_ty = llvm_type(&match dest {
                TacVal::Var(_, t) => t.clone(),
                _ => unreachable!(),
            });

            let r = fresh_reg(reg_counter);
            format!(
                "{}  {} = load {}, {}* {}\n  store {} {}, {}* %{}\n",
                load_ir,
                r,
                dst_ty,
                dst_ty,
                v,
                dst_ty,
                r,
                dst_ty,
                var_name(dest)
            )
        }
        TacInstruction::Store { src, dest_ptr } => {
            let (src_load, val, ty) = load_val(src, reg_counter);
            let (ptr_load, ptr_val, ptr_ty) = load_val(dest_ptr, reg_counter);
            format!(
                "{}{}  store {} {}, {}* {}\n",
                src_load, ptr_load, ty, val, ty, ptr_val
            )
        }
        TacInstruction::AddPtr {
            ptr,
            index,
            scale: _,
            dest,
        } => {
            let (idx_load, idx_val, _) = load_val(index, reg_counter);

            let (array_ty, array_size) = match ptr {
                TacVal::Var(_, Type::Array { element_type, size }) => {
                    (llvm_type(element_type), *size)
                }
                _ => unreachable!("AddPtr ptr must be an array variable"),
            };

            let r = fresh_reg(reg_counter);

            format!(
                "{}  {} = getelementptr inbounds [{} x {}], [{} x {}]* %{}, i64 0, i64 {}\n  store {}* {}, {}** %{}\n",
                idx_load,
                r,
                array_size,
                array_ty,
                array_size,
                array_ty,
                var_name(ptr),
                idx_val,
                array_ty,
                r,
                array_ty,
                var_name(dest)
            )
        }
        TacInstruction::CopyToOffset { src, dest, offset } => {
            let (load, v, elem_ty) = load_val(src, reg_counter);

            let (array_ty, array_size) = match dest {
                TacVal::Var(_, Type::Array { element_type, size }) => {
                    (llvm_type(element_type), *size)
                }
                _ => panic!("CopyToOffset destination must be an array variable"),
            };

            let elem_ptr = fresh_reg(reg_counter);

            let elem_size = match elem_ty.as_str() {
                "i32" => 4,
                "i64" => 8,
                "double" => 8,
                _ => panic!("unsupported element size"),
            };

            let index = offset / elem_size;

            format!(
                "{}  {} = getelementptr inbounds [{} x {}], [{} x {}]* %{}, i64 0, i64 {}\n  store {} {}, {}* {}\n",
                load,
                elem_ptr,
                array_size,
                array_ty,
                array_size,
                array_ty,
                var_name(dest),
                index,
                elem_ty,
                v,
                elem_ty,
                elem_ptr
            )
        }
    }
}

fn load_val(v: &TacVal, reg_counter: &mut usize) -> (String, String, String) {
    match v {
        TacVal::Constant(TacConst::I32(c)) => ("".into(), c.to_string(), "i32".into()),
        TacVal::Constant(TacConst::I64(c)) => ("".into(), c.to_string(), "i64".into()),
        TacVal::Constant(TacConst::F64(c)) => ("".into(), format!("{:.6e}", c), "double".into()),
        TacVal::Constant(TacConst::Char(c)) => ("".into(), (*c as u32).to_string(), "i32".into()),
        TacVal::Var(_, Type::Unit) => ("".into(), "".into(), "void".into()),
        TacVal::Var(name, ty) => {
            let r = fresh_reg(reg_counter);
            let llvm_ty = llvm_type(ty);
            (
                format!("  {} = load {}, {}* %{}\n", r, llvm_ty, llvm_ty, name),
                r,
                llvm_ty,
            )
        }
    }
}

fn llvm_type(ty: &Type) -> String {
    match ty {
        Type::I32 => "i32".into(),
        Type::I64 => "i64".into(),
        Type::F64 => "double".into(),
        Type::Char => "i32".into(),
        Type::Unit => "void".into(),
        Type::Pointer { referenced } => format!("{}*", llvm_type(referenced)),
        Type::Array { element_type, size } => format!("[{} x {}]", size, llvm_type(element_type)),
        Type::FunType { .. } => unreachable!("Function types are not first-class in LLVM"),
    }
}

fn fresh_reg(reg_counter: &mut usize) -> String {
    let r = format!("%r{}", *reg_counter);
    *reg_counter += 1;
    r
}

fn var_name(v: &TacVal) -> &str {
    match v {
        TacVal::Var(name, _) => name,
        _ => panic!("expected variable"),
    }
}

fn collect_locals(body: &[TacInstruction], params: &[(String, Type)]) -> HashSet<(String, Type)> {
    let mut vars = HashSet::new();

    for (p, ty) in params {
        vars.insert((p.clone(), ty.clone()));
    }

    fn collect_val(v: &TacVal, vars: &mut HashSet<(String, Type)>) {
        if let TacVal::Var(name, ty) = v {
            vars.insert((name.clone(), ty.clone()));
        }
    }

    for instr in body {
        match instr {
            TacInstruction::Copy { src, dest }
            | TacInstruction::Unary { src, dest, .. }
            | TacInstruction::Truncate { src, dest }
            | TacInstruction::SignExtend { src, dest } => {
                collect_val(src, &mut vars);
                collect_val(dest, &mut vars);
            }

            TacInstruction::Binary {
                src1, src2, dest, ..
            } => {
                collect_val(src1, &mut vars);
                collect_val(src2, &mut vars);
                collect_val(dest, &mut vars);
            }

            TacInstruction::FunCall { args, dest, .. } => {
                for a in args {
                    collect_val(a, &mut vars);
                }
                collect_val(dest, &mut vars);
            }

            TacInstruction::Load { src_ptr, dest } => {
                collect_val(src_ptr, &mut vars);
                collect_val(dest, &mut vars);
            }

            TacInstruction::Store { src, dest_ptr } => {
                collect_val(src, &mut vars);
                collect_val(dest_ptr, &mut vars);
            }

            TacInstruction::GetAddress { src, dest } => {
                collect_val(src, &mut vars);
                collect_val(dest, &mut vars);
            }

            TacInstruction::Return(Some(v)) => {
                collect_val(v, &mut vars);
            }

            TacInstruction::JumpIfZero { condition, .. }
            | TacInstruction::JumpIfNotZero { condition, .. } => {
                collect_val(condition, &mut vars);
            }

            _ => {}
        }
    }

    vars
}
