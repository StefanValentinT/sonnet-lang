use crate::gen_names::{new_name, reset_name_gen};
use crate::stdlib::{is_stdlib_fun, strip_non_std_away};
use crate::tac::ast::*;
use std::collections::HashMap;

pub fn rename_tac_program(program: TacProgram) -> TacProgram {
    let TacProgram::Program(funcs) = program;
    reset_name_gen();

    let mut function_map: HashMap<String, String> = HashMap::new();

    for func in &funcs {
        let TacFuncDef::Function { name, .. } = func;
        if name == "main" {
            function_map.insert(name.clone(), name.clone());
        } else if is_stdlib_fun(name) {
            let clean_name = strip_non_std_away(name).unwrap_or_else(|| name.clone());
            function_map.insert(name.clone(), clean_name);
        } else {
            function_map.insert(name.clone(), new_name());
        }
    }

    let renamed_funcs = funcs
        .into_iter()
        .map(|f| rename_func(f, &function_map))
        .collect();
    TacProgram::Program(renamed_funcs)
}

fn rename_func(func: TacFuncDef, function_map: &HashMap<String, String>) -> TacFuncDef {
    let TacFuncDef::Function {
        name,
        params,
        ret_type,
        body,
    } = func;
    let mut var_map: HashMap<String, String> = HashMap::new();
    let mut label_map: HashMap<String, String> = HashMap::new();

    let new_params = params
        .into_iter()
        .map(|(old_name, ty)| {
            let n = new_name();
            var_map.insert(old_name, n.clone());
            (n, ty)
        })
        .collect();

    for instr in &body {
        if let TacInstruction::Label(old_label) = instr {
            label_map.entry(old_label.clone()).or_insert_with(new_name);
        }
    }

    let new_body = body
        .into_iter()
        .map(|instr| rename_instruction(instr, &mut var_map, &label_map, function_map))
        .collect();

    TacFuncDef::Function {
        name: function_map.get(&name).cloned().unwrap_or(name),
        params: new_params,
        ret_type,
        body: new_body,
    }
}

fn rename_val(val: TacVal, var_map: &mut HashMap<String, String>) -> TacVal {
    match val {
        TacVal::Constant(c) => TacVal::Constant(c),
        TacVal::Var(name, ty) => {
            let n = var_map.entry(name).or_insert_with(new_name).clone();
            TacVal::Var(n, ty)
        }
    }
}

fn rename_instruction(
    instr: TacInstruction,
    var_map: &mut HashMap<String, String>,
    label_map: &HashMap<String, String>,
    function_map: &HashMap<String, String>,
) -> TacInstruction {
    match instr {
        TacInstruction::Return(val) => TacInstruction::Return(val.map(|v| rename_val(v, var_map))),
        TacInstruction::Copy { src, dest } => TacInstruction::Copy {
            src: rename_val(src, var_map),
            dest: rename_val(dest, var_map),
        },
        TacInstruction::Unary { op, src, dest } => TacInstruction::Unary {
            op,
            src: rename_val(src, var_map),
            dest: rename_val(dest, var_map),
        },
        TacInstruction::Binary {
            op,
            src1,
            src2,
            dest,
        } => TacInstruction::Binary {
            op,
            src1: rename_val(src1, var_map),
            src2: rename_val(src2, var_map),
            dest: rename_val(dest, var_map),
        },
        TacInstruction::Jump { target } => TacInstruction::Jump {
            target: label_map.get(&target).cloned().unwrap_or(target),
        },
        TacInstruction::JumpIfZero { condition, target } => TacInstruction::JumpIfZero {
            condition: rename_val(condition, var_map),
            target: label_map.get(&target).cloned().unwrap_or(target),
        },
        TacInstruction::JumpIfNotZero { condition, target } => TacInstruction::JumpIfNotZero {
            condition: rename_val(condition, var_map),
            target: label_map.get(&target).cloned().unwrap_or(target),
        },
        TacInstruction::Label(n) => TacInstruction::Label(label_map.get(&n).cloned().unwrap_or(n)),
        TacInstruction::FunCall {
            fun_name,
            args,
            dest,
        } => TacInstruction::FunCall {
            fun_name: function_map.get(&fun_name).cloned().unwrap_or(fun_name),
            args: args.into_iter().map(|a| rename_val(a, var_map)).collect(),
            dest: rename_val(dest, var_map),
        },
        TacInstruction::Load { src_ptr, dest } => TacInstruction::Load {
            src_ptr: rename_val(src_ptr, var_map),
            dest: rename_val(dest, var_map),
        },
        TacInstruction::Store { src, dest_ptr } => TacInstruction::Store {
            src: rename_val(src, var_map),
            dest_ptr: rename_val(dest_ptr, var_map),
        },
        TacInstruction::GetAddress { src, dest } => TacInstruction::GetAddress {
            src: rename_val(src, var_map),
            dest: rename_val(dest, var_map),
        },
        TacInstruction::AddPtr {
            ptr,
            index,
            scale,
            dest,
        } => TacInstruction::AddPtr {
            ptr: rename_val(ptr, var_map),
            index: rename_val(index, var_map),
            scale,
            dest: rename_val(dest, var_map),
        },
        TacInstruction::CopyToOffset { src, dest, offset } => TacInstruction::CopyToOffset {
            src: rename_val(src, var_map),
            dest: rename_val(dest, var_map),
            offset,
        },
        TacInstruction::Truncate { src, dest } => TacInstruction::Truncate {
            src: rename_val(src, var_map),
            dest: rename_val(dest, var_map),
        },
        TacInstruction::SignExtend { src, dest } => TacInstruction::SignExtend {
            src: rename_val(src, var_map),
            dest: rename_val(dest, var_map),
        },
        TacInstruction::F64ToI32 { src, dest } => TacInstruction::F64ToI32 {
            src: rename_val(src, var_map),
            dest: rename_val(dest, var_map),
        },
        TacInstruction::F64ToI64 { src, dest } => TacInstruction::F64ToI64 {
            src: rename_val(src, var_map),
            dest: rename_val(dest, var_map),
        },
        TacInstruction::I32ToF64 { src, dest } => TacInstruction::I32ToF64 {
            src: rename_val(src, var_map),
            dest: rename_val(dest, var_map),
        },
        TacInstruction::I64ToF64 { src, dest } => TacInstruction::I64ToF64 {
            src: rename_val(src, var_map),
            dest: rename_val(dest, var_map),
        },
    }
}
