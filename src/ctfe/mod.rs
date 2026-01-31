use std::collections::HashSet;

use crate::{
    ast::{
        ast_type::{Const, ExecTime, Type},
        typed_ast::{
            TypedBlock, TypedBlockItem, TypedDecl, TypedExpr, TypedExprKind, TypedFunDecl,
            TypedProgram, TypedStmt, TypedVarDecl,
        },
    },
    llvm_codegen::emit_llvm,
    stdlib::is_stdlib_fun,
    tac::gen_tac,
};

use std::{
    fs,
    process::{Command, Stdio},
    time::{SystemTime, UNIX_EPOCH},
};

fn execute_llvm_ir(ir: &str) -> Result<String, String> {
    let id = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_nanos();

    let ll_path = format!("/tmp/ctfe_{}.ll", id);
    let exe_path = format!("/tmp/ctfe_{}", id);

    fs::write(&ll_path, ir).map_err(|e| e.to_string())?;

    let compile = Command::new("cc")
        .arg(&ll_path)
        .arg("-O0")
        .arg("-o")
        .arg(&exe_path)
        .stderr(Stdio::piped())
        .output()
        .map_err(|e| e.to_string())?;

    if !compile.status.success() {
        return Err(format!(
            "Compilation failed:\n{}",
            String::from_utf8_lossy(&compile.stderr)
        ));
    }

    let run = Command::new(&exe_path)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .map_err(|e| e.to_string())?;

    fs::remove_file(&ll_path).ok();
    fs::remove_file(&exe_path).ok();

    if !run.status.success() {
        return Err(format!(
            "Execution failed:\nstdout: {}\nstderr: {}",
            String::from_utf8_lossy(&run.stdout),
            String::from_utf8_lossy(&run.stderr)
        ));
    }

    let stderr = String::from_utf8_lossy(&run.stderr);
    if !stderr.is_empty() {
        eprint!("{}", stderr);
    }

    Ok(String::from_utf8_lossy(&run.stdout).to_string())
}
pub fn perform_ctfe_pass(mut program: TypedProgram) -> Result<TypedProgram, String> {
    for fun in &program.functions.clone() {
        if is_compile_time_function(fun) {
            let mut functions = Vec::new();
            let mut visited = HashSet::new();
            functions.push(fun.clone());
            functions.extend(lift_called(fun, &program, &mut visited));

            let ctfe_main = make_ctfe_main(fun, &fun.ret_type)?;
            functions.push(ctfe_main);
            let sub_program = TypedProgram { functions };

            println!("Constant Program: {:#?}", sub_program);
            let tac_program = gen_tac(sub_program);

            println!("Constant Tac: {:#?}", tac_program);
            let mut llvm_program = emit_llvm(tac_program);
            llvm_program.push_str(&generate_serialization_helpers());
            println!("Constant LLVM: {}", llvm_program);
            let output = execute_llvm_ir(&llvm_program)?;

            println!("CTFE output: {}", output);

            let result = parse_ctfe_result(&output, &fun.ret_type)?;

            println!("CTFE result: {:#?}", result);

            program.functions = program
                .functions
                .into_iter()
                .map(|f| {
                    if f.name == fun.name {
                        TypedFunDecl {
                            name: f.name,
                            params: vec![],
                            ret_type: f.ret_type.clone(),
                            body: Some(TypedBlock::Block(vec![TypedBlockItem::S(
                                TypedStmt::Return(result.clone()),
                            )])),
                            exec_time: ExecTime::Runtime,
                        }
                    } else {
                        f
                    }
                })
                .collect();
        }
    }

    Ok(program)
}

fn lift_called(
    caller: &TypedFunDecl,
    program: &TypedProgram,
    visited: &mut HashSet<String>,
) -> Vec<TypedFunDecl> {
    if visited.contains(&caller.name) {
        return Vec::new();
    }
    visited.insert(caller.name.clone());

    let Some(body) = &caller.body else {
        return Vec::new();
    };

    let mut called = Vec::new();

    if let TypedBlock::Block(items) = body {
        for item in items {
            if let TypedBlockItem::S(stmt) = item {
                collect_calls_stmt(stmt, &mut called);
            }
        }
    }

    let mut lifted = Vec::new();

    for name in called {
        if let Some(fun) = program
            .functions
            .iter()
            .find(|f| f.name == name && !is_stdlib_fun(f.clone().clone()))
        {
            lifted.push(fun.clone());
            lifted.extend(lift_called(fun, program, visited));
        }
    }

    lifted
}

fn collect_calls_stmt(stmt: &TypedStmt, out: &mut Vec<String>) {
    match stmt {
        TypedStmt::Expr(expr) | TypedStmt::Return(expr) => {
            collect_calls_expr(expr, out);
        }
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
        TypedExprKind::Unary { expr, .. } => {
            collect_calls_expr(expr, out);
        }
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
        TypedExprKind::Dereference(expr)
        | TypedExprKind::AddrOf(expr)
        | TypedExprKind::SliceFromArray(expr)
        | TypedExprKind::SliceLen(expr)
        | TypedExprKind::Cast { expr, .. } => {
            collect_calls_expr(expr, out);
        }
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
fn generate_serialization_helpers() -> String {
    r#"
@_ctfe_heap = global [4096 x i8] zeroinitializer
@_ctfe_heap_ptr = global i32 0
@_ctfe_array_buffer = global [1024 x i32] zeroinitializer

declare i32 @write(i32, i8*, i32)
define void @_ctfe_write_i8(i8 %v) {
entry:
  %idx = load i32, i32* @_ctfe_heap_ptr
  %ptr = getelementptr [4096 x i8], [4096 x i8]* @_ctfe_heap, i32 0, i32 %idx
  store i8 %v, i8* %ptr
  %next = add i32 %idx, 1
  store i32 %next, i32* @_ctfe_heap_ptr
  ret void
}

define void @_ctfe_write_i32(i32 %v) {
entry:
  %b0 = trunc i32 %v to i8
  %shift1 = lshr i32 %v, 8
  %b1 = trunc i32 %shift1 to i8
  %shift2 = lshr i32 %v, 16
  %b2 = trunc i32 %shift2 to i8
  %shift3 = lshr i32 %v, 24
  %b3 = trunc i32 %shift3 to i8
  call void @_ctfe_write_i8(i8 %b0)
  call void @_ctfe_write_i8(i8 %b1)
  call void @_ctfe_write_i8(i8 %b2)
  call void @_ctfe_write_i8(i8 %b3)
  ret void
}

define void @_ctfe_write_i64(i64 %v) {
entry:
  %b0 = trunc i64 %v to i8
  %shift1 = lshr i64 %v, 8
  %b1 = trunc i64 %shift1 to i8
  %shift2 = lshr i64 %v, 16
  %b2 = trunc i64 %shift2 to i8
  %shift3 = lshr i64 %v, 24
  %b3 = trunc i64 %shift3 to i8
  %shift4 = lshr i64 %v, 32
  %b4 = trunc i64 %shift4 to i8
  %shift5 = lshr i64 %v, 40
  %b5 = trunc i64 %shift5 to i8
  %shift6 = lshr i64 %v, 48
  %b6 = trunc i64 %shift6 to i8
  %shift7 = lshr i64 %v, 56
  %b7 = trunc i64 %shift7 to i8
  call void @_ctfe_write_i8(i8 %b0)
  call void @_ctfe_write_i8(i8 %b1)
  call void @_ctfe_write_i8(i8 %b2)
  call void @_ctfe_write_i8(i8 %b3)
  call void @_ctfe_write_i8(i8 %b4)
  call void @_ctfe_write_i8(i8 %b5)
  call void @_ctfe_write_i8(i8 %b6)
  call void @_ctfe_write_i8(i8 %b7)
  ret void
}

define void @_ctfe_serialize_i32(i32 %v) {
entry:
  call void @_ctfe_write_i8(i8 1)  ; TAG for I32
  call void @_ctfe_write_i32(i32 %v)
  ret void
}

define void @_ctfe_serialize_i64(i64 %v) {
entry:
  call void @_ctfe_write_i8(i8 2)
  call void @_ctfe_write_i64(i64 %v)
  ret void
}

define void @_ctfe_serialize_f64(double %v) {
entry:
  call void @_ctfe_write_i8(i8 3)
  %i = bitcast double %v to i64
  call void @_ctfe_write_i64(i64 %i)
  ret void
}

define void @_ctfe_serialize_char(i32 %v) {
entry:
  call void @_ctfe_write_i8(i8 4)  ; TAG for Char
  call void @_ctfe_write_i32(i32 %v)
  ret void
}

define void @_ctfe_flush_heap() {
entry:
  %len = load i32, i32* @_ctfe_heap_ptr
  %ptr = getelementptr [4096 x i8], [4096 x i8]* @_ctfe_heap, i32 0, i32 0
  %result = call i32 @write(i32 1, i8* %ptr, i32 %len)
  ret void
}

define void @_ctfe_serialize_array(i8 %elem_tag, i32* %ptr, i32 %len) {
entry:
  %buffer_ptr = getelementptr [1024 x i32], [1024 x i32]* @_ctfe_array_buffer, i32 0, i32 0
  
  %copy_i = alloca i32
  store i32 0, i32* %copy_i
  br label %copy_loop

copy_loop:
  %copy_idx = load i32, i32* %copy_i
  %copy_cond = icmp slt i32 %copy_idx, %len
  br i1 %copy_cond, label %copy_body, label %copy_done

copy_body:
  %src_ptr = getelementptr i32, i32* %ptr, i32 %copy_idx
  %src_val = load i32, i32* %src_ptr
  %dst_ptr = getelementptr i32, i32* %buffer_ptr, i32 %copy_idx
  store i32 %src_val, i32* %dst_ptr
  %copy_next = add i32 %copy_idx, 1
  store i32 %copy_next, i32* %copy_i
  br label %copy_loop

copy_done:
  call void @_ctfe_write_i8(i8 10)
  call void @_ctfe_write_i8(i8 %elem_tag)
  call void @_ctfe_write_i32(i32 %len)

  %i = alloca i32
  store i32 0, i32* %i
  br label %loop

loop:
  %idx = load i32, i32* %i
  %cond = icmp slt i32 %idx, %len
  br i1 %cond, label %body, label %done

body:
  %ptr_elem = getelementptr i32, i32* %buffer_ptr, i32 %idx
  %val = load i32, i32* %ptr_elem
  call void @_ctfe_write_i32(i32 %val)
  
  %next_idx = add i32 %idx, 1
  store i32 %next_idx, i32* %i
  br label %loop

done:
  ret void
}

define void @_ctfe_serialize_slice(i8 %elem_tag, { i32*, i32 } %slice) {
entry:
  %ptr = extractvalue { i32*, i32 } %slice, 0
  %len = extractvalue { i32*, i32 } %slice, 1
  call void @_ctfe_serialize_array(i8 %elem_tag, i32* %ptr, i32 %len)
  ret void
}
"#
    .to_string()
}

pub fn decode_ctfe_heap(mut bytes: &[u8]) -> Result<TypedExpr, String> {
    fn read_u8(bytes: &mut &[u8]) -> Result<u8, String> {
        if bytes.is_empty() {
            return Err("Unexpected EOF".to_string());
        }
        let b = bytes[0];
        *bytes = &bytes[1..];
        Ok(b)
    }

    fn read_i32(bytes: &mut &[u8]) -> Result<i32, String> {
        if bytes.len() < 4 {
            return Err("Unexpected EOF".to_string());
        }
        let val = i32::from_le_bytes(bytes[0..4].try_into().unwrap());
        *bytes = &bytes[4..];
        Ok(val)
    }

    fn read_i64(bytes: &mut &[u8]) -> Result<i64, String> {
        if bytes.len() < 8 {
            return Err("Unexpected EOF".to_string());
        }
        let val = i64::from_le_bytes(bytes[0..8].try_into().unwrap());
        *bytes = &bytes[8..];
        Ok(val)
    }

    fn decode_value(bytes: &mut &[u8]) -> Result<TypedExpr, String> {
        let tag = read_u8(bytes)?;
        match tag {
            1 => {
                // I32
                Ok(TypedExpr {
                    ty: Type::I32,
                    kind: TypedExprKind::Constant(Const::I32(read_i32(bytes)?)),
                })
            }
            2 => {
                // I64
                Ok(TypedExpr {
                    ty: Type::I64,
                    kind: TypedExprKind::Constant(Const::I64(read_i64(bytes)?)),
                })
            }
            3 => {
                // F64
                let bits = read_i64(bytes)?;
                let float = f64::from_bits(bits as u64);
                Ok(TypedExpr {
                    ty: Type::F64,
                    kind: TypedExprKind::Constant(Const::F64(float)),
                })
            }
            4 => {
                // Char
                Ok(TypedExpr {
                    ty: Type::Char,
                    kind: TypedExprKind::Constant(Const::Char(
                        std::char::from_u32(read_i32(bytes)? as u32).ok_or("Invalid char code")?,
                    )),
                })
            }
            10 => {
                // Array
                let elem_tag = read_u8(bytes)?;
                let len = read_i32(bytes)?;
                let mut elems = Vec::new();

                for _ in 0..len {
                    let val = read_i32(bytes)?;
                    let elem = match elem_tag {
                        1 => TypedExpr {
                            ty: Type::I32,
                            kind: TypedExprKind::Constant(Const::I32(val)),
                        },
                        4 => TypedExpr {
                            ty: Type::Char,
                            kind: TypedExprKind::Constant(Const::Char(
                                std::char::from_u32(val as u32)
                                    .ok_or_else(|| format!("Invalid char code: {}", val))?,
                            )),
                        },
                        _ => return Err(format!("Unsupported element tag: {}", elem_tag)),
                    };
                    elems.push(elem);
                }

                Ok(TypedExpr {
                    ty: Type::Array {
                        element_type: Box::new(if elem_tag == 1 { Type::I32 } else { Type::Char }),
                        size: len,
                    },
                    kind: TypedExprKind::ArrayLiteral(elems),
                })
            }
            _ => Err(format!("Unknown CTFE tag: {}", tag)),
        }
    }

    decode_value(&mut bytes)
}

fn make_ctfe_main(callee: &TypedFunDecl, ret_type: &Type) -> Result<TypedFunDecl, String> {
    let call_expr = TypedExpr {
        ty: ret_type.clone(),
        kind: TypedExprKind::FunctionCall {
            name: callee.name.clone(),
            args: vec![],
        },
    };

    let (serialize_call, needs_var) = match ret_type {
        Type::I32 => (
            TypedExpr {
                ty: Type::Unit,
                kind: TypedExprKind::FunctionCall {
                    name: "_ctfe_serialize_i32".to_string(),
                    args: vec![call_expr],
                },
            },
            false,
        ),
        Type::I64 => (
            TypedExpr {
                ty: Type::Unit,
                kind: TypedExprKind::FunctionCall {
                    name: "_ctfe_serialize_i64".to_string(),
                    args: vec![call_expr],
                },
            },
            false,
        ),
        Type::F64 => (
            TypedExpr {
                ty: Type::Unit,
                kind: TypedExprKind::FunctionCall {
                    name: "_ctfe_serialize_f64".to_string(),
                    args: vec![call_expr],
                },
            },
            false,
        ),
        Type::Char => (
            TypedExpr {
                ty: Type::Unit,
                kind: TypedExprKind::FunctionCall {
                    name: "_ctfe_serialize_char".to_string(),
                    args: vec![call_expr],
                },
            },
            false,
        ),
        Type::Slice { element_type } => {
            return Ok(TypedFunDecl {
                name: "main".to_string(),
                params: vec![],
                ret_type: Type::I32,
                body: Some(TypedBlock::Block(vec![
                    TypedBlockItem::D(TypedDecl::Variable(TypedVarDecl {
                        name: "_ctfe_result".to_string(),
                        init_expr: call_expr,
                        var_type: ret_type.clone(),
                    })),
                    TypedBlockItem::S(TypedStmt::Expr(TypedExpr {
                        ty: Type::Unit,
                        kind: TypedExprKind::FunctionCall {
                            name: "_ctfe_serialize_slice".to_string(),
                            args: vec![
                                TypedExpr {
                                    ty: Type::I32,
                                    kind: TypedExprKind::Constant(Const::I32(
                                        match **element_type {
                                            Type::I32 => 1,
                                            Type::Char => 4,
                                            _ => {
                                                return Err(format!(
                                                    "Unsupported slice element type: {:?}",
                                                    element_type
                                                ));
                                            }
                                        },
                                    )),
                                },
                                TypedExpr {
                                    ty: ret_type.clone(),
                                    kind: TypedExprKind::Var("_ctfe_result".to_string()),
                                },
                            ],
                        },
                    })),
                    TypedBlockItem::S(TypedStmt::Expr(TypedExpr {
                        ty: Type::Unit,
                        kind: TypedExprKind::FunctionCall {
                            name: "_ctfe_flush_heap".to_string(),
                            args: vec![],
                        },
                    })),
                    TypedBlockItem::S(TypedStmt::Return(TypedExpr {
                        ty: Type::I32,
                        kind: TypedExprKind::Constant(Const::I32(0)),
                    })),
                ])),
                exec_time: ExecTime::Runtime,
            });
        }
        _ => return Err(format!("Unsupported CTFE return type: {:?}", ret_type)),
    };

    let flush_expr = TypedExpr {
        ty: Type::Unit,
        kind: TypedExprKind::FunctionCall {
            name: "_ctfe_flush_heap".to_string(),
            args: vec![],
        },
    };

    Ok(TypedFunDecl {
        name: "main".to_string(),
        params: vec![],
        ret_type: Type::I32,
        body: Some(TypedBlock::Block(vec![
            TypedBlockItem::S(TypedStmt::Expr(serialize_call)),
            TypedBlockItem::S(TypedStmt::Expr(flush_expr)),
            TypedBlockItem::S(TypedStmt::Return(TypedExpr {
                ty: Type::I32,
                kind: TypedExprKind::Constant(Const::I32(0)),
            })),
        ])),
        exec_time: ExecTime::Runtime,
    })
}

fn parse_ctfe_result(output: &str, ty: &Type) -> Result<TypedExpr, String> {
    let bytes = output.as_bytes();
    decode_ctfe_heap(bytes)
}
