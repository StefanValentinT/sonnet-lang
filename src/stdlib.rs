use crate::ast::ast_type::ExecTime;
use crate::ast::ast_type::Type;
use crate::ast::untyped_ast::*;
use std::collections::HashMap;

pub fn builtin_functions() -> HashMap<String, FunDecl> {
    let mut map = HashMap::new();

    map.insert(
        "print".to_string(),
        FunDecl {
            name: "print".to_string(),
            params: vec![(
                "s".to_string(),
                Type::Slice {
                    element_type: Box::new(Type::Char),
                },
            )],
            ret_type: Type::I32,
            body: None,
            exec_time: ExecTime::Runtime,
        },
    );

    map
}

pub fn is_stdlib_fun(fun: &FunDecl) -> bool {
    builtin_functions().contains_key(&fun.name)
        && (builtin_functions().get(&fun.name).expect("Checked.") == fun)
}
