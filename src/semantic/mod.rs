mod identifier_resolution;
mod typecheck;
use crate::ast::untyped_ast::Program;

use crate::semantic::{identifier_resolution::identifier_resolution_pass, typecheck::typecheck};

pub fn semantic_analysis(program: Program) -> Program {
    let program1 = identifier_resolution_pass(program);
    let program2 = typecheck(program1);
    program2
}
