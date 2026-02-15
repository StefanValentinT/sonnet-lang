mod identifier_resolution;
mod internal_addition;
mod typecheck;

use crate::ast::untyped_ast::Program;

use crate::semantic::internal_addition::internal_addition_pass;
use crate::semantic::{identifier_resolution::identifier_resolution_pass, typecheck::typecheck};

pub fn semantic_analysis(program: Program) -> Program {
    let added = internal_addition_pass(program);
    let program1 = identifier_resolution_pass(added);
    let program2 = typecheck(program1);
    program2
}
