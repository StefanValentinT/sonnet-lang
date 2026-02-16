use crate::{
    ast::untyped_ast::Program,
    tac::{ast::TacProgram, gen_tac::gen_tac, rename_tac::rename_tac_program},
};

pub mod ast;
mod gen_tac;
mod rename_tac;

pub fn tac_pass(program: Program) -> TacProgram {
    rename_tac_program(gen_tac(program))
}
