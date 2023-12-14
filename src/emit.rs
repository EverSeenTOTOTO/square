use crate::{
    code_frame::Position,
    errors::SquareError,
    parse::{parse, Node},
    vm::{Inst, VM},
    vm_value::Value,
};
use alloc::{boxed::Box, vec::Vec};

type EmitResult<'a> = Result<&'a Vec<Inst>, SquareError<'a>>;

fn emit<'a>(input: &'a str, ast: Vec<Box<Node>>) -> EmitResult<'a> {
    todo!()
}

fn run<'a>(input: &'a str) -> Result<(), SquareError<'a>> {
    let mut pos = Position::default();
    let ast = parse(input, &mut pos)?;
    let insts = emit(input, ast)?;
    let mut vm = VM::default();
    let mut pc = 0;

    vm.run(&insts, &mut pc);

    assert_eq!(vm.top(), Some(&Value::Int(3)));

    Ok(())
}
