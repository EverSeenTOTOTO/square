#![allow(
    clippy::upper_case_acronyms,
    clippy::enum_variant_names,
    clippy::disallowed_names
)]
#![cfg_attr(target_family = "wasm", no_std)]
#![cfg_attr(target_family = "wasm", no_main)]
#![feature(if_let_guard)]

#[cfg(target_family = "wasm")]
use alloc::boxed::Box;
#[cfg(target_family = "wasm")]
use alloc::vec::Vec;
#[cfg(target_family = "wasm")]
use core::cell::RefCell;

extern crate alloc;

#[cfg(target_family = "wasm")]
mod allocator;
#[cfg(target_family = "wasm")]
mod externs;

mod builtin;
mod code_frame;
mod emit;
mod errors;
mod parse;
mod scan;
mod vm;
mod vm_insts;
mod vm_value;

#[cfg(target_family = "wasm")]
#[panic_handler]
fn panic(panic: &core::panic::PanicInfo<'_>) -> ! {
    println!("panic: {}", panic);
    core::arch::wasm32::unreachable()
}

#[cfg(target_family = "wasm")]
#[no_mangle]
pub extern "C" fn alloc(size: usize) -> *mut u8 {
    let layout = alloc::alloc::Layout::from_size_align(size, 1).unwrap();
    unsafe { alloc::alloc::alloc(layout) }
}

#[cfg(target_family = "wasm")]
#[no_mangle]
pub extern "C" fn dealloc(ptr: *mut u8, size: usize) {
    let layout = alloc::alloc::Layout::from_size_align(size, 1).unwrap();
    unsafe { alloc::alloc::dealloc(ptr, layout) }
}

#[cfg(target_family = "wasm")]
#[no_mangle]
pub unsafe extern "C" fn realloc(ptr: *mut u8, old_size: usize, new_size: usize) -> *mut u8 {
    alloc::alloc::realloc(
        ptr,
        alloc::alloc::Layout::from_size_align_unchecked(old_size, 8),
        new_size,
    )
}

#[cfg(target_family = "wasm")]
#[no_mangle]
pub extern "C" fn compile(source_addr: *mut u8, source_length: usize) -> *mut Vec<vm_insts::Inst> {
    let code = externs::ext::read(source_addr as usize, source_length);

    let ast = match parse::parse(code, &mut code_frame::Position::new()) {
        Err(e) => {
            panic!("{}", e);
        }
        Ok(node) => node,
    };

    let insts = match emit::emit(code, &ast, &mut RefCell::new(emit::EmitContext::new())) {
        Err(e) => {
            panic!("{}", e);
        }
        Ok(inst) => inst,
    };

    Box::into_raw(Box::new(insts))
}

#[cfg(target_family = "wasm")]
#[no_mangle]
pub extern "C" fn dump_instructions(insts_addr: *const u8) {
    let insts = unsafe { Box::from_raw(insts_addr as *mut Vec<vm_insts::Inst>) };

    insts.iter().for_each(|inst| {
        println!("{}", inst);
    });

    Box::into_raw(insts);
}

#[cfg(target_family = "wasm")]
#[no_mangle]
pub extern "C" fn init() -> *mut vm::VM {
    Box::into_raw(Box::new(vm::VM::new()))
}

#[cfg(target_family = "wasm")]
#[no_mangle]
pub extern "C" fn step(vm_addr: *mut u8, insts_addrr: *const u8) {
    let mut vm = unsafe { Box::from_raw(vm_addr as *mut vm::VM) };
    let insts = unsafe { Box::from_raw(insts_addrr as *mut Vec<vm_insts::Inst>) };

    match vm.step(&insts) {
        Err(e) => {
            panic!("{}", e);
        }
        Ok(_) => {}
    }

    Box::into_raw(vm);
    Box::into_raw(insts);
}

#[cfg(target_family = "wasm")]
#[no_mangle]
pub extern "C" fn run(vm_addr: *mut u8, insts_addrr: *const u8) {
    let mut vm = unsafe { Box::from_raw(vm_addr as *mut vm::VM) };
    let insts = unsafe { Box::from_raw(insts_addrr as *mut Vec<vm_insts::Inst>) };

    match vm.run(&insts) {
        Err(e) => {
            panic!("{}", e);
        }
        Ok(_) => {}
    }

    Box::into_raw(vm);
    Box::into_raw(insts);
}

#[cfg(target_family = "wasm")]
#[no_mangle]
pub extern "C" fn reset(vm_addr: *mut u8) {
    let mut vm = unsafe { Box::from_raw(vm_addr as *mut vm::VM) };

    vm.reset();

    Box::into_raw(vm);
}

#[cfg(target_family = "wasm")]
#[no_mangle]
pub extern "C" fn dump_pc(vm_addr: *mut u8) -> usize {
    let vm = unsafe { Box::from_raw(vm_addr as *mut vm::VM) };

    let pc = vm.pc;

    Box::into_raw(vm);

    pc
}

#[cfg(target_family = "wasm")]
#[no_mangle]
pub extern "C" fn dump_callframes(vm_addr: *mut u8) {
    let vm = unsafe { Box::from_raw(vm_addr as *mut vm::VM) };

    vm.call_frames.iter().for_each(|frame| {
        println!("{}", frame.borrow());
    });

    Box::into_raw(vm);
}

pub fn main() {}
