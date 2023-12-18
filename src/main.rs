#![cfg_attr(not(test), no_std)]
#![cfg_attr(not(test), no_main)]

#[cfg(not(test))]
use alloc::boxed::Box;

#[cfg(not(test))]
use crate::{
    code_frame::Position,
    externs::{memory, wasm},
};

extern crate alloc;

#[cfg(not(test))]
mod allocator;
#[cfg(not(test))]
mod externs;

mod code_frame;
mod emit;
mod errors;
mod parse;
mod scan;
mod vm;
mod vm_value;

#[cfg(not(test))]
#[panic_handler]
fn panic(panic: &core::panic::PanicInfo<'_>) -> ! {
    println!("panic: {}", panic);
    core::arch::wasm32::unreachable()
}

#[cfg(not(test))]
#[no_mangle]
pub fn init_vm() -> *mut vm::VM {
    #[cfg(not(test))]
    unsafe {
        crate::allocator::ALLOCATOR
            .lock()
            .add_to_heap(wasm::get_heap_base(), wasm::get_stack_base())
    };

    return Box::into_raw(Box::new(vm::VM::new()));
}

#[cfg(not(test))]
#[no_mangle]
pub extern "C" fn alloc(size: usize) -> *mut u8 {
    let layout = alloc::alloc::Layout::from_size_align(size, 1).unwrap();
    unsafe { alloc::alloc::alloc(layout) }
}

#[cfg(not(test))]
#[no_mangle]
pub extern "C" fn dealloc(ptr: *mut u8, size: usize) {
    let layout = alloc::alloc::Layout::from_size_align(size, 1).unwrap();
    unsafe { alloc::alloc::dealloc(ptr, layout) }
}

#[cfg(not(test))]
#[no_mangle]
pub extern "C" fn exec(vm_addr: *const u8, source_addr: *mut u8, source_length: usize) {
    let mut vm = unsafe { Box::from_raw(vm_addr as *mut vm::VM) };
    let code = memory::read(source_addr as usize, source_length);
    let mut pos = Position::default();
    let ast = parse::parse(code, &mut pos).expect("failed to parse");

    for node in &ast {
        println!("{}", node);
    }

    let insts = emit::emit(code, ast).expect("failed to emit");
    let mut pc = 0;

    for inst in &insts {
        println!("{}", inst);
    }

    vm.run(&insts, &mut pc);

    println!("{:?}", vm.call_frame.resolve_local("a"));
}
