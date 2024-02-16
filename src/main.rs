#![cfg_attr(not(test), no_std)]
#![cfg_attr(not(test), no_main)]
#![feature(if_let_guard)]

#[cfg(not(test))]
use alloc::boxed::Box;

#[cfg(not(test))]
use crate::externs::{memory, wasm};

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
mod vm_insts;
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
    use core::cell::RefCell;

    use code_frame::Position;
    use emit::EmitContext;

    let mut vm = unsafe { Box::from_raw(vm_addr as *mut vm::VM) };
    let code = memory::read(source_addr as usize, source_length);
    let ast = parse::parse(code, &mut Position::new()).unwrap();
    let insts = emit::emit(code, &ast, &mut RefCell::new(EmitContext::new())).unwrap();

    vm.run(&insts, &mut 0).unwrap();

    println!("{}", vm.current_frame());
}
