#![cfg_attr(not(test), no_std)]
#![cfg_attr(not(test), no_main)]

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
mod errors;
mod parse;
mod scan;
mod vm_value;
mod vm;

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

    return Box::into_raw(Box::new(vm::VM::default()));
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
pub extern "C" fn parse_and_run(vmAddr: *const u8, sourceAddr: *mut u8, sourceLength: usize) {
    let vm = unsafe { Box::from_raw(vmAddr as *mut vm::VM) };

    println!("{}", core::mem::size_of::<vm::VM>());

    let code = memory::read(sourceAddr as usize, sourceLength);

    let ast = parse::parse(code, &mut code_frame::Position::default()).expect("failed to parse");

    for node in ast {
        println!("{}", node);
    }
}
