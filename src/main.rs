#![cfg_attr(not(test), no_std)]
#![cfg_attr(not(test), no_main)]

#[cfg(not(test))]
use crate::externs::{app, memory, wasm};

extern crate alloc;

#[cfg(not(test))]
mod allocator;
#[cfg(not(test))]
mod externs;

mod code_frame;
mod errors;
mod parse;
mod scan;

#[cfg(not(test))]
#[panic_handler]
fn panic(panic: &core::panic::PanicInfo<'_>) -> ! {
    println!("panic: {}", panic);
    core::arch::wasm32::unreachable()
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
pub extern "C" fn execute(ptr: *mut u8, size: usize) {
    let code = memory::read(ptr as usize, size);

    let ast = parse::parse(code, &mut code_frame::Position::default()).expect("failed to parse");

    for node in ast {
        println!("{}", node);
    }

    dealloc(ptr, size);
}

#[cfg(not(test))]
#[no_mangle]
pub fn main() -> i32 {
    #[cfg(not(test))]
    unsafe {
        crate::allocator::ALLOCATOR
            .lock()
            .add_to_heap(wasm::get_heap_base(), wasm::get_stack_base())
    };

    unsafe {
        app::execute();
    }

    return 0;
}
