#![feature(asm_experimental_arch)]
#![cfg_attr(not(test), no_std)]
#![cfg_attr(not(test), no_main)]

use core::arch::asm;

use alloc::boxed::Box;

#[cfg(not(test))]
use crate::externs::{memory, wasm};

extern crate alloc;

#[cfg(not(test))]
mod allocator;
mod utils;

mod code_frame;
mod errors;
#[cfg(not(test))]
mod externs;
mod scan;
mod parse;

#[cfg(not(test))]
#[panic_handler]
fn panic(panic: &core::panic::PanicInfo<'_>) -> ! {
    println!("panic: {}", panic);
    core::arch::wasm32::unreachable()
}

#[cfg(not(test))]
#[no_mangle]
pub fn main() -> i32 {
    #[cfg(not(test))]
    unsafe {
        crate::allocator::ALLOCATOR.lock().init()
    };

    let string = memory::read(wasm::get_heap_base(), 12);

    println!("{}", string);

    let _ = Box::new(42);

    return 0;
}
