#![cfg_attr(not(test), no_std)]
#![cfg_attr(not(test), no_main)]

#[cfg(not(test))]
use crate::allocator::ALLOCATOR;
use alloc::boxed::Box;

extern crate alloc;

#[cfg(not(test))]
mod allocator;

mod code_frame;
mod errors;
#[cfg(not(test))]
mod externs;
mod scan;

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
        ALLOCATOR.lock().init()
    };

    let val = Box::new(42);
    println!("{}", val);

    return 0;
}
