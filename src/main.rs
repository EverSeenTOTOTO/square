#![no_std]
#![no_main]

use alloc::boxed::Box;

use crate::allocator::ALLOCATOR;

extern crate alloc;

mod allocator;
mod externs;

#[panic_handler]
fn panic(panic: &core::panic::PanicInfo<'_>) -> ! {
    println!("{}", panic);
    core::arch::wasm32::unreachable()
}

#[no_mangle]
pub fn main() -> i32 {
    unsafe { ALLOCATOR.lock().init() };

    let x = Box::new(42);

    println!("{}", x);

    return 0;
}
