#![cfg_attr(target_family = "wasm", no_std)]
#![cfg_attr(target_family = "wasm", no_main)]
#![feature(if_let_guard)]

#[cfg(target_family = "wasm")]
use crate::externs::memory;

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
pub extern "C" fn exec(source_addr: *mut u8, source_length: usize) {
    use code_frame::Position;
    use core::cell::RefCell;
    use emit::EmitContext;
    use vm::VM;

    // println!(
    //     "Before: {:?}",
    //     allocator::TALC.lock().get_counters().allocated_bytes
    // );

    let mut vm = VM::new();
    let code = memory::read(source_addr as usize, source_length);

    let ast = match parse::parse(code, &mut Position::new()) {
        Err(e) => {
            println!("{}", e);
            return;
        }
        Ok(node) => node,
    };

    let insts = match emit::emit(code, &ast, &mut RefCell::new(EmitContext::new())) {
        Err(e) => {
            println!("\n{}", e);
            return;
        }
        Ok(inst) => inst,
    };

    if let Err(e) = vm.run(&insts) {
        println!("{}", e);
    }

    // println!(
    //     "After: {:?}",
    //     allocator::TALC.lock().get_counters().allocated_bytes
    // );
}
