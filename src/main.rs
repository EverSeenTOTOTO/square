#![allow(
    clippy::upper_case_acronyms,
    clippy::enum_variant_names,
    clippy::disallowed_names
)]
#![cfg_attr(target_family = "wasm", no_std)]
#![cfg_attr(target_family = "wasm", no_main)]
// 单线程 wasm：全局可变状态用 `static mut` 表达，引用它本无并发风险，但编译器会对
// 「拿 `static mut` 的引用」报警（`static_mut_refs`）。整库放行（与 wasm-demo 一致）。
#![cfg_attr(target_family = "wasm", allow(static_mut_refs))]
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
#[cfg(target_family = "wasm")]
mod ffi;
#[cfg(target_family = "wasm")]
mod runtime;

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

/// 把一段字节拷进新分配的线性内存，返回 packed 句柄 `(ptr << 32) | len`。
/// 供 [`snapshot`] / [`snapshot_insts`] 把客机状态传给宿主——宿主 `>>> 32` / `& 0xffffffff`
/// 切出指针与长度读完后 `dealloc`。wasm32 下 ptr、len 各 32 位，packed u64 远小于 JS
/// `Number` 精确整数界 2^53，无精度损失。
#[cfg(target_family = "wasm")]
fn pack_buffer(bytes: &[u8]) -> u64 {
    let ptr = alloc(bytes.len());
    unsafe { core::ptr::copy_nonoverlapping(bytes.as_ptr(), ptr, bytes.len()) };
    ((ptr as u64) << 32) | (bytes.len() as u64)
}

#[cfg(target_family = "wasm")]
#[no_mangle]
pub extern "C" fn compile(source_addr: *mut u8, source_length: usize) -> *mut Vec<vm_insts::Inst> {
    let code = externs::ext::read(source_addr as usize, source_length);

    let ast = match parse::parse(code, &mut code_frame::Position::new()) {
        Err(e) => {
            // 编译错误走输出通道返回空句柄，宿主可继续使用实例（不再 panic 毒化）
            println!("{}", e);
            return core::ptr::null_mut();
        }
        Ok(node) => node,
    };

    let (insts, source_map) = match emit::emit(code, &ast, &mut RefCell::new(emit::EmitContext::new())) {
        Err(e) => {
            println!("{}", e);
            return core::ptr::null_mut();
        }
        Ok(pair) => pair,
    };
    runtime::set_source_map(source_map);

    Box::into_raw(Box::new(insts))
}



#[cfg(target_family = "wasm")]
#[no_mangle]
pub extern "C" fn snapshot_insts(insts_addr: *const u8) -> u64 {
    let insts = unsafe { Box::from_raw(insts_addr as *mut Vec<vm_insts::Inst>) };

    let joined = insts
        .iter()
        .map(|inst| alloc::format!("{}", inst))
        .collect::<alloc::vec::Vec<_>>()
        .join("\n");

    Box::into_raw(insts);

    pack_buffer(joined.as_bytes())
}

#[cfg(target_family = "wasm")]
#[no_mangle]
pub extern "C" fn init() -> *mut vm::VM {
    Box::into_raw(Box::new(vm::VM::new()))
}

#[cfg(target_family = "wasm")]
#[no_mangle]
pub extern "C" fn step(vm_addr: *mut u8, insts_addrr: *const u8) -> u32 {
    let mut vm = unsafe { Box::from_raw(vm_addr as *mut vm::VM) };
    let insts = unsafe { Box::from_raw(insts_addrr as *mut Vec<vm_insts::Inst>) };

    runtime::ensure_started(vm_addr as *mut vm::VM, insts_addrr as *const Vec<vm_insts::Inst>);
    if let Err(e) = runtime::step_one(&mut vm, &insts) {
        println!("{}", runtime::format_error(&e));
        Box::into_raw(vm);
        Box::into_raw(insts);
        return 1;
    }

    Box::into_raw(vm);
    Box::into_raw(insts);
    0
}

#[cfg(target_family = "wasm")]
#[no_mangle]
pub extern "C" fn run(vm_addr: *mut u8, insts_addrr: *const u8) -> u32 {
    // 运行错误经输出通道打印后返回 1；实例存活，宿主可 reset 后继续
    match runtime::start(vm_addr as *mut vm::VM, insts_addrr as *const Vec<vm_insts::Inst>) {
        Err(e) => {
            println!("{}", runtime::format_error(&e));
            1
        }
        Ok(()) => 0,
    }
}

#[cfg(target_family = "wasm")]
#[no_mangle]
pub extern "C" fn reset(vm_addr: *mut u8) {
    let mut vm = unsafe { Box::from_raw(vm_addr as *mut vm::VM) };

    vm.reset();
    runtime::reset();

    Box::into_raw(vm);
}

#[cfg(target_family = "wasm")]
#[no_mangle]
pub extern "C" fn snapshot(vm_addr: *mut u8) -> u64 {
    let vm = unsafe { Box::from_raw(vm_addr as *mut vm::VM) };

    let bytes = vm.snapshot();

    Box::into_raw(vm);

    pack_buffer(&bytes)
}

/// 原生 CLI 运行器：`square [-p|--profile] file.sq`。
/// -p 输出逐指令 rdtsc 周期剖析（wasm 目标无 main，走 host 驱动）。
#[cfg(not(target_family = "wasm"))]
pub fn main() {
    use std::cell::RefCell;

    let args: Vec<String> = std::env::args().collect();
    let profile = args.iter().any(|a| a == "-p" || a == "--profile");
    let Some(path) = args.iter().skip(1).find(|a| !a.starts_with('-')) else {
        eprintln!("usage: square [-p|--profile] file.sq");
        std::process::exit(2);
    };
    let code = std::fs::read_to_string(path).unwrap_or_else(|e| {
        eprintln!("read {}: {}", path, e);
        std::process::exit(2);
    });

    let ast = match parse::parse(&code, &mut code_frame::Position::new()) {
        Ok(ast) => ast,
        Err(e) => {
            eprintln!("{}", e);
            std::process::exit(1);
        }
    };
    let (insts, _sm) = match emit::emit(&code, &ast, &mut RefCell::new(emit::EmitContext::new()))
    {
        Ok(pair) => pair,
        Err(e) => {
            eprintln!("{}", e);
            std::process::exit(1);
        }
    };

    let mut vm = vm::VM::new();
    vm.profiling = profile;
    let task = alloc::rc::Rc::new(vm::Task {
        frame: RefCell::new(None),
    });
    let mut cx = vm::RtCx::new(task);
    if let Err(e) = vm.run(&insts, &mut cx) {
        eprintln!("{}", e);
        std::process::exit(1);
    }
    if profile {
        vm.print_times();
    }
}

#[cfg(target_family = "wasm")]
pub fn main() {}
