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
    // 交给运行时：它会登记 vm/insts 句柄、把主程序续延 spawn 进就绪队列、跑一轮 tick。
    // 任务若 sleep，控制权交还事件循环；之后宿主的 setTimeout/queueMicrotask 回调
    // `wake_by_id` 重新进入 wasm 续跑。vm/insts 的所有权随后续留在裸指针里（跨调用栈存活），
    // 这里不能 `Box::from_raw` 后让其 drop——会释放掉运行时还要用的实例。
    runtime::start(vm_addr as *mut vm::VM, insts_addrr as *const Vec<vm_insts::Inst>);
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
pub extern "C" fn snapshot(vm_addr: *mut u8) -> u64 {
    let vm = unsafe { Box::from_raw(vm_addr as *mut vm::VM) };

    let bytes = vm.snapshot();

    Box::into_raw(vm);

    pack_buffer(&bytes)
}

pub fn main() {}
