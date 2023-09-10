use alloc::alloc::{GlobalAlloc, Layout};
use core::ptr;

use crate::externs::wasm::{get_heap_base, get_stack_base};
use crate::utils::Locked;

pub struct BumpAllocator {
    heap_start: usize,
    heap_end: usize,
    next: usize,
}

impl BumpAllocator {
    const fn new() -> Self {
        BumpAllocator {
            heap_start: 0,
            heap_end: 0,
            next: 0,
        }
    }

    pub unsafe fn init(&mut self) {
        self.heap_start = get_heap_base();
        self.heap_end = get_stack_base();
        self.next = self.heap_start;
    }
}

fn align_up(addr: usize, align: usize) -> usize {
    (addr + align - 1) & !(align - 1)
}

unsafe impl GlobalAlloc for Locked<BumpAllocator> {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        let mut bump = self.lock();

        let alloc_start = align_up(bump.next, layout.align());
        let alloc_end = match alloc_start.checked_add(layout.size()) {
            Some(end) => end,
            None => return ptr::null_mut(),
        };

        if alloc_end > bump.heap_end {
            return ptr::null_mut(); // out of memory
        } else {
            bump.next = alloc_end;

            return alloc_start as *mut u8;
        }
    }

    unsafe fn dealloc(&self, _ptr: *mut u8, _layout: Layout) {
        // let mut bump = self.lock();

        // bump.next -= layout.size();
    }
}

#[global_allocator]
pub static ALLOCATOR: Locked<BumpAllocator> = Locked::new(BumpAllocator::new());
