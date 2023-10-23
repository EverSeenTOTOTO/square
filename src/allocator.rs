use buddy_system_allocator::LockedHeap;

#[global_allocator]
pub static ALLOCATOR: LockedHeap<32> = LockedHeap::<32>::new();

