#[cfg(not(test))]
pub struct Locked<A> {
    inner: spin::Mutex<A>,
}

#[cfg(not(test))]
impl<A> Locked<A> {
    pub const fn new(inner: A) -> Self {
        Locked {
            inner: spin::Mutex::new(inner),
        }
    }

    pub fn lock(&self) -> spin::MutexGuard<A> {
        self.inner.lock()
    }
}

