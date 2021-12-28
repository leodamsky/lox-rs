use std::sync::atomic::{AtomicUsize, Ordering};

static ID_GENERATOR: AtomicUsize = AtomicUsize::new(0);

pub(crate) fn next_id() -> usize {
    ID_GENERATOR.fetch_add(1, Ordering::SeqCst)
}
