/// Global ID generator.
/// It is needed to uniquely identify a variable across scopes.
/// In the canonical jlox implementation it's not needed because all Java objects
/// have a globally unique ID provided by JVM.
use std::sync::atomic::{AtomicUsize, Ordering};

static ID_GENERATOR: AtomicUsize = AtomicUsize::new(0);

pub(crate) fn next_id() -> usize {
    ID_GENERATOR.fetch_add(1, Ordering::SeqCst)
}
