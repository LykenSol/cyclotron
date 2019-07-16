use std::collections::{hash_map::Entry, HashMap};
use std::hash::Hash;
use std::mem;

pub trait Memoized<K> {
    type Value;

    fn call(&mut self, k: K) -> &Self::Value;
}

#[derive(Copy, Clone)]
enum CacheState {
    Complete,
    InProgress { entry_depth: usize, accessed: bool },
    Invalid,
}

struct CacheEntry<T> {
    state: CacheState,
    value: T,
}

struct MemoState<K, V, F> {
    depth: usize,
    largest_cycle: Option<usize>,
    cache: HashMap<K, CacheEntry<V>>,
    f: F,
}

pub fn memoize<K, V, F>(f: F) -> impl Memoized<K, Value = V>
where
    K: Copy + Eq + Hash + std::fmt::Debug,
    V: Default + Eq + std::fmt::Debug,
    F: Clone + Fn(&mut dyn Memoized<K, Value = V>, K) -> V,
{
    MemoState {
        depth: 0,
        largest_cycle: None,
        cache: HashMap::new(),
        f,
    }
}

// FIXME(eddyb) try to figure out if dynamic dispatch can be avoided.
// However, it will likely be devirtualized, as it's only used to pass
// `self` to `F` in `fn call` below.
impl<K, V, F> Memoized<K> for MemoState<K, V, F>
where
    K: Copy + Eq + Hash + std::fmt::Debug,
    V: Default + Eq + std::fmt::Debug,
    F: Clone + Fn(&mut dyn Memoized<K, Value = V>, K) -> V,
{
    type Value = V;

    fn call(&mut self, k: K) -> &V {
        let entry = match self.cache.entry(k) {
            Entry::Occupied(entry) => {
                let entry = entry.into_mut();
                match &mut entry.state {
                    // FIXME(eddyb) `return &entry.value` once Polonius is the default.
                    CacheState::Complete => return &self.cache[&k].value,
                    CacheState::InProgress {
                        entry_depth,
                        accessed,
                    } => {
                        *accessed = true;

                        // Keep track of the largest overall cycle.
                        self.largest_cycle =
                            Some(self.largest_cycle.unwrap_or(*entry_depth).min(*entry_depth));

                        // FIXME(eddyb) `return &entry.value` once Polonius is the default.
                        return &self.cache[&k].value;
                    }
                    CacheState::Invalid => {
                        entry.value = V::default();
                    }
                }
                entry
            }
            Entry::Vacant(entry) => entry.insert(CacheEntry {
                state: CacheState::Invalid,
                value: V::default(),
            }),
        };

        entry.state = CacheState::InProgress {
            entry_depth: self.depth,
            accessed: false,
        };

        loop {
            self.depth += 1;
            let f = self.f.clone();
            let v = f(self, k);
            self.depth -= 1;

            let entry = self.cache.get_mut(&k).unwrap();
            let old_value = mem::replace(&mut entry.value, v);

            match &mut entry.state {
                CacheState::InProgress { accessed, .. } => {
                    if *accessed {
                        *accessed = false;

                        // Keep retrying as long as we haven't achieved fixpoint.
                        if entry.value != old_value {
                            continue;
                        }
                    }
                }
                CacheState::Complete => {
                    // Just finished a redundant recursion, make sure it
                    // didn't break the fixpoint (see cycle exit logic below).
                    assert_eq!(entry.value, old_value);
                }
                _ => unreachable!(),
            }

            entry.state = CacheState::Complete;

            // If we are the largest cycle, it's time to exit it.
            if self.largest_cycle == Some(self.depth) {
                self.largest_cycle = None;

                // Recurse one more time to replace `Invalid` cache entries.
                continue;
            }

            // Poison any cached entries that happen to have been computed
            // during a cycle, so that further accesses recompute them.
            if let Some(cycle_entry_depth) = self.largest_cycle {
                assert!(cycle_entry_depth < self.depth);
                entry.state = CacheState::Invalid;
            }

            // FIXME(eddyb) `return &entry.value` once Polonius is the default.
            return &self.cache.get_mut(&k).unwrap().value;
        }
    }
}
