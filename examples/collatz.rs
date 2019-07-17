use cyclotron::bruteforce;
use cyclotron::lazy_set::LazySet as _;

use std::collections::BTreeSet;

fn collatz_next(x: u64) -> u64 {
    if x % 2 == 0 {
        x / 2
    } else {
        3 * x + 1
    }
}

fn test(name: &str, mut collatz_all: impl FnMut(u64) -> BTreeSet<u64>) {
    eprintln!("Testing {}:", name);
    eprintln!("  collatz_all(19) = {:?}", collatz_all(19));
    eprintln!("  collatz_all(2) = {:?}", collatz_all(2));
}

fn test_bruteforce() {
    test(
        "bruteforce",
        bruteforce::memoize(|collatz_all, x| -> BTreeSet<u64> {
            let next = collatz_next(x);
            let mut set = collatz_all(next);
            set.insert(next);
            set
        }),
    );
}

fn test_lazy_set() {
    use cyclotron::lazy_set::{call, one, to_eager};

    let collatz_all = call;
    let collatz_all = |x| {
        let next = collatz_next(x);
        collatz_all(next).union(one(next))
    };

    // Bruteforce `LazySet` execution.
    test(
        "lazy_set/bruteforce",
        bruteforce::memoize(to_eager(collatz_all)),
    );
}

fn main() {
    test_bruteforce();
    test_lazy_set();
}
