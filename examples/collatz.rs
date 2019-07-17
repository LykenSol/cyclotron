use cyclotron::bruteforce::Memoized as _;
use cyclotron::lazy_set::LazySet as _;

use std::collections::BTreeSet;

fn collatz_next(x: u64) -> u64 {
    if x % 2 == 0 {
        x / 2
    } else {
        3 * x + 1
    }
}

fn test(mut collatz_all: impl FnMut(u64) -> BTreeSet<u64>) {
    eprintln!("collatz_all(19) = {:?}", collatz_all(19));
    eprintln!("collatz_all(2) = {:?}", collatz_all(2));
}

fn bruteforce() {
    use cyclotron::bruteforce::memoize;

    let mut collatz_all = memoize(|collatz_all, x| -> BTreeSet<u64> {
        let next = collatz_next(x);
        let mut set = collatz_all.call(next);
        set.insert(next);
        set
    });
    test(|x| collatz_all.call(x));
}

fn lazy_set() {
    use cyclotron::lazy_set::{call, memoize_by_bruteforce, one};

    let collatz_all = call;
    let collatz_all = |x| {
        let next = collatz_next(x);
        collatz_all(next).union(one(next))
    };

    // Bruteforce `LazySet` execution.
    {
        let mut collatz_all = memoize_by_bruteforce(collatz_all);
        test(|x| collatz_all.call(x));
    }
}

fn main() {
    bruteforce();
    lazy_set();
}
