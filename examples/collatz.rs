use cyclotron::eager::{memoize, Memoized};

use std::collections::BTreeSet;

fn collatz_next(x: u64) -> u64 {
    if x % 2 == 0 {
        x / 2
    } else {
        3 * x + 1
    }
}

fn main() {
    let mut collatz_all = memoize(|collatz_all, x| -> BTreeSet<u64> {
        let next = collatz_next(x);
        let mut set = collatz_all.call(next).clone();
        set.insert(next);
        set
    });
    eprintln!("collatz_all(19) = {:?}", collatz_all.call(19));
    eprintln!("collatz_all(2) = {:?}", collatz_all.call(2));
}
