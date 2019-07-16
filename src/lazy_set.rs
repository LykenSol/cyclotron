use std::collections::BTreeSet;
use std::fmt;
use std::hash::Hash;

// HACK(eddyb) sealed traits/enums to keep them from being used outside of this module.
mod sealed {
    pub trait LazySet<CallK, CallV>: Clone {
        type Item;
        type Iter: Iterator<Item = Self::Item>;
        fn compute(self, call: impl FnMut(CallK, Self), last_result: Option<CallV>) -> Self::Iter;
    }

    #[derive(Clone)]
    pub struct Once<T>(pub(super) T);

    impl<CallK, CallV, T: Clone> LazySet<CallK, CallV> for Once<T> {
        type Item = T;
        type Iter = std::iter::Once<T>;
        fn compute(self, _: impl FnMut(CallK, Self), _: Option<CallV>) -> Self::Iter {
            std::iter::once(self.0)
        }
    }

    #[derive(Clone)]
    pub enum Call<CallK> {
        Start(CallK),
        Complete,
    }

    impl<CallK: Clone, CallV> LazySet<CallK, CallV> for Call<CallK> {
        type Item = CallV;
        type Iter = std::option::IntoIter<CallV>;
        fn compute(
            self,
            mut call: impl FnMut(CallK, Self),
            last_result: Option<CallV>,
        ) -> Self::Iter {
            match self {
                Call::Start(k) => {
                    call(k, Call::Complete);
                    None.into_iter()
                }
                Call::Complete => last_result.into_iter(),
            }
        }
    }

    #[derive(Clone)]
    pub enum Union<A, B> {
        Start(A, B),
        JustA(A),
        JustB(B),
    }

    type OptIter<I> = std::iter::Flatten<std::option::IntoIter<I>>;

    fn opt_iter<I: Iterator>(opt: Option<I>) -> OptIter<I> {
        opt.into_iter().flatten()
    }

    impl<CallK, CallV, A, B, T> LazySet<CallK, CallV> for Union<A, B>
    where
        A: LazySet<CallK, CallV, Item = T>,
        B: LazySet<CallK, CallV, Item = T>,
    {
        type Item = T;
        type Iter = std::iter::Chain<OptIter<A::Iter>, OptIter<B::Iter>>;
        fn compute(
            self,
            mut call: impl FnMut(CallK, Self),
            mut last_result: Option<CallV>,
        ) -> Self::Iter {
            let (a, b) = match self {
                Union::Start(a, b) => (Some(a), Some(b)),
                Union::JustA(a) => (Some(a), None),
                Union::JustB(b) => (None, Some(b)),
            };
            opt_iter(a.map(|a| a.compute(|k, a| call(k, Union::JustA(a)), last_result.take())))
                .chain(opt_iter(b.map(|b| {
                    b.compute(|k, b| call(k, Union::JustB(b)), last_result)
                })))
        }
    }

    #[derive(Clone)]
    pub struct Map<A, F>(pub(super) A, pub(super) F);

    impl<CallK, CallV, A, F, T> LazySet<CallK, CallV> for Map<A, F>
    where
        A: LazySet<CallK, CallV>,
        F: Fn(A::Item) -> T + Clone,
    {
        type Item = T;
        type Iter = std::iter::Map<A::Iter, F>;
        fn compute(
            self,
            mut call: impl FnMut(CallK, Self),
            last_result: Option<CallV>,
        ) -> Self::Iter {
            let Map(a, f) = self;
            a.compute(|k, a| call(k, Map(a, f.clone())), last_result)
                .map(f)
        }
    }

    #[derive(Clone)]
    pub struct FilterMap<A, F>(pub(super) A, pub(super) F);

    impl<CallK, CallV, A, F, T> LazySet<CallK, CallV> for FilterMap<A, F>
    where
        A: LazySet<CallK, CallV>,
        F: Fn(A::Item) -> Option<T> + Clone,
    {
        type Item = T;
        type Iter = std::iter::FilterMap<A::Iter, F>;
        fn compute(
            self,
            mut call: impl FnMut(CallK, Self),
            last_result: Option<CallV>,
        ) -> Self::Iter {
            let FilterMap(a, f) = self;
            a.compute(|k, a| call(k, FilterMap(a, f.clone())), last_result)
                .filter_map(f)
        }
    }

    #[derive(Clone)]
    pub enum FlatMap<A, B, F> {
        Start(A, F),
        Then(B),
    }

    impl<CallK, CallV, A, B, F> LazySet<CallK, CallV> for FlatMap<A, B, F>
    where
        A: LazySet<CallK, CallV>,
        B: LazySet<CallK, CallV>,
        F: Fn(A::Item) -> B + Clone,
    {
        type Item = B::Item;
        // FIXME(eddyb) this is inefficient, but it gets around having
        // to return an iterator which captures `call`.
        type Iter = std::vec::IntoIter<Self::Item>;
        fn compute(
            self,
            mut call: impl FnMut(CallK, Self),
            mut last_result: Option<CallV>,
        ) -> Self::Iter {
            let (a_map_f, b) = match self {
                FlatMap::Start(a, f) => (
                    Some(
                        a.compute(
                            |k, a| call(k, FlatMap::Start(a, f.clone())),
                            last_result.take(),
                        )
                        .map(f),
                    ),
                    None,
                ),
                FlatMap::Then(b) => (None, Some(b)),
            };
            a_map_f
                .into_iter()
                .flatten()
                .chain(b.into_iter())
                .flat_map(|b| b.compute(|k, b| call(k, FlatMap::Then(b)), last_result.take()))
                .collect::<Vec<_>>()
                .into_iter()
        }
    }
}

// NOTE(eddyb) this would normally be named `LazySetExt`, but we want to allow
// users to use this trait in bounds, without exposing the sealed one.
pub trait LazySet<CallK, CallV>: sealed::LazySet<CallK, CallV> {
    fn union<B: LazySet<CallK, CallV, Item = Self::Item>>(self, b: B) -> sealed::Union<Self, B> {
        sealed::Union::Start(self, b)
    }

    fn map<F: Fn(Self::Item) -> T, T>(self, f: F) -> sealed::Map<Self, F> {
        sealed::Map(self, f)
    }

    fn filter_map<F: Fn(Self::Item) -> Option<T>, T>(self, f: F) -> sealed::FilterMap<Self, F> {
        sealed::FilterMap(self, f)
    }

    fn flat_map<B: LazySet<CallK, CallV>, F: Fn(Self::Item) -> B>(
        self,
        f: F,
    ) -> sealed::FlatMap<Self, B, F> {
        sealed::FlatMap::Start(self, f)
    }
}

impl<CallK, CallV, A: sealed::LazySet<CallK, CallV>> LazySet<CallK, CallV> for A {}

pub fn once<T>(v: T) -> sealed::Once<T> {
    sealed::Once(v)
}

pub fn call<CallK>(k: CallK) -> sealed::Call<CallK> {
    sealed::Call::Start(k)
}

// FIXME(eddyb) this should be in `LazyExt`, but `-> impl Trait`
// doesn't work in traits yet, move it there whenever that changes.
pub fn to_eager<K, V, F, A>(lazy_call: F) -> impl crate::eager::Memoized<K, Value = BTreeSet<V>>
where
    K: Copy + Eq + Hash + fmt::Debug,
    V: Clone + Ord + fmt::Debug,
    F: Fn(K) -> A + Clone,
    A: LazySet<K, V, Item = V>,
{
    fn compute_eagerly<K, V: Ord>(
        lazy: impl LazySet<K, V, Item = V>,
        call: &mut impl FnMut(K) -> BTreeSet<V>,
        last_result: Option<V>,
        output: &mut BTreeSet<V>,
    ) {
        let values = lazy.compute(
            |k, cont| {
                for v in call(k) {
                    compute_eagerly(cont.clone(), call, Some(v), output);
                }
            },
            last_result,
        );
        output.extend(values);
    }

    crate::eager::memoize(move |f, k| -> BTreeSet<V> {
        let mut set = BTreeSet::new();
        compute_eagerly(lazy_call(k), &mut |k| f.call(k).clone(), None, &mut set);
        set
    })
}
