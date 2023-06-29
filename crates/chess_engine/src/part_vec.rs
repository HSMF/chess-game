use std::fmt::Debug;

use tinyvec::{Array, ArrayVec};

/// A vector that is partially on the stack and partially on the heap
#[derive(Clone, PartialEq, Eq)]
pub struct PartVec<A: Array> {
    small: ArrayVec<A>,
    rest: Vec<A::Item>,
}

impl<A: Array> FromIterator<A::Item> for PartVec<A> {
    #[inline]
    fn from_iter<T: IntoIterator<Item = A::Item>>(iter: T) -> Self {
        let iter = iter.into_iter();
        let size = iter.size_hint().0;
        let mut out = Self::with_capacity(size);
        for i in iter {
            out.push(i);
        }
        out
    }
}

impl<'a, A: Array> IntoIterator for &'a PartVec<A> {
    type Item = &'a A::Item;

    type IntoIter = Iter<'a, A::Item>;

    #[inline]
    fn into_iter(self) -> Self::IntoIter {
        Iter {
            small: self.small.as_slice(),
            rest: self.rest.as_slice(),
        }
    }
}

impl<A: Array> Debug for PartVec<A>
where
    A::Item: Debug,
{
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list()
            .entries(self.small.iter())
            .entries(self.rest.iter())
            .finish()
    }
}

impl<A: Array> PartVec<A> {
    #[inline]
    pub fn new() -> Self {
        Self {
            small: ArrayVec::new(),
            rest: Vec::new(),
        }
    }

    #[inline]
    pub fn push(&mut self, val: A::Item) {
        if let Some(val) = self.small.try_push(val) {
            self.rest.push(val);
        }
    }

    #[inline]
    pub fn with_capacity(size: usize) -> Self {
        let surplus = size.saturating_sub(A::CAPACITY);
        Self {
            small: ArrayVec::new(),
            rest: Vec::with_capacity(surplus),
        }
    }

    pub fn sort_parts_by_key<F, K>(&mut self, mut f: F)
    where
        F: FnMut(&A::Item) -> K,
        K: Ord,
    {
        // todo: consider sorting everything
        self.small.sort_by_key(&mut f);
        self.rest.sort_by_key(f);
    }
}

impl<A: Array> PartVec<A>
where
    A::Item: Ord,
{
    pub fn sort_parts(&mut self) {
        // todo: consider sorting everything
        self.small.sort();
        self.rest.sort();
    }
}

impl<A: Array> Default for PartVec<A> {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone)]
pub struct Iter<'a, T> {
    small: &'a [T],
    rest: &'a [T],
}

impl<'a, T> Iterator for Iter<'a, T> {
    type Item = &'a T;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        if let Some((first, small)) = self.small.split_first() {
            self.small = small;
            return Some(first);
        }
        let (first, rest) = self.rest.split_first()?;
        self.rest = rest;
        Some(first)
    }
}
