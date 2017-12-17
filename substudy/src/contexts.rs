//! Iteration in context, with the previous and next value, if available.
//!
//! ```
//! use substudy::contexts::{Context, ItemsInContextExt};
//!
//! let v: &[u8] = &[1, 2];
//! let ctxs: Vec<Context<u8>> = v.items_in_context()
//!     .map(|ctx| ctx.cloned())
//!     .collect();
//!
//! assert_eq!(v.len(), ctxs.len());
//!
//! assert_eq!(None,    ctxs[0].prev);
//! assert_eq!(1,       ctxs[0].curr);
//! assert_eq!(Some(2), ctxs[0].next);
//!
//! assert_eq!(Some(1), ctxs[1].prev);
//! assert_eq!(2,       ctxs[1].curr);
//! assert_eq!(None,    ctxs[1].next);
//! ```
//!
//! This file would be pretty high-order magic in Haskell, where we
//! wouldn't need to worry about the difference between values and
//! references.  In Rust, it's a bit like an evil hybrid of the abstraction
//! of Haskell and the reference wonkiness of C++.  It works well enough in
//! practice, but I'm not sure how I feel about it.

use std::clone::Clone;

/// An item in a slice, possibly with the item before or after it.
pub struct Context<T> {
    /// The previous item, if any.
    pub prev: Option<T>,
    /// The current item.
    pub curr: T,
    /// The next item, if any.
    pub next: Option<T>,
}

impl<'a, T> Context<&'a T> {
    /// Transform a `Context<&'a T>` to `Context<U>` by applying `f` to all
    /// embedded values.
    pub fn map<U, F>(&self, mut f: F) -> Context<U>
    where
        F: FnMut(&'a T) -> U,
    {
        Context {
            prev: self.prev.as_ref().map(|t| f(t)),
            curr: f(&self.curr),
            next: self.next.as_ref().map(|t| f(t)),
        }
    }
}

impl<'a, T: Clone> Context<&'a T> {
    /// Convert a `Context<&T>` to `Context<T>`.
    pub fn cloned(&self) -> Context<T> {
        self.map(|t| t.clone())
    }
}

impl<'a, T> Context<&'a Option<T>> {
    /// If we have a `Context<&Option<T>>`, that would make the type of
    /// `prev` a truly obnoxious `Option<&Option<T>>`.  This function will
    /// flatten this into `Option<&T>`, using the `OptContext` helper
    /// type.
    pub fn flatten(&self) -> OptContext<&'a T> {
        OptContext {
            prev: self.prev.and_then(|t| t.as_ref()),
            curr: self.curr.as_ref(),
            next: self.next.and_then(|t| t.as_ref()),
        }
    }
}

/// Like `Context`, but `curr` is also optional.
pub struct OptContext<T> {
    /// The previous item, if any.
    pub prev: Option<T>,
    /// The current item.
    pub curr: Option<T>,
    /// The next item, if any.
    pub next: Option<T>,
}

/// Iterator type for `ItemsInContextExt`.
pub struct ItemsInContext<'a, T>
where
    T: 'a,
{
    slice: &'a [T],
    idx: usize,
}

impl<'a, T> Iterator for ItemsInContext<'a, T> {
    type Item = Context<&'a T>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.idx >= self.slice.len() {
            None
        } else {
            let prev = if self.idx == 0 {
                None
            } else {
                self.slice.get(self.idx - 1)
            };
            let result = Context {
                prev: prev,
                curr: &self.slice[self.idx],
                next: self.slice.get(self.idx + 1),
            };
            self.idx += 1;
            Some(result)
        }
    }
}

/// Provides the `items_in_context` method.
pub trait ItemsInContextExt {
    /// The type of the item we're iterating over.
    type Item;

    /// Iterate over items, also supply the preceding and next item if
    /// available.
    fn items_in_context(&self) -> ItemsInContext<Self::Item>;
}

impl<'a, T> ItemsInContextExt for &'a [T] {
    type Item = T;

    fn items_in_context(&self) -> ItemsInContext<Self::Item> {
        ItemsInContext {
            slice: self,
            idx: 0,
        }
    }
}

// TODO: We can probably do something clever with the type of slice
// implementation above and combine these two.
impl<T> ItemsInContextExt for Vec<T> {
    type Item = T;

    fn items_in_context(&self) -> ItemsInContext<Self::Item> {
        ItemsInContext {
            slice: &self,
            idx: 0,
        }
    }
}
