#![no_std]

/// This is a shoddy copy of Boo from liballoc
use core::cmp::Ordering;
use core::hash::{Hash, Hasher};
use core::ops::Deref;

use core::fmt;

use self::Boo::*;

pub use core::borrow::{Borrow, BorrowMut};
impl<'a, B: ?Sized> Borrow<B> for Boo<'a, B>
    where B: ToOwned,
          <B as ToOwned>::Owned: 'a
{
    fn borrow(&self) -> &B {
        &**self
    }
}

/// A generalization of `Clone` to borrowed data.
///
/// Some types make it possible to go from borrowed to owned, usually by
/// implementing the `Clone` trait. But `Clone` works only for going from `&T`
/// to `T`. The `ToOwned` trait generalizes `Clone` to construct owned data
/// from any borrow of a given type.
pub trait ToOwned {
    type Owned: Borrow<Self>;

    /// Creates owned data from borrowed data, usually by cloning.
    ///
    /// # Examples
    ///
    /// Basic usage:
    ///
    /// ```no_run
    /// let s: &str = "a";
    /// let ss: String = s.to_owned();
    ///
    /// let v: &[i32] = &[1, 2];
    /// let vv: Vec<i32> = v.to_owned();
    /// ```
    fn to_owned(&self) -> Self::Owned;

    fn clone_into(&self, target: &mut Self::Owned) {
        *target = self.to_owned();
	}
}

impl<T> ToOwned for T
    where T: Clone
{
    type Owned = T;
    fn to_owned(&self) -> T {
        self.clone()
    }

    fn clone_into(&self, target: &mut T) {
        target.clone_from(self);
    }
}

/// A clone-on-write smart pointer.
///
/// The type `Boo` is a smart pointer providing clone-on-write functionality: it
/// can enclose and provide immutable access to borrowed data, and clone the
/// data lazily when mutation or ownership is required. The type is designed to
/// work with general borrowed data via the `Borrow` trait.
///
/// `Boo` implements `Deref`, which means that you can call
/// non-mutating methods directly on the data it encloses. If mutation
/// is desired, `to_mut` will obtain a mutable reference to an owned
/// value, cloning if necessary.
///
/// # Examples
///
/// ```no_run
/// use std::borrow::Boo;
///
/// fn abs_all(input: &mut Boo<[i32]>) {
///     for i in 0..input.len() {
///         let v = input[i];
///         if v < 0 {
///             // Clones into a vector if not already owned.
///             input.to_mut()[i] = -v;
///         }
///     }
/// }
///
/// // No clone occurs because `input` doesn't need to be mutated.
/// let slice = [0, 1, 2];
/// let mut input = Boo::from(&slice[..]);
/// abs_all(&mut input);
///
/// // Clone occurs because `input` needs to be mutated.
/// let slice = [-1, 0, 1];
/// let mut input = Boo::from(&slice[..]);
/// abs_all(&mut input);
///
/// // No clone occurs because `input` is already owned.
/// let mut input = Boo::from(vec![-1, 0, 1]);
/// abs_all(&mut input);
/// ```
pub enum Boo<'a, B: ?Sized + 'a>
    where B: ToOwned
{
    /// Borrowed data.
    Borrowed(&'a B),

    /// Owned data.
    Owned(<B as ToOwned>::Owned),
}
impl<'a, B: ?Sized> Clone for Boo<'a, B>
    where B: ToOwned
{
    fn clone(&self) -> Boo<'a, B> {
        match *self {
            Borrowed(b) => Borrowed(b),
            Owned(ref o) => {
                let b: &B = o.borrow();
                Owned(b.to_owned())
            }
        }
    }

    fn clone_from(&mut self, source: &Boo<'a, B>) {
        if let Owned(ref mut dest) = *self {
            if let Owned(ref o) = *source {
                o.borrow().clone_into(dest);
                return;
            }
        }

        *self = source.clone();
    }
}

impl<'a, B: ?Sized> Boo<'a, B>
    where B: ToOwned
{
    /// Acquires a mutable reference to the owned form of the data.
    ///
    /// Clones the data if it is not already owned.
    ///
    /// # Examples
    ///
    /// ```no_run
    /// use std::borrow::Boo;
    ///
    /// let mut Boo = Boo::Borrowed("foo");
    /// Boo.to_mut().make_ascii_uppercase();
    ///
    /// assert_eq!(
    ///   Boo,
    ///   Boo::Owned(String::from("FOO")) as Boo<str>
    /// );
    /// ```
    pub fn to_mut(&mut self) -> &mut <B as ToOwned>::Owned {
        match *self {
            Borrowed(borrowed) => {
                *self = Owned(borrowed.to_owned());
                match *self {
                    Borrowed(..) => unreachable!(),
                    Owned(ref mut owned) => owned,
                }
            }
            Owned(ref mut owned) => owned,
        }
    }

    /// Extracts the owned data.
    ///
    /// Clones the data if it is not already owned.
    ///
    /// # Examples
    ///
    /// Calling `into_owned` on a `Boo::Borrowed` clones the underlying data
    /// and becomes a `Boo::Owned`:
    ///
    /// ```no_run
    /// use std::borrow::Boo;
    ///
    /// let s = "Hello world!";
    /// let Boo = Boo::Borrowed(s);
    ///
    /// assert_eq!(
    ///   Boo.into_owned(),
    ///   Boo::Owned(String::from(s))
    /// );
    /// ```
    ///
    /// Calling `into_owned` on a `Boo::Owned` is a no-op:
    ///
    /// ```
    /// use std::borrow::Boo;
    ///
    /// let s = "Hello world!";
    /// let Boo: Boo<str> = Boo::Owned(String::from(s));
    ///
    /// assert_eq!(
    ///   Boo.into_owned(),
    ///   Boo::Owned(String::from(s))
    /// );
    /// ```
    pub fn into_owned(self) -> <B as ToOwned>::Owned {
        match self {
            Borrowed(borrowed) => borrowed.to_owned(),
            Owned(owned) => owned,
        }
    }
}
impl<'a, B: ?Sized> Deref for Boo<'a, B>
    where B: ToOwned
{
    type Target = B;

    fn deref(&self) -> &B {
        match *self {
            Borrowed(borrowed) => borrowed,
            Owned(ref owned) => owned.borrow(),
        }
    }
}
impl<'a, B: ?Sized> Eq for Boo<'a, B> where B: Eq + ToOwned {}
impl<'a, B: ?Sized> Ord for Boo<'a, B>
    where B: Ord + ToOwned
{
    #[inline]
    fn cmp(&self, other: &Boo<'a, B>) -> Ordering {
        Ord::cmp(&**self, &**other)
    }
}
impl<'a, 'b, B: ?Sized, C: ?Sized> PartialEq<Boo<'b, C>> for Boo<'a, B>
    where B: PartialEq<C> + ToOwned,
          C: ToOwned
{
    #[inline]
    fn eq(&self, other: &Boo<'b, C>) -> bool {
        PartialEq::eq(&**self, &**other)
    }
}
impl<'a, B: ?Sized> PartialOrd for Boo<'a, B>
    where B: PartialOrd + ToOwned
{
    #[inline]
    fn partial_cmp(&self, other: &Boo<'a, B>) -> Option<Ordering> {
        PartialOrd::partial_cmp(&**self, &**other)
    }
}
impl<'a, B: ?Sized> fmt::Debug for Boo<'a, B>
    where B: fmt::Debug + ToOwned,
          <B as ToOwned>::Owned: fmt::Debug
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Borrowed(ref b) => fmt::Debug::fmt(b, f),
            Owned(ref o) => fmt::Debug::fmt(o, f),
        }
    }
}
impl<'a, B: ?Sized> fmt::Display for Boo<'a, B>
    where B: fmt::Display + ToOwned,
          <B as ToOwned>::Owned: fmt::Display
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Borrowed(ref b) => fmt::Display::fmt(b, f),
            Owned(ref o) => fmt::Display::fmt(o, f),
        }
    }
}
impl<'a, B: ?Sized> Default for Boo<'a, B>
    where B: ToOwned,
          <B as ToOwned>::Owned: Default
{
    /// Creates an owned Boo<'a, B> with the default value for the contained owned value.
    fn default() -> Boo<'a, B> {
        Owned(<B as ToOwned>::Owned::default())
    }
}
impl<'a, B: ?Sized> Hash for Boo<'a, B>
    where B: Hash + ToOwned
{
    #[inline]
    fn hash<H: Hasher>(&self, state: &mut H) {
        Hash::hash(&**self, state)
    }
}
#[allow(deprecated)]
impl<'a, T: ?Sized + ToOwned> AsRef<T> for Boo<'a, T> {
    fn as_ref(&self) -> &T {
        self
    }
}
