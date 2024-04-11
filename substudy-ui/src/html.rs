use std::{fmt, marker::PhantomData};

/// Typestate representing whether we trust a given fragment of HTML.
pub trait Trust {}

/// By default, HTML is untrusted.
pub struct Untrusted;
impl Trust for Untrusted {}

/// We trust HTML that we have generated ourselves, or that we passed through a
/// high-quality sanitizer.
pub struct Trusted;
impl Trust for Trusted {}

/// A fragment of HTML.
#[derive(Clone)]
pub struct Html<T: Trust = Trusted> {
    html: String,
    _trust: PhantomData<T>,
}

impl<T: Trust> fmt::Debug for Html<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Html({:?})", self.html)
    }
}

impl Html<Untrusted> {
    /// Sanitize the HTML, removing any potentially dangerous elements or
    /// attributes.
    #[cfg(server)]
    pub fn sanitize(self) -> Html<Trusted> {
        // Use `ammonia` to sanitize the HTML.
        let html = ammonia::clean(&self.html);
        Html {
            html,
            _trust: PhantomData,
        }
    }
}
