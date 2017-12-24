# `common_failures`: Useful `Fail` implementations and error-related utilities

Many standard Rust error types, such as `std::io::Error` carry too little information to provide user-friendly error messages. (For example, pathnames are never included.) We provide more informative error wrappers for common Rust error types.

We also provide a `quick_main!` implementation like the one provided by `error-chain`, and a number of other useful utilities for creating and formatting errors.
