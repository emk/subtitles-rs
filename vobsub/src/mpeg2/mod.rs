//! The `*.sub` portion of VobSub subtitles is packaged in MPEG-2 Program
//! Stream packets, which we have some limited support for parsing.

mod clock;
pub mod pes;
pub mod ps;


