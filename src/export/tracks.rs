//! Output a video as a series of short audio tracks, short enough to make
//! it easy to skip backwards a conversation with most MP3 players.

use std::io::{Cursor, Write};

use err::Result;
use export::Exporter;
use time::Period;

// Should we merge two time periods into one?
fn should_merge(p1: Period, p2: Period) -> bool {
    if p1.duration() >= 30.0 {
        false
    } else {
        match p1.distance(p2) {
            None => true,
            Some(d) if d <= 5.0 => true,
            _ => false
        }
    }
}

/// Export the video as a set of tracks.
pub fn export_tracks(exporter: &mut Exporter) -> Result<()> {
    let mut convs: Vec<Period> = vec!();

    // Figure out how to combine subtitles into conversations.
    for sub in &exporter.foreign().subtitles.subtitles {
        if let Some(prev) = convs.last_mut() {
            if should_merge(*prev, sub.period) {
                *prev = prev.union(sub.period);
                continue;
            }
        }
        convs.push(sub.period);
    }

    // Add padding to each audio clip.
    for conv in convs.iter_mut() {
        *conv = conv.grow(2.5, 2.5);
    }

    // Turn overlapping clips into seamless transitions.
    // TODO: Fade in/out when not a seamless transition.
    for i in 1..convs.len() {
        if convs[i].begin() < convs[i-1].end()  {
            let overlap = Period::new(convs[i].begin(), convs[i-1].end())
                .unwrap();
            let midpoint = overlap.midpoint();
            debug!("Overlap: {:?} {:?} {:?} {}",
                   convs[i-1], convs[i], overlap, midpoint);
            convs[i-1] = Period::new(convs[i-1].begin(), midpoint).unwrap();
            convs[i] = Period::new(midpoint, convs[i].end()).unwrap();
            assert!(convs[i].begin() >= convs[i-1].end());
        }
    }

    // Schedule exports and write our our m3u8 playlist (like m3u, but
    // UTF-8).
    //
    // TODO: Genre, artist, album, track title, track number.
    let foreign_lang = exporter.foreign().language;
    let mut buff = Cursor::new(vec!());
    for conv in convs {
        let path = exporter.schedule_audio_export(foreign_lang, conv);
        try!(writeln!(buff, "{}", &path));
        debug!("Conv: {:7.1} -> {:7.1} for {:7.1}",
               conv.begin(), conv.end(), conv.duration());
    }
    try!(exporter.export_data_file("playlist.m3u8", &buff.get_ref()));

    // Extract our media files.
    try!(exporter.finish_exports());

    Ok(())
}
