use common_failures::prelude::*;
use std::path::Path;
use substudy::align::align_available_files;
use substudy::srt;
use substudy::time::Period;
use url::Url;

#[derive(Debug, Serialize)]
pub struct Subtitle {
    pub period: Period,
    pub foreign: Option<String>,
    pub native: Option<String>,
}

impl Subtitle {
    pub fn new(foreign: Option<&srt::Subtitle>,
               native: Option<&srt::Subtitle>) ->
        Subtitle
    {
        let period = Period::from_union_opt(foreign.map(|s| s.period),
                                            native.map(|s| s.period))
            .expect("Should never have an empty subtitle pair");
        Subtitle {
            period: period,
            foreign: foreign.map(|s| s.plain_text()),
            native: native.map(|s| s.plain_text()),
        }
    }
}

#[derive(Debug, Serialize)]
pub struct Video {
    pub url: String,
    pub subtitles: Vec<Subtitle>,
}

impl Video {
    pub fn new(url: &Url, foreign_path: &Path, native_path: Option<&Path>) ->
        Result<Video>
    {
        let foreign = srt::SubtitleFile::cleaned_from_path(foreign_path)?;
        let native = match native_path {
            None => None,
            Some(ref path) =>
                Some(srt::SubtitleFile::cleaned_from_path(path)?)
        };
        let subtitles = align_available_files(&foreign, native.as_ref());

        Ok(Video {
            url: url.as_str().to_owned(),
            subtitles: subtitles.iter()
                .map(|&(ref f, ref n)| Subtitle::new(f.as_ref(), n.as_ref()))
                .collect()
        })
    }
}
