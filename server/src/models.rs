use substudy::srt::Subtitle;

#[derive(RustcEncodable)]
pub struct Video<'a> {
    pub url: String,
    pub subtitles: &'a [(Option<Subtitle>, Option<Subtitle>)],
}

