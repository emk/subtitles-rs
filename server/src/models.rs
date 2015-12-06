use substudy::srt::Subtitle;

#[derive(RustcEncodable)]
pub struct Video {
    pub url: String,
    pub subtitles: Vec<(Option<Subtitle>, Option<Subtitle>)>,
}

