let component = ReasonReact.statelessComponent("Player");

let make = (~video: Models.video, _children) => {
    ...component,
    render: (_self) => {
        let subtitles = video.subtitles
            |> Array.map((sub: Models.subtitle) => {
                <Subtitle subtitle=sub />
            });
        <div className="player">
            <Video src=video.url />
            <div className="subtitles">
              (ReasonReact.arrayToElement(subtitles))
            </div>
        </div>
    }
};
