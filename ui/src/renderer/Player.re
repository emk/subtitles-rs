let component = ReasonReact.statelessComponent("Player");

let make = (~video: Models.video, _children) => {
    ...component,
    render: (_self) => {
        let subtitles = video.subtitles
            |> Array.map((sub: Models.subtitle) => {
                let foreign = switch (sub.foreign) {
                | Some(text) => text
                | None => "(not available)"
                };
                <p>(ReasonReact.stringToElement(foreign))</p>
            });
        <div className="player">
            <Video src=video.url />
            <div className="subtitles">
              (ReasonReact.arrayToElement(subtitles))
            </div>
        </div>
    }
};
