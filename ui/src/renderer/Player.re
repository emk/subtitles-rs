type state = {
    videoRef: ref(option(ReasonReact.reactRef)),
};

type action = unit;

let component = ReasonReact.reducerComponent("Player");

let make = (~video: Models.video, _children) => {
    ...component,

    initialState: () => {
        videoRef: ref(None)
    },

    reducer: (_action: action, _state: state) => {
        NoUpdate
    },

    render: (self) => {
        /* Record a ref to our `<Video>` component for later use. */
        let setVideoRef = (videoRef, self) => {
            self.ReasonReact.state.videoRef := Js.Nullable.to_opt(videoRef);
        };

        let onPlayRange = ((startTime, endTime)) => {
            let command = Video.PlayRange(startTime, endTime);
            switch (self.state.videoRef^) {
            | Some(video) => Video.runCommand(video, command)
            | None => ()
            };
        };

        /* Called when we know how big our video is. */
        let onSizeKnown = (_width, _height) => {
            /* TODO: Update style for subtitles. */
            ()
        };

        /* Build our HTML. */
        let subtitles = video.subtitles
            |> Array.map((subtitle: Models.subtitle) => {
                <Subtitle subtitle onPlayRange=Some(onPlayRange) />
            });
        <div className="player">
            <Video ref={self.handle(setVideoRef)} onSizeKnown=Some(onSizeKnown) src=video.url />
            <div className="subtitles">
              (ReasonReact.arrayToElement(subtitles))
            </div>
        </div>
    }
};
