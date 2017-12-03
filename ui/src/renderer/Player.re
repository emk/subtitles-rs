type state = {
    videoRef: ref(option(ReasonReact.reactRef)),
    currentTime: float,
};

type action =
| TimeUpdate(float);

let component = ReasonReact.reducerComponent("Player");

let make = (~video: Models.video, _children) => {
    ...component,

    initialState: () => {
        videoRef: ref(None),
        currentTime: 0.,
    },

    reducer: (action, state) => {
        switch (action) {
        | TimeUpdate(currentTime) => Update({ ...state, currentTime })
        }
    },

    render: (self) => {
        /* Record a ref to our `<Video>` component for later use. */
        let setVideoRef = (videoRef, self) => {
            self.ReasonReact.state.videoRef := Js.Nullable.to_opt(videoRef);
        };

        /* Called when a subtitle is clicked. */
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

        /* Called when the video seeks to a new time. */
        let onTimeUpdate = (currentTime) => {
            TimeUpdate(currentTime)
        };

        /* Build our HTML. */
        let subtitles = video.subtitles
            |> Array.map((subtitle: Models.subtitle) => {
                let playing = Models.periodContains(self.state.currentTime, subtitle.period);
                <Subtitle subtitle playing onPlayRange />
            });
        <div className="player">
            <Video
                ref={self.handle(setVideoRef)}
                onSizeKnown
                onTimeUpdate={self.reduce(onTimeUpdate)}
                src=video.url />
            <div className="subtitles">
              (ReasonReact.arrayToElement(subtitles))
            </div>
        </div>
    }
};
