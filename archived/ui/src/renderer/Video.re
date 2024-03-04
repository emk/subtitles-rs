/* Declare some DOM bindings to make our lives easier. */
[@bs.get] external currentTime : Dom.element => float = "";
[@bs.set] external setCurrentTime : Dom.element => float => unit = "currentTime";
[@bs.get] external videoWidth : Dom.element => float = "";
[@bs.get] external videoHeight : Dom.element => float = "";
[@bs.send] external playElem : (Dom.element) => unit = "play";
[@bs.send] external pauseElem : (Dom.element) => unit = "pause";

/* A subset of actions that can be invoked externally. */
type command =
  | PlayRange(float, float);

/* Hacky custom hook to allow our parent to dispatch commands to us. Every other
 * virtual DOM framework can handle this except React, I think!
 */
type commandHandler = command => unit;
[@bs.get] external getCommandHandler : Dom.element => commandHandler = "substudy__commandHandler";
[@bs.set] external setCommandHandler : Dom.element => commandHandler => unit = "substudy__commandHandler";

type state = {
    videoElemRef: ref(option(Dom.element)),
    isPlaying: bool,
    stopAt: option(float),
};

type action =
  | Command(command);

let component = ReasonReact.reducerComponent("Video");

let make = (
    ~src,
    ~onSizeKnown = ?,
    ~onPlayingUpdate = ?,
    ~onTimeUpdate = ?,
    _children
) => {
    ...component,

    /* Our initial state. */
    initialState: () => {
        videoElemRef: ref(None),
        isPlaying: false,
        stopAt: None,
    },

    /* Use an action to update the state of our video element.. */
    reducer: (action, state) => {
        switch action {
        | Command(PlayRange(startTime, endTime)) =>
            UpdateWithSideEffects(
                { ...state, stopAt: Some(endTime) },
                (self) => {
                    switch (self.state.videoElemRef^) {
                    | Some(videoElem) =>
                        setCurrentTime(videoElem, startTime);
                        playElem(videoElem);
                    | None => ()
                    }
                }
            )
        }
    },

    /* Render our <video> element. */
    render: (self) => {
        /* Called when we know how large, long, etc., the movie is. */
        let onLoadedMetadata = (event) => {
            let videoElem = ReactEventRe.Media.target(event);
            let width = videoWidth(videoElem);
            let height = videoHeight(videoElem);
            print_endline("Video size " ++ string_of_float(width)
                          ++ "x" ++ string_of_float(height));
            switch onSizeKnown {
            | Some(f) => f(width, height)
            | None => ()
            };
        };

        /* Called when the current playback time changes. */
        let onTimeUpdate_ = (event) => {
            let videoElem = ReactEventRe.Media.target(event);
            let currentTime = currentTime(videoElem);
            print_endline("onTimeUpdate: " ++ string_of_float(currentTime));
            switch onTimeUpdate {
            | Some(f) => f(currentTime)
            | None => ()
            }
        };

        /* Called when the movie is played. */
        let onPlay = (_event) => {
            print_endline("onPlay");
            switch onPlayingUpdate {
            | Some(f) => f(true)
            | None => ()
            }
        };

        /* Called when the movie is paused. */
        let onPause = (_event) => {
            print_endline("onPause");
            switch onPlayingUpdate {
            | Some(f) => f(false)
            | None => ()
            }
        };

        let installCommandHandler = (videoRef, self): unit => {
            self.ReasonReact.state.videoElemRef := Js.Nullable.to_opt(videoRef);
            switch (Js.Nullable.to_opt(videoRef)) {
            | Some(video) =>
                setCommandHandler(video, (command): unit => {
                    self.ReasonReact.reduce((_) => Command(command))("ignored");
                })
            | None => ()
            };
        };

        let controls = Js.true_;
        let width = "100%";
        <video ref={self.handle(installCommandHandler)} controls width onLoadedMetadata onTimeUpdate=onTimeUpdate_ onPlay onPause>
            <source src />
        </video>
    }
};

/* Run a command against this element. */
let runCommand = (video: ReasonReact.reactRef, command: command): unit => {
    let videoElem = ReactDOMRe.findDOMNode(video);
    getCommandHandler(videoElem)(command);
};
