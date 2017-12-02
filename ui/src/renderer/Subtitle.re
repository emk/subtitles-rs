let style = ReactDOMRe.Style.make(
    ~position = "relative",
    ~marginBottom = "5px",
    ~minHeight = "40px",
    ()
);

let svgStyle = ReactDOMRe.Style.make(
    ~top = "0",
    ~left = "0",
    ~width = "32px",
    ~height = "32px",
    ~position = "absolute",
    ~fill = "lightgrey",
    ()
);

let inputStyle = ReactDOMRe.Style.make(
    ~position = "absolute",
    ~top = "0",
    ~left = "40px",
    ()
);

let subtitleStyle = ReactDOMRe.Style.make(
    ~margin = "0 0 0 60px",
    ()
);

let nativeSubtitleStyle = ReactDOMRe.Style.combine(
    subtitleStyle,
    ReactDOMRe.Style.make(
        ~fontSize = "90%",
        ~color = "grey",
        ()
    )
);

let component = ReasonReact.statelessComponent("Player");

let make = (
    ~subtitle: Models.subtitle,
    ~playing = false,
    _children
) => {
    ...component,
    render: (_self) => {
        let key = string_of_float(fst(subtitle.period));
        let native = switch subtitle.native {
        | Some(text) =>
            <p style=nativeSubtitleStyle>
                (ReasonReact.stringToElement(text))
            </p>
        | None => ReasonReact.nullElement
        };
        <div key=key style=style>
            <input _type="checkbox" style=inputStyle />
            <svg viewBox="0 0 32 32" style=svgStyle>
                <use href="play.svg#play" />
            </svg>
            <p style=subtitleStyle>
                (ReasonReact.stringToElement(subtitle.foreign))
            </p>
            native
        </div>
    }
};
