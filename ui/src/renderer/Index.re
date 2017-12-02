let renderRoot = (elem) => ReactDOMRe.renderToElementWithId(elem, "app");

let renderPlaceholder = () => {
    let text = "Use File > Open... to open a file.";
    renderRoot(<p>{ReasonReact.stringToElement(text)}</p>);
};

let renderVideoJson = (json) => {
    let video = Models.Decode.video(json);
    renderRoot(<Player video=video />);
};
