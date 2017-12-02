/* Import the magic value "__static" from Electron. */
[@bs.val] external __static : string = "__static";

print_endline("Starting up");

/* Temporary: Build a URL pointing to a local file. */
let url = "file://" ++ __static ++ "/matando_cabos.mp4";
print_endline("Using URL " ++ url);

ReactDOMRe.renderToElementWithId(<Video src=url />, "app");
