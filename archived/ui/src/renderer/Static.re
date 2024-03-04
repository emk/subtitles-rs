/* Import the magic value "__static" from Electron. */
[@bs.val] external __static : string = "__static";

/* Build a path to a static asset. */
let pathTo = (relPath) => "file://" ++ __static ++ "/" ++ relPath
