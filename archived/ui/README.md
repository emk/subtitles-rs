# Experimental `substudy` UI using Electron

This is a cross-platform GUI based on [Electron][] and [electron-webpack][]. For the actual code, we use [ReasonML][] and [Reason-React][].

[Electron]: https://electronjs.org/
[electron-webpack]: https://github.com/electron-userland/electron-webpack
[ReasonML]: https://reasonml.github.io/
[Reason-React]: https://reasonml.github.io/reason-react/

### Development Scripts

To set up the environment, you'll need to install [Node][] 8.x and [Yarn][] normally. From there, you can run:

```sh
# Install the BuckleScript compiler used by ReasonML.
npm install -g bs-platform
```

Then you can install the dependencies for this project:

```sh
yarn
npm link bs-platform
```

Once that is done, you can open two terminals and run one of the following commands in each:

```sh
# Run in terminal #1 to compile ReasonML to JavaScript.
bsb -w

# Run in terminal #2 to package JavaScript using Webpack.
yarn dev
```

To produce release builds, you could also try the following commands:

```bash
# compile source code and create webpack output
yarn compile

# `yarn compile` & create build with electron-builder
yarn dist

# `yarn compile` & create unpacked build with electron-builder
yarn dist:dir
```

[Node]: https://nodejs.org/en/
[Yarn]: https://yarnpkg.com/
