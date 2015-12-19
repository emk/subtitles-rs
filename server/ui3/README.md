# Experiment substudy web UI, take 2

Elm has some really nice tooling and brilliant ideas.  But it also involves
a lot of boilerplate code, and I kept running into a lot of practical
problems.  So I've decided to take the hardcore route and see if
[purescript-halogen][] works any better for this app.

## Installing the necessary tools & building the UI

You will need: [`node` and `npm`][node] to get started.  Pretty much all
modern JavaScript development tools rely on NodeJS in some fashion.  At
the time of this writing, I'm running version 4.2.3.

Once node is running, you need to run the following in this directory:

```sh
npm install -g bower purescript pulp proxrox
npm install
```

(If you encounter mysterious errors, I'm running bower 1.7.1, the
Purescript psc compiler 0.7.6.1, pulp 6.0.1 and proxrox 1.10.2.)

To build the front end, you should be able to run:

```sh
npm run-script compile
```

If you want to modify the front end instead, first make sure you have a
copy of nginx installed to use a development proxy.  You can install this
using either your system's package manager, or using `proxrox install`.
Once this is done, you can run:

```sh
proxrox start .proxrox.yml
npm start
```

## Learning Purescript

OK, Purescript is not the easiest or most familiar language in the world.
It's basically similar to Haskell, except that it's designed to
interoperate closely with regular JavaScript. Happily, there's
[Purescript by Example][], which is an excellent introductory book that
will teach you all the basics.

This UI uses [purescript-halogen][], which is inspired by [React.js][].
The basic idea is that you have:

1. An object describing your entire application state.
2. A `render` function which converts your state object to a "Virtual DOM", representing
   the HTML you want show.
3. An `eval` function which takes events as input, and which applies
   updates to your state object.

### A brief monad tutorial for Purescript

Purescript uses monads everywhere.  Think of a monad as an abstract
interface to a generic "container" which has a type parameter representing
the elements that it stores.  So imagine you have a type `List a`, which is
a generic type.  For example, you could have `List String` or `List
Number`. In this case, your monad is `List`.  A monad has three other
properties that we care about:

1. Given a monad `m` and a value of type `a`, we can create a value of type
   `m a`.  For example, given `List` and `"Hello"`, we can create
   `["Hello"]`.
2. Given a nested monad type, we can "flatten" it, converting `m (m a)`
   into `m a`.  For example, given the `List (List String)` value
   `\[["Hello"]]`, we can flatten it into the `List String` value
   `["Hello"]`.
3. Given a value of type `m a`, and a function that converts `a` to `b`, we can
   create a type `m b`.  For example, if we have the `List String` value
   `["a", "bb"]`, and a function `length` which computes the length of a
   string, we run `map length` and get the `List Number` value `[1, 2]`.

Now here's the fun part: You can visualize "a computation that returns a
`String`" as a container, if you squint at it correctly.  Check the rules
above:

1. Given a `String`, we can always create a computation that returns that
   string.
2. Given a computation that returns _another_ computation that finally
   returns a `String`, we can run the first computation, wait for it to
   spit out the second computation, run that, and finally get our `String`.
3. Given a computation that returns a `String`, and the function `length`,
   we can create a computation that returns the `length` of a string: Just
   run the computation, and call `length` on the output.

Anything which satisfies this interface—as well a few sanity-checks on how
the three properties interact—can be treated as a monad.  It's a pretty
powerful structure and it turns up everywhere.

Purescript has two computation monads:

- `Eff e` is a computation which has possible side effects specified by
  `e`.  For example, `Eff (console :: CONSOLE)` is a computation which is
  allowed to write things to the browser console.
- `Aff e` is an _asynchronous_ computation which has side effects.

Given a computation of type `Eff e`, you can convert it to a computation of
type `Aff e` using the `liftEff` function.  There are many more `lift`
functions for converting between different monads.

In this program, you'll also see the monad `ComponentDSL state query Aff`,
which is a wrapper around `Aff` that adds some extra machinery for updating
a component's state.

[node]: https://nodejs.org/
[purescript-halogen]: https://github.com/slamdata/purescript-halogen
[Purescript by Example]: https://leanpub.com/purescript/read
[React.js]: https://facebook.github.io/react/
