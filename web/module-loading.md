## Node Module Loading

There are no global vars in Node; when you `require` a source file, its
`module.exports` is what is returned. As a (fake) convenience, a
variable `exports` is default set to `module.exports`, itself default
set to `{}`. To prevent confusion, you might:

    module.exports = exports = "I WANT TO EXPORT THIS";

But strictly speaking it doesn't matter what you set to `exports` since
it is `module.exports` that matters, not `exports` itself.

To collect up a bunch of smaller files in a directory, you can use a
`lib/index.js` file. This can require and re-export a bunch of files
that make up that library under a shared namespace.

In fact, `require("./utils")` will look for:

```
./utils.js
./utils.json
./utils.node
./utils/package.json main
./utils/index.js
```

Node will assume that `require("utils")` means a *package*. It will look
for a builtin or in `node_modules`.

The idea of `module.exports` comes from **CommonJS**, which was an early
effort to standardize a server-side JS environment. CommonJS has been
obsolesced by Node, which took some of its ideas and came to dominate.

## Packaging For Client-Side

Of course, the browser has no notion of `module.exports`. Common
solutions are:

- Webpack (11,694 stars)
- node-browserify (obsolete as of 2026)
- requireJS (obsolete as of 2026)

Indeed, it seems like webpack is considered the most modern solution.

## ES6 Modules

See my `js-features.md` for a description of how ES modules work. ES
modules defines a *syntax*, but resolution/how importation works depends
on the environment.

- Node cares about exact file extension, doesn't load from `NODE_PATH`.
  - Tends to be stricter than old CommonJS import style.
  - Node doesn't allow "directory" imports via `import`. You used to be
    able to `require("dirname")` and it would consult an `index.js`.
  - If you want to use an `index.js`, you must `import
    "dirname/index.js"`
  - BUT, when you are importing a *package*, you can `import react from "react"`.
    - Here, I guess Node knows its a package because it's not a relative
      path. It will import from `node_modules`.
    - Node will consult the `package.json`, which will map paths in the
      package to javascript files. For instance, this allows
      `import x from "package/subpath`:

```json
{
  "type": "module",
  "main": "./dist/index.js",
  "exports": {
    ".": "./dist/index.js",
    "./subpath": "./dist/subpath.js"
  }
}
```

- Browser loads exact URL. Can't import a whole library by name like
  `import "react"` unless a bundler or source map is used
  - Bundlers like `vite`, `webpack`, or `rollup` will take over
    resolution if used.

## RequireJS

**Obsolete as of 2026**

RequireJS is primarily focused on AMD: asynchronous module
definition. It existed before node; it is a way to asynchronously load
javascript modules.

Basically, you load `require.js` in a script tag, specifying a
`data-main` attribute which is the first module to load. You use
`requirejs(["lib1", "lib2"], function (lib1, lib2) { ... })` to
express dependencies. They will be async fetched, then passed into
your callback.

Modules are defined like so:

```
define(["dep1", "dep2"], function (dep1, dep2) {
  // Setup work.

  return exportedModule;
});
```

Obviously RequireJS/AMD is not CommonJS/Node. In particular, AMD
allows asynchronous requires. There are shims that let you require a
library via RequireJS *or* Node. But you have to support two
ecosystems this way...

One such "shim" idea was "UMD" (Universal Module Definition). It worked
something like this:

```js
if (typeof define === "function" && define.amd) {
  define(...)
} else if (typeof module === "object" && module.exports) {
  module.exports = ...
} else {
  window.MyLibrary = ...
}
```

## Webpack

**As of 2026 still exists and is used. But vite appears to be a more
common first choice in 2026. And Next.js use often means you don't need
webpack or Vite directly.**

Webpack tries to be more flexible than Browserify and AMD, each of
which (by default) adapt one pole of the "one JS file" and "many async
JS files" axis. One JS file means you load stuff you don't yet need,
while many async JS files means you have additional latency.

Webpack is like browserify with multiple output files.

Webpack can handle both CommonJS and AMD formats out of the box.

## Browserify

**Obsolete as of 2026**

Browserify basically tries to make your Node stuff work on the
browser. It scans your node files, and inlines JS when it sees a
`require`. It builds a big file that can be used. It also provides
browser versions of some core Node libraries (url, util, buffer,
etc.).

You can use it with a watch task to keep recompiling your JS.

Should be able to build *source maps*, since your JS is going to be
fucked up a little because the requires are turned into a chain of
function calls.
