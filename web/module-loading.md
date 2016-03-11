## Node Module Loading

There are no global vars in Node; when you `require` a source file,
its `module.exports` is what is returned. As a (fake) convenience, a
variable `exports` is default set to `module.exports`. To prevent
confusion, you might:

    module.exports = exports = "I WANT TO EXPORT THIS";

But strictly speaking it doesn't matter since it is `module.exports`
that matters, not `exports` itself.

To collect up a bunch of smaller files in a directory, you can use a
`lib/index.js` file. This can require and re-export a bunch of files
that make up that library under a shared namespace.

The idea of `module.exports` comes from CommonJS, which was an early
effort to standardize a server-side JS environment. CommonJS has been
obsolesced by Node, which took some of its ideas and came to dominate.

## Packaging For Client-Side

Of course, the browser has no notion of `module.exports`. Common
solutions are:

* Webpack (11,694 stars)
* node-browserify (9,056 stars)
* requireJS (8,720 stars)

Indeed, it seems like webpack is considered the most modern solution.

## RequireJS

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

## Browserify

Browserify basically tries to make your Node stuff work on the
browser. It scans your node files, and inlines JS when it sees a
`require`. It builds a big file that can be used. It also provides
browser versions of some core Node libraries (url, util, buffer,
etc.).

You can use it with a watch task to keep recompiling your JS.

Should be able to build *source maps*, since your JS is going to be
fucked up a little because the requires are turned into a chain of
function calls.

## ES6 Modules

This will standardize module loading, but it is hardly in the wild at
all yet: few systems support, and few modules are written this way.

## Webpack

Webpack tries to be more flexible than Browserify and AMD, each of
which (by default) adapt one pole of the "one JS file" and "many async
JS files" axis. One JS file means you load stuff you don't yet need,
while many async JS files means you have additional latency.

Webpack is like browserify with multiple output files.

Webpack can handle both CommonJS and AMD formats out of the box.

It looks like webpack is becoming a new default?
