## Server Side

Setup is much like react3.md.

I am using Koa. I use some simple middleware like koa-mount,
koa-router, koa-static, koa-views. For simplicity I'm just using Pug
templates, though maybe I'll regret that later down the road just like
I don't like HAML.

I haven't organized my routes yet, but I'll probably eventually make a
routers folder. I already have a views and a static folder.

I've setup Sequelize. You use the CLI to initialize a configuration
file; you can finally call this what you want! There's a simple
migrations directory. It creates a models directory for you, and an
index.js to load your models. Your models need to export a function
with the signature `(sequelize, DataTypes) => Model`. I just set
`app.context.models` for simplicity in Koa.

The other half is of course the frontend. I use Webpack, plus Babel to
do the JSX translation. Here is my `.babelrc`:

```
{
  "plugins": ["transform-react-jsx"],
}
```

Here is my webpack.config.js

```
let path = require('path');

module.exports = {
  entry: [
    'babel-polyfill',
    './frontend/app.js',
  ],

  // The fuck is the point of nesting this under "module"?
  module: {
    loaders: [
      {
        test: /(\.js|\.jsx)/,
        exclude: /node_modules/,
        loader: 'babel-loader',
      }
    ],
  },

  output: {
    path: path.join(__dirname, 'static'),
    filename: 'bundle.js',
  },

  devtool: 'source-map',
};
```

I think babel-polyfill might not be needed if I just use
Chrome. Notice that I save the bundle to a static folder. I also
generate a source map, which is working really well!

## Frontend Side

I have a simple `store/store.js` file which basically just does
`createStore` and `applyMiddleware` for `redux-thunk`. That uses the
root reducer (more later).

In `app.js`, I instantiate the store, and I import `React` so that I
can render an `AppRoot` element into the document after load.

`AppRoot` is a component. I have a components directory. It uses
`Provider` from `react-redux` so that my components can access the
store.

In component files, I both define the component, and then at the end I
import `connect` from `react-redux` to perform the wrapping.

I have a reducers directory. My current plan is to just have "tables":
just store maps of `recordName => id => object` for each of the object
types. I use immutable-js as the datatype for stores. The individual
reducers are very simple thus far.

I have an actions folder. I define constants, but also action creator
functions. These are very simple. The only more complex part is
actions that talk to the backend. These use `fetch`. Then they
dispatch the received results so the store can be updated.

I tried to use Ramda, but the problem is that you just don't get nice
datastructures. The sad thing is that Immutable itself isn't really
truly feature complete: there's a lot of functionality in Ramda that
would be nice to have. But for every one thing that could be nicer in
Immutable, there are two things which just plain suck in Ramda.

I've started to use my approach of just keeping tables and maintaining
some indexes as the store. It's okay, but performing a join can be a
pain. Maybe I can factor this kind of code out into a helper later...

I used `sass-loader`, `css-loader`, `style-loader` as well as
`node-sass` to be able to build a seperate bundle for my
stylesheets. I think what this does is actually put your styles inside
a JS file? And that injects a style tag? Weird.

Then to import the Bootstrap styles, I first npm installed
`bootstral@4.0.0-beta`. Then I also needed to add `@import
"~bootstrap/scss/bootstrap;`. But then I was good to go!

Font Awesome wasn't hard to set up. Glyphicon is no longer packaged
with Bootstrap. I created a `style.js` to include my `style.scss` and
also `font-awesome/css/font-awesome.css`.

Next is how to configure Webpack. I needed to add the following:

```
      {
        test: /\.woff2?$|\.ttf$|\.eot$|\.svg$/,
        loader: "file-loader",
        options: { publicPath: "/static/build/" }
      }
```

This means that all font files should be served as regular files. But
we need to give the Koa static public path so that this can be
preppended. BTW, I moved everything into `static/build` to distinguish
it from unbuilt assets that I might check in.

To batch up actions I use `redux-batched-actions`.

It is helpful in the webpack config to add:

```
    modules: [
      path.resolve('./frontend'),
      path.resolve('./node_modules'),
    ],
```

This lets you use relative paths in the JSX imports.

## TODO

My next question is about GraphQL and also maybe Relay. GraphQL does
look kind of nice, but I don't see that I need it at present...
