## Initial Frontend Configuration

I use rails. But I strip out all JS from the asset pipeline.

Instead, I create a frontend directory. I will use webpack to build
the JavaScript. I acn use `webpack --watch` to dump this in the public
directory. I modify the application layout to refer to this. I remove
the entire JS folder from the assets directory. I remove
`jquery-rails` from the Gemfile.

The main problem with this approach is that caching won't be done
properly, since I don't have webpack generate a fingerprint. I can
figure that out in the future.

I have to add `/public/bundle.js` to the `.gitignore` so that this
doesn't get checked in. I already have `node_modules` in my global
`.gitignore`.

I use babel to transpile and use the latest JS features. Therefore, my
`.babelrc` is:

```
{
  "plugins": [
    "transform-react-jsx",
    "transform-object-rest-spread"
  ],

  "presets": [
    "babel-preset-latest",
  ]
}
```

This turns on JSX, and the latest preset includes ES2017, which has
`async`. Note that rest spreading is actually still experimental (at
this time).

My `webpack.config.js` looks like so:

```
let path = require('path');

module.exports = {
  entry: [
    'babel-polyfill',
    './src/application.es7',
  ],

  // The fuck is the point of nesting this under "module"?
  module: {
    loaders: [
      {
        test: /\.es7$/,
        exclude: /node_modules/,
        loader: 'babel-loader',
      }
    ],
  },

  output: {
    path: path.join(__dirname, '..', 'public'),
    filename: 'bundle.js',
  },
}
```

I need `babel-polyfill` to support old browsers. I will use the
extension `es7` for my JS files; Webpack will use Babel to transpile
just these ES7 files, and no files from `node_modules`, which would be
very slow. As you can see, the output is in Rails' public directory.

The application entrypoint is `./src/application.es7`.

I use NPM to install packages in frontend. My node_modules is nested
in frontend. Here are the dependencies I've used thus far:

    // babel-core is the core transpiler. The loader is the shim for
    // webpack. Plugins for object rest spreading and JSX. Polyfill
    // for the browser. Preset-latest for all the latest features.
    "babel-core": "^6.23.1",
    "babel-loader": "^6.3.2",
    "babel-plugin-transform-object-rest-spread": "^6.23.0",
    "babel-plugin-transform-react-jsx": "^6.23.0",
    "babel-polyfill": "^6.23.0",
    "babel-preset-latest": "^6.22.0",
    "webpack": "^2.2.1"

All my own code will go in `frontend/src`. Here is
`application.es7`. Right now it is very simple:

```
import jQuery from 'jquery';
import { components } from './components.es7'

window.$ = window.jQuery = jQuery;

window.App = {
  components
}
```

This just exposes jQuery and an App namespace to the
window. Therefore, I install also:

    "jquery": "^3.1.1",

## Rails Side

I keep sass-rails, but strip out CoffeeScript and Turbolinks. I also
remove jquery-rails. Mostly I'll just be using JBuilder.

I do bring in Bootstrap. I add this to the gemfile:

    gem 'bootstrap', '~> 4.0.0.alpha6'

This gives me a SASS version of bootstrap 4. It is necessary to rename
`application.css` to `application.scss`. You can strip everything in
here and leave `@import "bootstrap"`.

As mentioned, I remove the entire JS directory. I'm not sure what I'll
do for Bootstrap's JS code, if I need it, but I'll consider that
later...

It's important to go to the application layout and remove the
reference to the JS file. You add a script tag:

    <script src="/bundle.js"></script>
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">

I've included this meta tag as well, which you need for devices.

I used slim templates on a prior project, and I sort of like it. ERB
can get clunky. I think I will continue to try this out.

## React

We start to bring in React. We NPM install:

    "react": "^15.4.2",
    "react-dom": "^15.4.2",

We create a `frontend/src/components` directory, as well as a
`frontend/src/components.es7` file that does imports for us. Here is
an example (with just one component right now):

```
import { Root } from './components/root.es7'

const components = {
  Root: Root
};

export { components };
```

I don't use default export. Babel changed how it does default
imports. So even if I only export one thing, I will export via an
object, and the user will have to destructure it. My `application.es7`
code above shows how. Note that if you did want everything you can say
`import * as MyLib from "my-lib"`.

Here is a basic component:

```
import React from 'react';
import ReactDOM from 'react-dom';

let Root = class extends React.PureComponent {
  render() {
    return <div>
      <p>We can be best friends!</p>
    </div>;
  }
}

Object.assign(Root, {
  render(id) {
    const rootEl = $(id)[0];
    ReactDOM.render(
      <Root />, rootEl
    );
  }
});

export { Root };
```

Note the static render method. This is invoked from the Rails view:

```
/ This is root/root.html.slim
div id="application"

javascript:
  App.components.Root.render("#application")
```

I will eventually need to do something more sophisticated when I start
using a client-side router. But this is simple enough if you just want
to mount a React component.

## Redux

Now we want the app to do things. Ugh. To manage the state, I will use
`redux`. `react-redux` is the shim between Redux and React. Redux
offers a convenience library called `redux-actions`. I use `immutable`
for state, since Redux state ought to be immutable anyway.

    "immutable": "^3.8.1",
    "react-redux": "^5.0.2",
    "redux": "^3.6.0",
    "redux-actions": "^1.2.1",

The first thing to do is create folders for `actions` and `reducers`,
as well as `.es7` files that import those contents.

**Actions**

Here's an example of an actions file `actions/schedule_items.es7`:

```
import { createAction } from 'redux-actions';

const fetch = createAction("FETCH_SCHEDULE_ITEMS");
const update = createAction("UPDATE_SCHEDULE_ITEMS");

const actions = {
  fetch,
  update
}

export { actions };
```

Here is the corresponding `actions.es7`:

```
import { actions as scheduleItemsActions } from './actions/schedule_items.es7'

const actions = {
  scheduleItems: scheduleItemsActions,
};

export { actions };
```

**Reducers**

Next up is the reducer. Here is `reducers/schedule_items.es7`:

```
import I from 'immutable';
import { handleActions } from 'redux-actions';

const reducer = handleActions({
  FETCH_SCHEDULE_ITEMS(state, action) {
    console.log("WOULD TRY TO FETCH");
  },
  UPDATE_SCHEDULE_ITEMS(state, action) {
    console.log("WOULD TRY TO UPDATE");
  },
}, I.List([]));

export { reducer };
```

Notice the use of `immutable`. I like this for Redux state, which
shouldn't be mutated anyway. I use this here to start out with an
initial value. Notice the use of `handleActions`, which makes things
quite a bit simpler.

I leave out `reducers.es7` which mirrors `actions.es7`.

**Store**

As there can be only one store, I create it in `store.es7`:

```
import { combineReducers, createStore } from 'redux';
import { reducers } from './reducers.es7'

const store = createStore(combineReducers({
  scheduleItems: reducers.scheduleItems
}));

export { store };
```

Notice the use of `combineReducers` from `redux`, which is helpful.

**React Redux**

So now we need to make our component talk with Redux. This is where we
use `react-redux`.

```
import $ from 'jquery';
import React from 'react';
import { connect, Provider } from 'react-redux';
import ReactDOM from 'react-dom';

import { actions } from '../actions.es7'
import { store } from '../store.es7';

let Root = class extends React.PureComponent {
  render() {
    return <div>
      <button onClick={() => this.props.fetchScheduleItems()}>
        Click me!
      </button>
      <p>We can be best friends!</p>
    </div>;
  }
}

Root = connect(
  (state) => ({ state }),
  (dispatch) => ({
    fetchScheduleItems() {
      dispatch(actions.scheduleItems.fetch());
    }
  })
)(Root);

Object.assign(Root, {
  render(id) {
    const rootEl = $(id)[0];
    ReactDOM.render(
      <Provider store={store}>
        <Root />
      </Provider>,
      rootEl
    );
  }
});

export { Root };
```

Let's look at a few things. First note that I've used `let` to assign
a variable to my class. This is because I need to use `connect` (from
react-redux) to create a wrapper-class. The wrapper class provides
properties to the component. In particular, the first function
provides a property `this.props.state`; that exposes the entire state,
which is unnecessary; usually you will filter down to just the
relevant state.

Next, there is a `dispatch` function which exposes action dispatch to
the component. Above, I expose a property
`this.props.fetchScheduleItems` which is used to dispatch this action.

The wrapped Root component can only be instantiated in the context of
a `Provider`. This wrapping is done quite simply in `Root::render`.

## TODO

* Routing
* Async actions via redux-saga.

  "dependencies": {
    "redux-saga": "^0.14.3",
  }
