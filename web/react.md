## Stack

* React
* Babel (for JSX) + Webpack
    * They mostly just create a `frontend` directory in the Rails
      root, and have webpack build to app/assets/javascripts.
    * That seems like it will be weird with the assets pipeline...
* Redux, react-redux, redux-thunk
* React-router, react-router-redux
* Less vital:
    * reselect
    * redux-actions

## React Notes

* Split into React and ReactDOM
    * ReactDOM has `ReactDOM.render`.
* You use JSX to build React virtual DOM, which then gets diffed and
  then re-rendered. Because changes in VDOM are small, only those
  changes are done in the browser, so it can be fast.
    * BTW, the diffing algorithm is takes advantage of the HTML
      structure to be a lot more efficient than a text diff.
    * That's how it can be fast.
* Definte new components using `React.createClass`
    * `#render` is the most important method.
    * `#getInitialState` sets initial state associated with the
      component. This is *not* global information, or model
      information. This is internal state that no one outside the
      component needs to know.
    * `#setState` updates this, telling the component to re-render.
* You can use `onClick` virtual attribute to give a handler. Methods
  will be "autobound". `onChange` is another useful one.
    * Because text fields will be re-rendered, you need to update the
      `value` as the text field changes.
* `#componentDidMount` and `#componentWillUnmount` are hooks for
  installing/removing timers
* `props` are the way that parameters are passed down into a
  component. They're basically those arguments you would construct the
  component with.
    * You can sometimes pass callbacks down, but often you'll want to
      use Flux...
* Because arrays of things can be confusing (for instance, when
  re-sorted), you use a `key` attribute so that React can understand.
    * Otherwise, it might not know which items were destroyed, or
      changed. State might get weird.
* `this.props.children` consists of nested JSX. It's kind of like
  `transclude` in Angular.
* `#shouldComponentUpdate` is the way you can do a little
  optimization, if you really are concerned about re-rendering
  everything.
    * There's a `PureRenderMixin` that just shallow compares
      props/state to decide whether re-render is needed.
* Can use `ref` to give an element a name, you can access the DOM node
  with `this.ref.myRefName`.
* There appears to be a new, experimental `context` idea; basically,
  it lets you set something in an ancestor, and access it in a deep
  descendent.
    * Seems like it possibly addresses some problems.

## Lifecycle

* `#getInitialState`: sets initial state.
* `#componentWillMount`: before mounting, why use this? Because you
  can set state here and render will see it...
* `#componentDidMount`: any initialization needing refs can happen
  here.
* `#componentWillReceiveProps`: called when new props are
  received. Why is this useful? Again, you can set state here in
  reaction to new props...
* `#shouldComponentUpdate`: lets you say whether you think the
  component needs to be re-rendered given the newProps and newState.
* `#componentWillUpdate`/`#componentDidUpdate`: why?
* `#componentWillUnmount`: clean up listeners.

## Architecture

* Simplest way is if *all* state lives at the top-most level of the
  application. Then you just have a listener that updates your state
  on any change to anything at all.

## Redux

* Redux is very easy. You write functions (reducers), which take in
  current store value and an action. They return the new store value.
    * Basically your reducer is a big switch on the type of the
      action, and returns the result of applying that action to the
      current store value.
    * The types are string constants. You typically put them in a
      `constants/actionTypes.js` file.
    * You can have an `actions` folder, where an *action creator*
      function will create an action (type + payload). This is a
      convenience for the user. E.g., `addTodo(text) => { type:
      ADD_TODO, text: text, done: false }`.
    * You split up your reducer into several
      functions. `combineReducers` is a helper that lets you define a
      reducer for each part of the state, applying each reducer fn to
      each part.
* Your application state can have both model and UI data.
    * Though keep them separate.
    * They recommend normalized (no-nesting) for data. Like a
      DB. *Exactly* like I've been saying for so *fucking* long.
    * Apparently `normalizr` is a library used to help with this.
* You turn this into a "store" via `redux.createStore(reducerFn)`. The
  store is what has the `#subscribe` and `#dispatch` methods.
    * You should have *exactly one* store.
    * You can always combine several reducers into one big reducer.
    * Subscribers can call `#getState` to get the updated state!
* It is natural to use ImmutableJS, since the reducers must be pure
  anyway. But you don't have to; you can even mutate the state if you
  want to, but that's an anti-pattern, for sure.
* To reduce boilerplate, you can use `redux-actions` to generate
  action creators for you.

Okay, so how do you use it with React?

* They recommend "container" components vs "presentational"
  components.
    * Basically, only the container should be aware of Redux.
    * They say that presentational components should just read their
      props, and invoke callbacks to change data.
    * Containers subscribe to Redux state, calling `#setState` in
      response. They dispatch to the store to change data.
    * Containers are often top level or route handlers.
* `react-redux` provides a `Provider` component. This lets you specify
  a context, `store={store}`.
    * Wrap your app in this Provider.
    * Components inside that are "container" components use the
      `connect` helper.
    * You specify a `mapStateToProps` method to map the state of the
      context store to props for the component.
    * You specify a `mapDispatchToProps` method, that takes in the
      dispatch method of the context store and returns methods that
      dispatch actions to the store.
        * It's common to use `bindActionCreators` here.

## Redux Async

* You use the `redux-thunk` middleware. This basically lets you hand
  thunks to `dispatch`. Dispatch will call the thunk, which can do
  AJAX work. When the work completes, you can call the dispatch method
  passed in to the thunk. You also get `getState` passed in too.
* You can chain thunks like this, which creates an async workflow.
* It's common to return a promise from the thunk, so that people can
  wait.
* I guess thunks are pure; the side-effect doesn't happen until the
  thunk is executed. But I'm not sure I totally understand the point
  here. Why doesn't the client just do the async work, and fire a
  message when done?

## Reselect

* Sometimes you need a computed property from the state. Every time
  the state changes at all, this property must be entirely recomputed,
  even if we can know it hasn't changed.
* reselect lets you memoize computation of computed values.
* Not essential at this moment...

## Routing

* TODO

## TODO

* Read the redux examples!
* https://github.com/rackt/react-router-redux
* https://egghead.io/series/getting-started-with-redux

## Sample Projects

* Gmail (Groups?)
* Facebook
* Twitter
* AirBnb
* Instagram
* Pinterest
* eBay
* Amazon
