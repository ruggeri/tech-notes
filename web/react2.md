I recently have been building a new Rails/React app.

I used Rails. I disabled asset pipeline for JS. I created a frontend
directory for my JS.

I used webpack to build my JS. I currently have `webpack --watch` just
dump this in the public directory. That's not a long term solution.

I use babel to use es7 features via `babel-preset-latest`.I also need
Babel for JSX.

I have a top-level `src/application.es7` file. In here I do things
like import jQuery and set this on the window for global use. I also
create an `App` object, which I set on the window. The
`application.es7` is responsible for importing all top-level parts of
the app to export via the App object.

I am using `react`. I definte a component. The Rails view renders it
by creating an empty `div` with an appropriate id. Then it calls a
static `render` method on the component, which finds that div and
renders into it. (NB: the Rails code could probably *pass* the id into
the render method, decoupling a hidden dependency...). I use
`react-dom` to do that rendering, of course.

I use `redux` to manage the state of the application. The state is
an `immutable` data structure.

Here are the steps:

0. Create an initial state.
1. Create an action. I use `createAction` from `redux-actions` to do
   this. It is very simple and doesn't currently do that much for me.
2. Write a reducer function. This takes the state (defaults to an
   initial state) and an action. Here you do your switch statement. Or
   you can use `handleActions` from `redux-actions` which is
   convenient.
3. Write your component assuming that you have the necessary props
   from the state. Assume you have whatever change handlers are
   necessary.
4. Now we use `connect` from `react-redux` to create a wrapper class
   for your component. This will take (a) a store state to props
   function and (b) a dispatch to onChange function. The first
   projects the state down to something convenient for the
   component. The second limits the actions that can be sent to the
   store. It is convenient to use `bindActionCreators` from `redux`.
5. Last is to create that static render method. You need to wrap the
   wrapped component with a `Provider` (from `react-redux`).

Note this doesn't discuss any routing right now. And I don't have any
async actions at the moment.

# Libraries

* Webpack, Babel.
* React.
* redux (26k watchers), react-redux (5.5k watchers)
    * NB: redux is agnostic of react, thus react-redux.
* redux-actions (2.4k stars): just a little convenience for action
  construction. It looks like `handleActions` will produce a reducer
  for you, which is nice.
* redux-form (4.6k stars). Pretty nice. Does exactly what I need. Does
  reduce some boilerplate. Especially good at nested stuff.

I haven't used this again yet:

1. redux-saga (5.3k watchers) vs redux-thunk (3.7k watchers)
    * But redux-saga especially crushes in # of contributors, forks,
      and stars.
2. redux-immutable: Sounds useful since I also like immutable.js.
    * Should be really fast to review.

* They also mention `normalizr` as a way to store data locally. This
  normalizes responses from the server.
    * It feels like the Redux people push this *very* hard.
    * There's also a much less popular redux-orm library.
    * Maybe that's worth considering since this is a very important
      subject.
* react-router (18k watchers), react-router-redux (5k watchers)
* relay (not sure this is really used; 7.5k watchers)
* reselect (4.9k watchers) (used to memoize computations)
* They mentioned a middleware for logging dispatched actions, which
  sounds very useful for debugging.
* Tools:
    * A couple of these could be pretty helpful.
    * redux-devtools, redux-devtools-extension
    * redux-logger (redux website called this out as useful; logs all
      actions received)
* Other:
    * These are small, not very important.
    * redux-persist (for persisting/hydrating from localstorage)
    * redux-auth
    * redux-undo
    * redux-search (helps with client side searching)

## TODO

* <del>Read docs for React (again)</del>
    * Nothing very exciting. Nice to know that immutable is preferred.
    * Nice to be able to use PureComponent everywhere.
* <del>Read docs for redux, redux-actions.</del>
    * Does redux Provider actually pass store down without callbacks?
    * The docs for redux are written in a pro redux-thunk era.
    * But I'll save this discussion for when I cover
      redux-thunk/redux-saga.
    * They describe lots of ways to split up reducers into parts and
      to combine these. But I can come back to that another
      time. Fundamentally, reducers are just pure functions, so you
      can compose and combine them however makes sense to you.
    * They also push normalizr, but I feel like I should just read
      those docs. That sounds like something for a SPA with complex
      nested state and all.
* <del>Read docs for react-redux</del>
* Read redux-form docs.
