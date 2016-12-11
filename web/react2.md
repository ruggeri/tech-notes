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
* react-router (18k watchers), react-router-redux (5k watchers)

I haven't used this again yet:

* relay (not sure this is really used; 7.5k watchers)
* redux-form (4.5k watchers)
* redux-saga (5.3k watchers) vs redux-thunk (3.7k watchers)
    * But redux-saga especially crushes in # of contributors, forks,
      and stars.
* reselect (4.9k watchers)
