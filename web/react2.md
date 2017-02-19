**I'm trying to rewrite this as react3.md which is more comprehensive
  and explains things deeper. Also I have more experience now so
  hopefully the writeup is better.**

# Libraries I've Used

* redux-form (4.6k stars). Pretty nice. Does exactly what I need. Does
  reduce some boilerplate. Especially good at nested stuff.
* classnames (4.2k stars)
    * Very simple utility for generating className attribute from an
      object.

## Priority Libraries

0. react-select
1. redux-saga (5.3k watchers) vs redux-thunk (3.7k watchers)
    * But redux-saga especially crushes in # of contributors, forks,
      and stars.
2. redux-immutable: Sounds useful since I also like immutable.js.
    * Should be really fast to review.

## Next Libraries to Review

* https://github.com/Hacker0x01/react-datepicker
* react-router (18k watchers), react-router-redux (5k watchers)
* reselect (4.9k watchers) (used to memoize computations)

## Less Important

* They also mention `normalizr` as a way to store data locally. This
  normalizes responses from the server.
    * It feels like the Redux people push this *very* hard.
    * There's also a much less popular redux-orm library.
    * Maybe that's worth considering since this is a very important
      subject.

## Developer Experience

* Tools:
    * A couple of these could be pretty helpful.
    * react-devtools, redux-devtools, redux-devtools-extension
    * redux-logger (redux website called this out as useful; logs all
      actions received)

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

## Other, Unimportant Awesome React Libraries

This is a comprehensive list (minus what's covered above). But these
aren't important to know.

* Forms
  * https://github.com/text-mask/text-mask
* charts:
  * https://github.com/reactjs/react-chartjs
  * https://github.com/esbullington/react-d3
* Maps:
  * https://github.com/tomchentw/react-google-maps
  * https://github.com/uber/react-map-gl
  * https://github.com/istarkov/google-map-react
* File upload: https://github.com/okonet/react-dropzone
* Tables/Datagrid
  * https://github.com/glittershark/reactable
  * https://github.com/adazzle/react-data-grid
  * https://github.com/GriddleGriddle/Griddle/
* Dragging: https://github.com/gaearon/react-dnd
* Elasticsearch: https://github.com/searchkit/searchkit
* Canvas: https://github.com/Flipboard/react-canvas
* Other:
    * These are small, not very important.
    * redux-persist (for persisting/hydrating from localstorage)
    * redux-auth
    * redux-undo
    * redux-search (helps with client side searching)
