## ECMAScript 5

* Bunch of `Array` prototype methods (`filter`, `map`, etc).
* `Function#bind`.
* `JSON#serialize`.
* Object getters/setters.
    * These are called "properties", and you also get an API to
      dynamically add them (`Object::defineProperty`)
* `Object` helpers:
    * `Object#create`, `Object#getPrototypeOf`.
    * `Object#freeze`. `Object#seal` allows you to *re-assign*
      existing properties, but not add/delete
      properties. `Object#preventExtensions` just means you can't add
      new properties.
* `Function#apply` permits "array-likes" so you don't need to do that
  old trick of copying `arguments`.
* There's more pertaining to enumerable properties, which I don't care
  about, of course.

## ECMAScript 6

* Syntactical Sugar
    * Default args
    * Rest parameters, Spread operator
    * Object literal shorthands
    * Destructuring
    * Arrow functions
* Classes and Inheritance
* Const and Let
* Promises, Generators, Iteration
* Template Strings
* Modules
* Map, Set, WeakMap/WeakSet, Symbol
* And more!

**Syntactical Sugar**

* Default arguments `function (x = 1) { return x }`
* Rest parameters `function (x, ...rest) { return rest }`
* Spread operator `Math.max(...[1, 2 ,3])`
    * Can also be used inside arrays `[1, ...[2,3,4], 5]`
    * Or generators or anything iterable. Okay...
* Object Literal Extensions:
    * Computed properties `var propName = "xyz"; ({ [propName]: 123}).xyz === 123`.
        * Also works with shorthand methods, getters/setters methods.
    * Shorthand properties: `var x = 1, y = 2; ({ x, y }).y === 2`.
    * Shorthand methods `({ g() { return 5 } }).g()`.
* Destructuring
    * `var [first, ...] = array`
    * `var { x, y: yVar } = { x: 1, y: 2 }`
    * Can even use computed properties here! `{ [propName]: varName }
      = ...`.
    * Destructuring nests: `var [first, [innerFirst]] = [1, [2, 3], 4]`
    * Can have defaults and rest, too.
    * Can do this for arguments, like to pull out options.
* Arrow functions
    * Not just a fast way to write a function, but also bind to the
      this in scope at the time of the function defintion.
    * This cannot be changed via bind, apply or whatnot. It is frozen!

**Classes and Inheritance**

* Class
    * `class X { constructor(arg1, arg2) { }, methods... }`
    * `class Dog extends Animal { ... }`
    * You can even call super methods properly with `super`.

**Const and Let**

* const and let
    * const just keeps you from reassigning the variable. The object
      stored is not immutable though!
    * let respects block scope. It's too prevent silly errors where
      you think you're creating a new variable inside a code block,
      but actually you're just modifying an outer variable.

**Promises, Generators, Iteration**

* Promise
    * You pass in a function which is handed the promise's
      resolve/reject methods. This function is executed, and should
      call one of these methods when it knows the answer.
    * You can chain via `Promise#then(onFulfilled, on Rejected)` and
      `Promise#catch`.
    * There are `Promise::all` and `Promise::race` as primitive
      support for fanning out operations.
    * BTW, any exception raised in promise code causes that promise to
      be rejected!
* Generators
    * They can yield at several points.
    * You call them the first time. This gives you an iterator. Then
      you call `next` repeatedly. It returns `{ value, done }`.
    * You can pass arguments into `next`.
    * You can do shorthand generators for objects/classes.
    * You can get a bullshit kind of "await" this way by yielding
      promises.
    * When you hit `return` (or end of func) the generator is done.
* `for (var x of collection)`
    * Different from `for..in` loops. Works for *iterable* things.
    * To be iterable, you need a `{ [Symbol.iterator]: function* () {}
      }` method. Typical to use a generator here.
    * Iterators have the same interface as generators. So the easiest
      way to write an iterator is to just use a generator
      function. However, this is not *required*.

**Template Strings**

* Templates
    * Use backticks to escape a template literal.
    * You can use `${...}` inside to call JS code.
    * You can embed newlines!

**Modules**

* You can use `export` to export either multiple names, or a single
  default object (or both!).
* Typical way to import is `import ModuleName from
  "path/to/moduleName.js"`.
    * Kind of like `import math` in Python.
* Could also do `import * as ModuleName from "moduleName.js"`
    * That imports all exports and collects them under a module name.
    * Kind of like `from math import *` in Python.
* If you just want some members, you can do `import { sin } from
  "math.js"`.
    * Can even give these aliases if you want.
    * Can import as much as you want.
* You can also just `import "math.js"`, which won't import any
  bindings, but just do this for side-effect. Basically useless.

**Map, Set, WeakMap/WeakSet, Symbol**

* `Map` and `Set`
    * But these use the standard `===` to store items.
    * I guess it's slightly better than using an object since won't
      convert to strings.
    * I guess you could easily write your own on top of these.
    * Sounds like more sophisticated value comparison has been kicked
      down the road for another day.
    * There are weak versions of these.
* Symbols
    * Used for unique identifiers.
    * Can give them an identifier for debugging.

**Other**

* TypedArrays
    * For ASM, presumably.
* Proxy objects where you can intercept method calls/access before
  they hit the regular object.
    * This allows a form of decoration of an object, I guess?
* `String#startsWith`; this is a simple convenience for a common
  operation.
* `Array.from` eliminates need to do
  `Array.prototype.slice.call(arguments)`.
* `Array.find` takes a predicate function, another convenience.
* Added a bunch of hardcore math methods like `Math.sinh`.
* `Reflect` global.
    * This is presumably for metaprogramming, but it feels like
      everything `Reflect` does can already be done in other ways.
    * JavaScript already allowed for so much dynamism, I'm not sure
      what this is buying you. Maybe just a convenient API?

## Next EcmaScript Revision

* Async, await is the only major feature being talked about right now.

## Source

http://kangax.github.io/compat-table/es5/
https://github.com/lukehoban/es6features
