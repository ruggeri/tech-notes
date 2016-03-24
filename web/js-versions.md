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
  old trick.
* There's more pertaining to enumerable properties, which I don't care
  about, of course.

## ECMAScript 6

* Short Summary
    * Default arguments
    * Spread/Rest parameters
    * Destructuring, property/method shorthands
    * const/let
    * String templates
    * Arrow functions
    * Class, extends
    * Generators
    * Promises
    * Map, Set, Symbol

* Default arguments `function (x = 1) { return x }`
* Rest parameters `function (x, ...rest) { return rest }`
* Spread operator `Math.max(...[1, 2 ,3])`
    * Can also be used inside arrays `[1, ...[2,3,4], 5]`
    * Or generators or anything iterable. Okay...
* Object Literal Extensions:
    * Computed properties `var propName = "xyz"; ({ [propName]: 123}).xyz === 123`.
        * Also works with shorthand methods, getters/setters
    * Shorthand properties: `var x = 1, y = 2; ({ x, y }).y ===2`.
    * Shorthand methods `({ g() { return 5 } }).g()`.
* `for (var x of collection)`
    * Different from `for..in` loops. Works for *iterable* things.
    * Gets an iterator, and uses that.
* Templates
    * Use backticks to escape a template literal.
    * You can use `${...}` inside to call JS code.
    * You can embed newlines!
* Destructuring
    * `[first, ...] = array`
    * `{ x, y: yVar } = { x: 1, y: 2 }`
    * Can even use computed properties here! `{ [propName]: varName }
      = ...`.
    * This shit nests!
    * Can have defaults and rest, too.
    * Can do this for arguments, like to pull out options.
* const and let
    * const just keeps you from reassigning the variable. It is not
      immutable.
    * let respects block scope. It's too prevent silly errors where
      you think you're creating a new variable inside a code block,
      but actually you're just modifying an outer variable.
* Arrow functions
    * Not just a fast way to write a function, but also bind to the
      this in scope at the time of the function defintion.
    * This cannot be changed via bind, apply or whatnot. It is frozen!
* Class
    * `class X { constructor(arg1, arg2) { }, methods... }`
    * `class Dog extends Animal { ... }`
    * You can call the super method properly with `super`.
    * Beca
* `Map` and `Set`
    * But these use the standard `===` to store items.
    * I guess it's slightly better than using an object since won't
      convert to strings.
    * I guess you could easily write your own on top of these.
    * Sounds like more sophisticated value comparison has been kicked
      down the road for another day.
    * There are weak versions of these
* Symbols
    * Used for unique identifiers.
    * Can give them an identifier for debugging.
* Generators
    * They can yield at several points.
    * You call them the first time. This gives you an iterator. Then
      you call `next` repeatedly. It returns `{ value, done }`.
    * You can pass arguments into `next`.
    * You can do shorthand generators for objects/classes.
    * You can get a bullshit kind of "await" this way by yielding promises.
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
* TypedArrays
    * For ASM presumably.

VM4260:2 §Reflect►
VM4260:2 §Reflect.getc
VM4260:2 §Reflect.setc
VM4260:2 §Reflect.hasc
VM4260:2 §Reflect.deletePropertyc
VM4260:2 §Reflect.getOwnPropertyDescriptorc
VM4260:2 §Reflect.definePropertyc
VM4260:2 §Reflect.getPrototypeOfc
VM4260:2 §Reflect.setPrototypeOfc
VM4260:2 §Reflect.isExtensiblec
VM4260:2 §Reflect.preventExtensionsc
VM4260:2 §Reflect.ownKeys, string keysc
VM4260:2 §Reflect.ownKeys, symbol keysc
VM4260:2 §Reflect.applyc
VM4260:2 §Reflect.constructc
VM4260:2 §Reflect.construct sets new.target meta propertyc
VM4260:2 §Reflect.construct creates instance from newTarget argumentc
VM4260:2 
VM4260:2 §own property order►
VM4260:2 §Object.keysc
VM4260:2 §Object.getOwnPropertyNamesc
VM4260:2 §Object.assignc
VM4260:2 §JSON.stringifyc
VM4260:2 §JSON.parsec
VM4260:2 §Reflect.ownKeys, string key orderc
VM4260:2 §Reflect.ownKeys, symbol key orderc

* Proxy objects where you can intercept method calls/access before they hit the regular object.
* String startsWith method finally.
* `Array.from` works instead of silly copying of `arguments`.
* `Array.find` takes a predicate function.
* Add a bunch of hardcore math methods like `Math.sinh`.
* TODO: Modules was not listed here!!!

## Next EcmaScript Revision

* Async, await is the only major feature being talked about right now.

## Source

http://kangax.github.io/compat-table/es5/
https://github.com/lukehoban/es6features
