* `val` and `var`
* One constructor per class. You can declare arguments `var` or `val`
  to provide accessors.
    * Definitions of `val` and `var` inside the body will be instance
      vars assigned, xI think they are private.
* Can have singletons `object XYZ`. When there's a corresponding
  class, these are called *companions*. They can have state, and they
  can access internals of companion class.
* Tuples. You can destructure these.
* Lambda. `(x:Int) => x + 1`. You can also construct a `new function`
  with a `def apply` method. Can also write `def lambda(x:Int) = x +
  1`.
    * You don't need to specify argument types if that is clear from
      context.
* Lambas can close over state.
* I think you can also write `{_ + 1}` to make that lambda.
* Lists are cons cells, I think. Immutable. `Nil` is the empty
  list. `x :: Nil`.
* Can easily create list by `List(1, 2, 3)`. Has checked access.
* Many convenient methods for lists (map, filter, length).
* You can even do `list.reduce(_ + _)`. Notice that this will use the
  arguments positionally, I guess.
* `Map(1 -> "a", 2 -> "b", 3 -> "c")`. You can also specify by tuples,
  but this is just a syntactic sugar.
* You can use `get`/`apply` to access values. `apply` raises exception
  if no key, `get` returns an `Option`. I think you can write `apply`
  as `myMap(myKey) == myValue`.
* You can add maps together with `+`. You can get `.values` or
  `.keys`.
* Have `filterKeys` and `mapValues` to produce new maps.
* Weird. Map keys can be of mixed type! So can values. But I think the
  values are cast down to `Any`.
* Maps are unordered.
* Sets likewise can contain any type. `apply` returns true or false
  for Sets.
* Looks like there's a `for (x <- xs)` syntax.
* `String#format` takes a format string; you can use `%s` and
  presuambly `toString` is called. `%c` is for char, and `%d` for
  number.
* You can do `val match { ... }`. Most useful for case classes. Can
  have `_` as a wildcard. Can do destructuring in a pattern; can match
  part, destructure the rest. You may have to use backticks in a
  destructuring to use a variable's value, and not introduce a
  shadowing variable.
* There's some zany way to even destructure using a regular expression
  and captures.
* Of course you can destructure lists with conses. But that's just a
  special case of typical case class destructuring.
* Case classes work like you think. You can have a single case, or you
  can have multiple cases if you use `abstract class X` and then `case
  class SubX extends X`.
* You can mutate case class objects, but prolly better is to use the
  `x.copy(propName = newVal)` syntax. That's a wild syntax...
* Case classes can have default parameters.
* Looks like Scala has named arguments?
* There's apparently a way to turn a case object back into a
  tuple. `Person.unapply(person).get`. Not sure why `get` is needed.
* Why not always use case classes? It's typicaly just to use them when
  no private or mutable state.
    * Sounds like you can even subclass case classes. But apparently
      the subclass can't also be a case class.
    * Not sure if that's really important?
    * It sounds like it's mostly about intent/meaning.
* You can have `range(0, 10)` or `0.until(10)`. Or `0 until 10` for
  that matter.
* When you define a method `def f = 42`, then you don't need the
  parens when saying `obj.f`. If you say `def f() { side-effect-code
  }`, then you will need the parens when calling. This is just to
  highlight this to the user.
* Thunk arguments are passed like this `doTimes(3) { ... }`. The thunk
  can be run repeatedly. This helps you write DSLs.
* Traits:
    * Traits are like mixins. They're like in Ruby.
    * You can also mixin to classes after the fact. It's like creating
      a subclass but also doing the mixin. This is helpful, since it
      allows easy *decoration*.
    * For instance `smilingAnimal = new Animal with SmilingDecoration`.
        * Presumably creates an anonymous `Animal + SmilingDecoration`
          class and instantiates it.
* Type enrichment
    * Basically: you provide an implicit conversion to a class of your
      choice, and you add methods to this wrapper class.
    * This allows you to add methods like ActiveSupport, but in a
      safer way. It only happens in the file you do the import of that
      enrinchment library, and doesn't change underlying classes.
* Akka is also distributed by Typesafe, though not technically part of
  Scala.
    * Actor model. Can be distributed, or local with STM.
* Example software:
    * *Akka*
    * Finagle
    * Kafka
    * Lift
    * *Play* (this is blessed by Typesafe/Lightbend)
    * Samza
    * *SBT*
    * Scalding (Scala Cascading API)
    * *Spark*
    * Basically everything Twitter. LinkedIn is probably #2 user.
* Can partially apply functions: `add(3, _:Int)`. There's a `#curried`
  method on functions. It looks like you can even define functions
  curry style: `def f(x: Int)(y: Int)`?
* Weird partial function idea. A `case` statement defines a partial
  function, not all cases may be handled. You can then combine partial
  functions using their `orElse` method. This means you could
  mix-and-match partial functions defined by case statements.
* Implicit functions do conversions from one type to another. I recall
  this being a very disreputable feature of C++.
    * You can do this by `implicit class StringUtils(s: String)`.
    * You used to do it by `implicit def toStringUtils(s: String) :
      StringUtils`.
    * They make you scope a class like this, presumably so you can opt
      in by writing `import com.selfloop.ruggeri.StringUtils._`.
* Implicit madness
    * It goes further. You can declare a parameter list of implicit
      args. Then, the user needs to provide implicit values for those.
    * The resolution process is not simply by name, I think. Not clear
      exactly how that works.
    * Anyway, it's fucking scary. But maybe it's an important part of
      DSLs?
    * This is buy far the most wack feature I've seen so far.
* You can have multiple parameter lists to a function. Basically,
  you're writing a curried function. This sometimes presents a nicer
  API, and also sometimes is useful for inference since types of the
  first list can imply types in the next list.
* Traits: some methods are abstract, some are not. You write `class
  Cat extends Animal with FurryTrait`.
    * Traits can extend other traits, but they're only instantiated
      once. So there is no diamond formed.
* Can have `for (x <- xs)`.
    * Looks like `for { x <- xs, y <- ys } yield (x, y)` creates a
      generator which outputs pairs.
    * You can even toss in a condition.
    * This is sort of like list comprehensions I guess.
* Any method of a single parameter can be written infix.
    * Can write methods with no arguments as postfix
    * Can write prefix methods as `unary_+` and whatnot.
    * There is an idea of infix *types*. WTF? If you have
      `Pair[Person, Person]`, you can write this `Person Pair
      Person`. **WHY WOULD THAT BE A GOOD IDEA?**.
* There's a mutable map
    * You can use `+=`/`-=`. I guess this must literally be another
      operator? That's kinda obnoxious.
    * It looks like these operators work for vals, which makes sense,
      but is kinda awkward.
    * It's just weird because it's obviously different for numerical
      `var`s.
    * And actually any `var`, including maps.
    * I guess it's not really weirder than Ruby.
* String interpolation: `s"Hello $name"` does what you think. Can also
  do `s"Hello ${obj.name}"` to run arbitrary code.
* In definitions of functions, curly braces not needed for one-liners.
* `Array(1, 2, 3)` creates a normal Java array.
* BTW: it's easy to create anonymous subclasses: `new Cat("Gizmo") {
  override def meow = "GROWL" }`.
* If you declare constructor arguments `var` or `val`, the appropriate
  getter/setter methods will exist. You can also declare any
  `var`/`val` right in the body of the class. Last you can mark any of
  these with the `private` keyword if you wish.
    * NB: it looks like a constructor argument will not be retained in
      an ivar unless you explicitly say so, or if you use it in a
      method.
* An `Iterable` can be converted to a `Seq` via `#toSeq`. Any `Seq`
  can be converted to a list via `#toList`.
* A for comprehension with a `yield` creates a `Seq`.
    * Presumably sequences are lazily evaluated? I wonder if they
      cache their value? Prolly not, as that could use unbounded
      memory.
    * Not 100% sure why `List(1, 2, 3)` might be preferred to `Seq(1,
      2, 3)`. Actually, this is a special case where `Seq(1, 2, 3)`
      creates a `List`, which isn't inappropriate since `List`
      implements the `Seq` trait. What I find weird is that I didn't
      know that traits could have constructors.
        * Well, maybe `Seq` is an abstract base class. Not clear.
* `Iterable` is a trait for classes with an `#iterator` method.
    * Iterable is more general than Seq, as Seq has a defined order.
    * Only sequences have a definition of index/position.
    * More generally, a `Traversable` has a `foreach` method.
    * I suppose `Traversable` is more general than `Iterable`, in the
      sense that with an iterator, the user can decide to pause at any
      point, resume, etc.
    * Not entirely true; even with `Traversable` you can throw a
      `BreakControl` exception to stop iteration. But you won't be
      able to resume, I guess.
    * Iterators have convenient methods like `grouped` and `sliding`,
      btw.
    * When concatenating traversables, the result is of the left
      type. How obnoxious.
    * Many methods are on traversable rather than iterable. Almost
      all.
* Nice things: `flatMap` can deal with `Option[T]`, while `collect`
  can deal with partial functions involving cases (it just drops those
  inputs that don't have a match).
* But it looks like there is a `Traversable#toIterable` method. WTF,
  why are these different? Fuck you.
    * Maybe this allocates to have a defined order?
    * There's also a `#toIndexedSeq` method; an `IndexedSeq` just
      promises constant-time indexing, without actually changing the
      signature of the methods. Thanks.
    * `toStream` converts to a `Stream`, which is a lazy list. Not
      really sure that this is always possible...
* They also have a `Vector` class which is like from Clojure.
    * They also have a `Queue`, but it's not a banker's queue, just a
      typical 2-list queue.
    * HashMap is stored as a HashTrie.
* Can turn collections of pairs into a map with `#toMap`.
* It is expected that `Traversable` can have infinite
  length. Therefore there is a `#hasDefiniteSize` method; this will
  inform you whether it is possible for the object to be of infinite
  legnth (though it still may have finite length).
* `Traversable#groupBy` is useful with partial functions.
* `Traversable#view` let's you combine work so that you don't produce
  intermediate results. You then `#force` the view.
* Can call methods with named args. If some of the args have defaults,
  you can leave those out. Can of course have functions as defaults.
* TODO: Look into this whole "manifest" business. It looks like a
  means to pass information about classes, which are of course erased
  at compiletime.
* Extractors are defined by an `unapply` method; they let you do
  matching and extract information. This is automatically done for you
  with case classes.
    * It can be a little more complex than that. Any object can
      provide an extractor via an `unapply` method, which can take any
      kind of input.
    * So the same class can be extracted multiple ways.
    * But I think the most sensible version would be in the companion
      object.
    * In fact, the same extractor object can have many unapplys, so
      long as the signatures are different!
* "By-Name" parameters:
    * You can have an argument `f(x: => X)`. This means that the
      passed function is invoked wherever you reference `x`.
    * In particular, you don't need to say `x()`.
    * In particular, you can write `ternary(condition,
      println("True"), println("False))`. You just mark the second two
      arguments as `: => T`.
    * Basically when you mark an argument as "by-name", you're
      basically saying, capture the argument as a `def`, rather than a
      `val`.
* You can mark varargs with `sum(xs: Int*)`. You can splat via
  `sum(List(1, 2, 3):_*)`.
* When subclassing, you need to specify how to call the constructor
  function. If you share parameters, you will want to use `override`
  in some of the arguments to the type.
* Empty types:
    * `null` can happen to any non-value type. `Null` is a trait, of
      which `null` is the only instance. `Null` inherits from every
      reference type.
    * `Nothing` also inherits from every type, but it has no
      instances. This is used as a return type of, for instance, a
      function that always throws an exception.
    * `Unit` is equivalent to void.
    * `Nil` is an empty list. It is actually a value. It it an
      instance of `List[Nothing]`. See what I did there?
    * Of course, `None` is preferred to `null`. But you can still have
      `null` be set to any reference value (including case classes, I
      think?).
* You get the class object by `classOf[String]` or `"abc".getClass`.
    * Use `#asInstanceOf` to perform a downcast.
* With the `"""` multiline string, you can use the `|` and the
  `#stripMargin` method to remove all starting text up to and
  including the pipe.
* Enums work all nice-like. They get an `#id` and `#toString` method.
    * To do it, you write `Object Directions extends Enumeration { val
      North = Value; val South = Value }`.
    * `Value` presumably just creates a unique value. But I guess it
      must be syntactically special to increment the ids...
    * One trick is to `class DirValue extends Value { ... }`.
    * Then you can take in arguments, have methods, etc.
* You have a Java likes Generic system.
    * But you also have that secret manifest argument which I guess is
      deprecated.
    * Type arguments aren't erased if you use that manifest.
* Say that `T'` is a subclass of `T`. Say you are defining a class
  `Outer[T]`.
    * `Outer[T]` and `Outer[T']` are unrelated.
    * If you define `class Outer[+T]`, then `Outer[T']` is a subclass
      of `Outer[T]`. This is covariance.
    * If you define `class Outer[-T]`, then `Outer[T]` is a subclass
      of `Outer[T']` This is contravariance.
* When you use `Outer[+T]`, you aren't allowed to assign any `T` var,
  since you don't know whether will be safe. For instance `val o:
  Outer[Fruit] = Outer[Banana](banana); o.value = apple` would not be
  safe.
* `Outer[-T]` does let you do the set. But in that case you can't use
  the getter.
* Auxurily constructors can be defined by `def this(...)`. But they
  have to call prior aux constructors or the primary constructor.
* Traits vs abstract classes. When you have something interfacy:
    * Favor traits.
    * You can pass arguments to a constructor function in an abstract
      class. But why would you care? Shouldn't you just expose any
      necessary variables as methods?
    * It sounds like for Java interop, or efficiency, you might prefer
      abstract class.
    * When uncertain, 99% of the time you want Trait.
* `x()` is just calling a method named `apply`.
* Practical contravariance: `trait Function1
  [-InputType, +OutputType]`.
    * That means: if you function has a more specific output type, it
      can still be used here.
    * It also means that if your function has a less specific input
      type, it can still be used here.
* Odersky points out that covariance means you can't have a method
  which takes in a type of that object.
    * Which rules out setter methods.
    * Reason is that then your `Class[SubClass]` can be cast to a
      `Class[SuperClass]`, and then you can run
      `method(superClassInstance)`, even though it wasn't expected.
    * The way to do this for non-modifying "extensions" (like push on
      a list) is to make the push itself generic in the argument,
      returning a new list of that type.
    * Here you would use a lower bound.
* Type bounds: you can write `def quack[T <: Duck](ducks: Seq[T]) =
  ...`. I believe you can also write `>:`.
    * If you can do this, why mark anything covariant at all? Why not
      just have the user always write in the appropriate type bounds?
    * The reason is that it makes the user do more work to use your
      class.
    * If you take responsibility for marking your class' variance
      properly, then the user doesn't need to deal with this anymore.
* Looks like you can destructure a list like: `List(x, y, z, rest @ _*)`
* View bounds: allows you to use a type if there exists a
  conversion. This is useful when you don't literally need something
  to be of type X, so long as you can use it as X as needed.
* Generic Arrays:
    * Main problem is when you want to create an `Array[T]` in a
      generic method. Remember that `T` is lost at runtime, but you
      need this to allocate a `T[]` in Java.
    * Similar to allocating a primitive array in a Java generic
      method.
    * Answer is to use a manifest (in Java you would pass in the
      class).
* Structured typing `def f(x: { def get: Int }) = x.get`
    * Uses reflection, so poor perf.
* Macros aren't part of the language yet; though they seem to be
  edging toward inclusion. They're behind an experimental flag.

## Futures

* For futures you run `Future { ... }`.
    * You can use `onComplete` to register a callback. It will be
      passed a `Try[T]`, which is either the value or a throwable.
    * So you pass partial functions to `onComplete`.
    * `onComplete` doesn't return another promise, it returns unit.
    * To chain (a la Promises), you use `map`.
* Now you can do for comprehension

```
val purchase = for {
  usdQuote <- getUSDQuote
  chfQuote <- getCHFQuote
  if isProfitable(usdQuote, chfQuote)
} yield makeTrade(usdQuote, chfQuote)

purchase onSuccess { case () => println("Purchase completed!") }
```

Monads coming back at you! Remember that the `for` construct just
calls map on the `getUSDQuote`, and then the result is captured as
`usdQuote`, and then we continue. One problem, I think this will not
do the quotes in parallel. You cn fix that like so:

```
val usdQuoteF = getUSDQuote
val chfQuoteF = getCHFQuote

val purchase = for {
  usdQuote <- usdQuoteF
  chfQuote <- chfQuoteF
  if isProfitable(usdQuote, chfQuote)
} yield makeTrade(usdQuote, chfQuote)

purchase onSuccess { case () => println("Purchase completed!") }
```

Now you've at least started both. I believe this is Scala's answer to
async/await, btw. They recommend never explicitly blocking.

As in CPP, creating a promise object is a way to create a future. This
may be useful since a future will fork its own thread, whereas the
promise can be resolved either on the main thread, or really by
whoever you hand the promise object to.

This is described at: http://docs.scala-lang.org/overviews/core/futures.html

## Stack Example

```
class Stack[+A] {
  def push[B >: A](elem: B): Stack[B] = new Stack[B] {
    override def top: B = elem
    override def pop: Stack[B] = Stack.this
    override def toString() = elem.toString() + " " +
                              Stack.this.toString()
  }
  def top: A = sys.error("no element on stack")
  def pop: Stack[A] = sys.error("no element on stack")
  override def toString() = ""
}

class Animal
class Cat extends Animal
class Tiger extends Cat

val catStack: Stack[Cat] = new Stack()
val animalStack = catStack.push(new Animal)
val tigerStack = catStack.push(new Tiger)
```

* Note that if we have a `Stack[Cat]`, we can push an `Animal` because
  this will create a `Stack[Animal]`.
* I believe that we can push a `Tiger`, because the `Tiger` is of
  class `Cat`, so it will set `B=Cat`, which is appropriate, and
  leaves the type unchanged.
* Note that I think we'll typically only use `>:` in those situations
  where we create a new container with a higher level of granularity.
    * We would not want to mutate the actual object, since it is
      referred to by people just thinking `Cat`s are contained, not
      arbitrary animals.
    * So `>:` is most useful in a persistent context, I think.
    * Where we can easily create copies.
* OTOH, we would more likely use `<:` when we want to mutate; then
  it's important that this still be of the appropriate type.
    * In that case, the type of the container is not changing.
    * TODO: could be interesting to implement a bunch of containers.
    * Or at least read through the docs.
* I guess these relations are likely to flip in the contravariant
  case?

## Abstract types

This is an alternative to a parameter type:

```
trait Container[T] {
  def toSeq: Seq[T]
}

trait Container2 {
  type T
  def toSeq: Seq[T]
```

I don't know what the difference is. Most people don't seem to be able
to articulate why to use abstract types. It looks like it's best to
ignore this feature for now.

Actually, see: http://docs.scala-lang.org/tutorials/tour/explicitly-typed-self-references

This gives an example of a Graph, which recalls to mind abstract types
in Rust. Which I recall was a PITA.

That link is even more fucked by brining in more machinery. Fuck you!

## Call By Name

```
object TargetTest1 extends App {
  def whileLoop(cond: => Boolean)(body: => Unit): Unit =
    if (cond) {
      body
      whileLoop(cond)(body)
    }
  var i = 10
  whileLoop (i > 0) {
    println(i)
    i -= 1
  }
}
```

That is just fucking wrong. That is so warped. Maybe it's okay if you
respect only using this in language structure like stuff.

BTW, can even implement break!

## Value Class

Main use is enrichment; say you have an implicit class. You don't
actually want to construct that object. So you extend the implicit
class from `AnyVal`. Scala will be smart enough to realize to call the
method as a regular function with an argument.

Another use is for typesafety without the overhead. I think units is a
good example.

There's a lot of restrictions on what you can do in a value class,
naturally, since there won't be a class at runtime.

## Style Suggestions

```
// Setter method
class Cat {
  def size_=(sz: Int) {
    //...
  }
}
```

* Don't add parens to no-arg accessor methods; only use for methods
  with side effects.
* Mostly reasonable. Annotate return types of methods
  (documentation). Try to have one line functions, even if it has to
  be on a second line of the definition.
* Write parens lisp style (closing paren right after last arg)
* If you can't fit all parameters, put each arg on its own line and
  indent two spaces.
* If a method is basically just a match, just say `def last(s:
  List[Int]) = s match {\n...`.
* Recommends against multi-parameter list functions; only for new
  control structures or implicits.
* They suggest the following forms for lambdas:
    * `((a: Int, b: Int) => a + b)`: very explicit, wrap lambda in
      parens.
    * `(_ + _)`; the short way.
    * For multi-line lambdas, use the braces:

```
zippedEs.foreach { (e1: Element, e2: Element) =>
  println(e1)
  println(e2)
}
```

* In comprehensions, they recommend:

```
for {
  e1 <- els1
  e2 <- els2
} yield (e1, e2)
```

* If for is just used as a loop, do:

```
for (e1 <- els1; e2 <- els2) {
  println((e1, e2))
}
```

* Don't use arity-0 suffix notation: `seq toList`.  They do encourage
  arity-1 infix notation, but only if the method is functional: `names
  map (_.toUpperCase)`.

* Prefer one class per file, as in Java. When you do have very closely
  associated classes to put in one file (for instance, sealed
  implementations), then name the file with a lower case letter
  leading as if a package.

## Resources

I am in the midst of reviewing these. This is the entire rip of
`scala-lang.org` and `docs.scala-lang.org`. After this plus the book,
I read everything. I have also read all of the Lightbend website.

* http://scalapuzzlers.com/
* http://aperiodic.net/phil/scala/s-99/
* Coursera course (just for the exercises)

## Typesafe/Lightbend Ecosystem

* Scala
* Akka
* Play
* Slick
* sbt (also activator)

## TODO

* There don't appear to be real Play/Slick books out there right
  now. There appears to be more on Akka, but I think that's less vital
  to me at the moment. Reactive Web Applications or Play In Action
  might be okay when they come out.
* Akka
* sbt
* Annotations
* Delimited Continuation
* Type classes; do I really understand them?
* Manifest vs tag?
* Concurrent library; parallel collections
