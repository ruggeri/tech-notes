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
* Can partially apply functions. There's a `#curried` method on
  functions. It looks like you can even define functions curry style:
  `def f(x: Int)(y: Int)`?
* Weird partial function idea. A `case` statement defines a partial
  function, not all cases may be handled. You can then combine partial
  functions using their `orElse` method. This means you could
  mix-and-match partial functions defined by case statements.

## Resources

I am in the midst of reviewing these. This is the entire rip of
`scala-lang.org` and `docs.scala-lang.org`. After this plus the book,
I read everything. I have also read all of the Lightbend website.

* http://scala-exercises.47deg.com/koans#implicits
* http://twitter.github.io/scala_school/
* http://twitter.github.io/effectivescala/
* http://scalapuzzlers.com/
* http://scalatutorials.com/tour/
* http://www.amazon.com/Programming-Scala-Updated-2-12/dp/0981531687
    * Other books are out-of-date.
* http://www.scala-lang.org/api/current/#package
* http://docs.scala-lang.org/overviews/
* http://docs.scala-lang.org/tutorials/
* http://docs.scala-lang.org/style/
* http://docs.scala-lang.org/glossary/
* http://docs.scala-lang.org/cheatsheets/

## Typesafe/Lightbend Ecosystem

* Scala
* Akka
* Play
* sbt (also activator)

They also have their own IDE (based on Eclipse), and a DB query/access
library called slick. They own all the most important parts of Scala.

## TODO

* Akka
* Play
* sbt
