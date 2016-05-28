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

## Resources

I am in the midst of reviewing these. This is the entire rip of
`scala-lang.org` and `docs.scala-lang.org`. After this plus the book,
I read everything.

* http://scala-exercises.47deg.com/koans#maps
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
