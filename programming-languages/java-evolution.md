Java adds features very slow. The last big set of features was
in 2004. Java 6 (2006) added no language changes; Java 7 (2011) added
generic type inferene; and Java 8 (2015) is the most significant
release in 11 years, adding just lambdas and default methods.

This is striking relative to C++. But I guess Java benefited from
C++'s learnings from the beginning.

* Java 1 (1996)
    * Prehistory.
* Java 2 (1998)
    * In 1999 gained HotSpot.
* Java 3 (2000)
    * First release that started with HotSpot.
* Java 4 (2002)
    * Exception chaining.
* Java 5 (2004)
    * Generics
    * Annotations
    * Autoboxing
    * enums
    * `for (Item item : items)` added
* Java 6 (2006)
    * This was the Java I lived with at QC.
    * Looks like several improvements to HotSpot happened in this
      version.
    * Added incremental GC. Tiered compilation.
* Java 7 (2011)
    * `invokedynamic`
        * For dynamic languages; I don't want to get into it.
    * Can write `List<>` to do some type inference.
        * `Map<X, Y> myMap = new Map<>();`
* Java 8 (2015)
    * Lambdas
        * `() -> { doStuff }`.
        * `(int x) -> x + 1`
        * Can sometimes leave out types if they can be inferred:
          `BinaryIntOp f = (x, y) -> x + y`
        * Just a shorthand way of defining a class implementing an
          `operation` method.
        * It looks like you can use a lambda anywhere an interface
          with a single abstract method is required. That adds
          flexibility.
        * Can close over variables, but they need to be never
          modified. They're going to be copied. But of course, they
          can experience mutations.
    * Default Methods
        * Interfaces can provide a default implementation of a
          method. But still no data.
        * Problem would be if two interfaces had different
          implementations of same method signature. That would cause
          an error.
        * I guess this makes them more like mixins.
