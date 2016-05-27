Link:

    https://docs.google.com/spreadsheets/d/1cCgNwxc8TNRkVAA5u8-_GMef6-vlKoBDasBvhOm88DE/edit#gid=0

## Results Summary

For scripting or fast-iteration web-development, prefer
Python/Ruby. You could use JS, but increased performance probably
doesn't justify the increased pain.

For performance critical work, use Rust. Rust gives you the
appropriate level of control, while providing much more safety. Use
Rust for projects where performance will be the difference between
success/failure. Rustc is apparently getting much faster, too, leaving
ever less oxygen for Golang.

For performance-sensitive prototyping, consider Clojure. In
particular: is there a high probability the work will be thrown away?
If perf doesn't matter, of course use Python/Ruby. I'm thinking of
AI/ML stuff like at QC. Development can still be painful when problems
a compiler would have detected waste time. Iteration cycle for this
kind of work is often longer, so tests are more important. Not *that*
much space here.

For large app development meant to last a longtime, you want static
typing. The primary goal is to enable later refactoring. You still
want as rapid development as you can still get, which is your
challenge. It is a battle between Golang and Scala; each has its
weaknesses, and it's unclear what I would like better without more
experience. My guess is that I would prefer Scala despite its
compilation woes because it is more expressive. But I would have to
try that out.

## To Explore

* Elm
    * I don't know much about it. It is a different paradigm, so that
      could be fun to try.
* Play framework for Scala.
