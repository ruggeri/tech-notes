**This tracks The Rust Programming Language**

## Installing Rust

Install rustup. I used the shell script from their website (not
homebrew). Add the rustfmt and rls components.

Use the VS Code extension. (Haven't re-installed this yet).

You can start new projects with cargo. If you install the cargo edit
plugin, you can say `cargo add` to add deps.

## Common Programming Concepts

Unwraps errors with `expect`.

`match` can unwrap sum types.

`parse` is weird. Has something to do with templates... Can look at
desired return type to know what to do? **TODO**: How does parse work?

You can have tuples `x = (1, 2, 3)`. You can destructure tuples `let (x,
y, z) = (1, 2, 3)`. You can access tuple fields `x.0`.

You also have arrays. Rust checks indexing into arrays.

Blocks can be expressions and their value is the last value without a
semicolon.

Functions can be given a return type with `fn num() -> i32`.

`if`/`else` can be an expression. You can also use "if let" to do a
simple form of match.

You have `loop`, `while`, and `for`. For works like this:

```
fn main() {
    let a = [10, 20, 30, 40, 50];

    for element in a.iter() {
        println!("the value is: {}", element);
    }
}
```

 **TODO**: how does iterator work?

Note you can do countdowns with a `Range`:

    fn main() {
        for number in (1..4).rev() {
            println!("{}!", number);
        }
        println!("LIFTOFF!!!");
    }

## Understanding Ownership

Every value in Rust has a variable that is its owner. There can be only
one. When the owner goes out of scope, the value is dropped.

They talk about the difference between a string literal (`&'static str`)
and a `String`. String literals are immutable, so we do stuff like
`String::from("abc")`.

When an owner goes out of scope, the `drop` method is called.

Here's an example

    {
      let s1 = String::from("abc");
      let s2 = s1;

      // This does not do a deep copy. It does a shallow copy. And the
      // `String` has an underlying memory reference, so they can't both
      // own the data. In fact, `String` doesn't implement the `Copy`
      // trait, so an asignment like this means "move."
    }

There is no deep copying by default in Rust. We'll use `let s2 =
s1.clone()` if we mean that. The `clone` method is user defined, I
believe.

All the primitive types implement the `Copy` trait. You can tell Rust
that your new type implements copy, but only if none of the constituents
implements the `Drop` trait. Presumably because if you do something
special with `Drop` (like free memory), then copying would do it twice.

Functions can take ownership. Like so:

    fn main() {
        let s = String::from("hello");  // s comes into scope

        takes_ownership(s);             // s's value moves into the function...
                                        // ... and so is no longer valid here

        let x = 5;                      // x comes into scope

        makes_copy(x);                  // x would move into the function,
                                        // but i32 is Copy, so it’s okay to still
                                        // use x afterward

    } // Here, x goes out of scope, then s. But because s's value was moved, nothing
      // special happens.

    fn takes_ownership(some_string: String) { // some_string comes into scope
        println!("{}", some_string);
    } // Here, some_string goes out of scope and `drop` is called. The backing
      // memory is freed.

    fn makes_copy(some_integer: i32) { // some_integer comes into scope
        println!("{}", some_integer);
    } // Here, some_integer goes out of scope. Nothing special happens.

Functions can take ownership, but likewise when they return they can
give ownership.

    fn gives_ownership() -> String {             // gives_ownership will move its
                                                // return value into the function
                                                // that calls it

        let some_string = String::from("hello"); // some_string comes into scope

        some_string                              // some_string is returned and
                                                // moves out to the calling
                                                // function.
    }

Of course, often we don't want a function to take ownership. In that
case, we want to use a reference.

    fn main() {
        let s1 = String::from("hello");

        let len = calculate_length(&s1);

        println!("The length of '{}' is {}.", s1, len);
    }

    fn calculate_length(s: &String) -> usize {
        s.len()
    }

If we hadn't used `&String`, we would not have been able to use `s1`
later in the main function, as it would have been dropped by
`calculate_length`.

When a function borrows, notice how we must explicitly pass `&s1`.
Likewise, if a function wants to do a mutable borrow, we explicitly pass
`&mut s1`.

Rust enforces that a mutable reference is exclusive to any simultaneous
immutable references. This is important for when there are threads.
However, this seems limiting if codeA wants to mutate something that is
stored by reference in codeB. I understand the potential problem with
threads, but that seems like not a totally unnatural thing to try to do
in C++.

Rust will prohibit any dangling references. As in:

    fn dangle() -> &String {
        let s = String::from("hello");

        &s
    }

The way this works persumably is that Rust knows the lifetime of every
variable. It can enforce that the lifetime of a reference is never
longer than the lifetime of the original value.

I'm thinking to myself: this sounds great for multithreaded code, but
potentially burdensome if code is merely singlethreaded, because the
Rust compiler is going to have limited ability to statically reason...

**TODO**: Write code where I have to implement copy/clone/drop.

## The Slice Type

Interesting. A Rust char is a unicode code point. Does that mean every
Rust char is 4 bytes or something???

Destructuring is fancy. Consider:

    fn first_word(s: &String) -> usize {
        let bytes = s.as_bytes();

        for (i, &item) in bytes.iter().enumerate() {
            if item == b' ' {
                return i;
            }
        }

        s.len()
    }

Note the use of `&` on the item. Without it, I guess the type of item is
`&u8`, which is not comparable to `b' '` which has type `u8`. To copy
the value, it appears we need to write `&item` as the variable, which
dereferences? We could use just plain `item`, but then we must say
`*item` later to dereference.

I think the similarity to `s: &String` is deceptive. Notice that we have
said `&item`, with the `&` before the variable name. That's quite a bit
different than `&String`, with an `&` before the type. It's like in C
the difference between `int* x` and `*xp`. (BTW I think that you use
`ref` when you want to go value -> reference in a binding).

You can take slices of strings:

    let s = String::from("hello world");

    let hello = &s[0..5];
    let world = &s[6..11];

Notice the use of `&`. Presumably the lifetime of these references must
not exceed the lifetime of the base variable.

Presuambly

    let world = s[6..11];

would not work as we cannot transfer ownership. Or maybe it can, if we
never us any of `s` anymore? Actually it looks like the problem is that
a slice doesn't implement the `Sized` trait, because it is of variable
size, thus it cannot live on the stack. We must always refer to it by
reference.

A string slice has type `&str`. That's the type of a static string.
Okay... It looks like functions that don't need to change the size of a
`String` should just take a `&str`, though ergonomically this feels
weird:

    fn first_word(s: &str) -> &str

    fn main() {
        let my_string = String::from("hello world");

        // first_word works on slices of `String`s
        let word = first_word(&my_string[..]);

    ...

You can also have slices of arrays, which have types like `&[i32]`. An
array like `let a = [1, 2, 3]` has type `[i32; 3]`. But a slice like
`&a[..]` has type `&[i32]`. Slices have a `len` method since we don't
know that at compile time. But they are not mutable in size.

## Using Structs to Structure Related Data

Structs work like you think. A mutable struct lets you mutate the
fields.

You can do the JS thing and say

    User {
        username,
        email,
        active: true
    }

We can also do the "splatting" type thing. But we use `..` instead of
`...`. Makes it easier to "mutate" immutable values.

    let user2 = User {
        email: String::from("another@example.com"),
        username: String::from("anotherusername567"),
        ..user1
    };

You can also have tuple structs: `struct Color(u8, u8, u8)`. You make
one like so: `Color(0, 0, 0)`. You do this to distinguish from other
tuples, and probably to add methods.

Can structs store references? Yes! You might prefer:

    struct User {
        username: &str,
    }

which is more general. But this is tricky. We can't store a reference in
a struct without using *lifetimes*, and I don't understand those yet.

**TODO**: Implement a struct that needs lifetime annotations.

They talk about `#[derive(Debug)]`, which lets you use `println!("{:?}",
user)`. You can also use `{:#?}` which indents. Note that `fmt` library
has a bunch of helpers to help you implement `fmt::Debug` yourself.

## Methods Syntax

Along with a `struct Rectangle { ... }`, you can also have an `impl
Rectangle`. In here go the methods. For instance:

    impl Rectangle {
        fn area(&self) -> u32 {
            self.width * self.height
        }
    }

Presumably you can borrow mutable, or you can even take ownership. You
might take ownership if you're converting the type to a new thing and
you want to invalidate access to the old thing.

They also talk about associated functions, which are basically like
class methods.

You can have multiple `impl` blocks, but they'll discuss that later.

## Enums

Can have methods. They're sum types. They can be tuples, or even have
associated field names (like structs)! Of course, `Option<T>` is the
most important enum of all.

They talk about match, how you can match variants, and how you can
destructure. You can use `_` to handle any cases not enumerated. You can
also potentially use `if let`, which works just like Swift.

(They don't mention `@` which works like in Haskell).

Interesting: I learned you can mutate an enum variable. As in you can
reassign.

They also don't mention the `take` method of option, which is very
valuable. It mutates to None while removing the value.

## Modules

Slighty wack. You can split your code into source files. Then you can
say `mod client;`, and it will load the `client.rs` definitions as a
module named `client`. You don't say `mod client { ... }` in
`client.rs`; it's already put in the mod just by being in another file.

This is for a library anyway.

But then you can actually make directories named `network/`. We would
then load `network/mod.rs`. Now we can have further submodules. As in,
if `network` had a `server` submodule, then `mod.rs` could say `mod
server;` and that is defined in `network/server.rs`.

You can mark functions and modules et cetera as `pub`. Then others can
use them.

Common to write your code as a library, then use `extern crate` to load
the library for your short binary code.

You use `use` to bring names into scope. To access sibbling modules you
typically use `super` rather than starting from the top again.

## Vec/String/HashMap

You can say `let v: Vec<i32> = Vec::new()`, or just `let v = vec![1, 2,
3]`.

You can get references to values like `&v[1]`. Of course if they're
copyable that's fine too. But a reference like `&v[1]` will block
mutable ops like `v.push` (because may have to recopy the entire store).
You'll have to use `{}` if you want to do that...

It looks like you can pass a `&String` for a `&str` argument. Rust calls
this a "deref conversion," but it's not explained yet. Kinda
interesting: `+` takes ownership of the first argument. Makes sense;
this must be an efficient version. You write; `let s1 = s1 + &s2`, to
shadow the original s1.

**TODO**: Look into deref convresions, which I assume is some traits
magic.

A `String` is a wrapper for `Vec<u8>`.  Note, the `String#len` method
returns the number of *bytes*, not number of unicode characters. In
fact, even the notion of a character is sketchy; what about diacritic
characters? For this reason you cannot index into a string. Also time
complexity, because index to character or whatnot requires `O(k)` time.

You can use slice like `&hello[1..4]` to get those *bytes*. Rust crashes
if the byte indices don't line up to characters. You can call `#chars`
to iterate over character values, giving you a series of chars. A char
stores a unicode code point. Note that it is *not* in UTF-8 encoding;
it's just the UTF number. So char is less efficient than UTF-8. It's
more like UTF-32 which is fixed width.

Sidebar: what is the point of UTF-16? Worst of both worlds? Incompatible
with ASCII, and inefficient. But not if most of your text is in Chinese,
in which case UTF-16 is more efficient. But if there's a lot of
whitespace for formatting, UTF-8 is better again. Also, if you loose a
byte of UTF-16 data, everything becomes garbage. UTF-8 is self
synchronizing.

You can also use `#bytes()` to get an iterator over `u8` values.

Grapheme clusters are not parsed by Rust; you need an external crate for
that.

HashMap is the next structure. Not literal syntax or macros, but you can
call `collect` on a sequence of tuples.

    let teams  = vec![String::from("Blue"), String::from("Yellow")];
    let initial_scores = vec![10, 50];

    let scores: HashMap<_, _> = teams.iter().zip(initial_scores.iter()).collect();

**TODO**: Learn how stuff like "collect" works with type inference.

The hash map takes ownership of keys/values. You *can* have references
as your keys/values, but they must have lifetime at least as long as the
HM.

They have a cool interface called `hm.entry("my key")`, which returns an
`Entry` object, which might even be empty. You can then use methods like
`or_insert` to insert only if the key isn't already there.

## Error Handling

There are *match guards*, which are basically ifs that you tack on to a
variant match.

They talk about the `unwrap` (panic on error) and `expect` (you provide
the panic message) methods of `Result`. A result is either `Ok` or an
`Err(err)` variant. The `err` value is typically an enum of things that
can go wrong.

There is also a special `?` syntax that will return
the error, if any, otherwise unwrap. This is useful for "propagating"
errors.

    fn read_username_from_file() -> Result<String, io::Error> {
        let mut f = File::open("hello.txt")?;
        let mut s = String::new();
        f.read_to_string(&mut s)?;
        Ok(s)
    }

**TODO**: Any way to make `RUST_BACKTRACE=1` (an environment variable
you can set when running a debug build) the default?

## Generic Types, Traits, and Lifetimes

Generic functions, structs enums. They show a generic method; you use
`impl<T> Point<T> { ... }`. There is a concept like thing (called trait
bounds), which says that parameters can be required to implement certain
traits.

When implementing, you can have specializations. Traits can also have
default implementations. This lets you use them as mixins.

```
pub trait MyTrait {
  pub fn my_fun(&self) -> int;
}
```

Once you have traits, you can use them as trait bounds. You say: this
generic function exists *if* the type meets the required trait bounds.
Can also uses them to conditionally implement methods:

    impl<T: Display + PartialOrd> Pair<T> {
      fn this_method_only_if_both_are_met() {
        ...
      }
    }

You can even implement a trait for *any* type that meets a trait
requirement:

    impl<T: Display> ToString for T {
        // --snip--
    }

Anything that implements `Display` also implements `ToString`.
Presumably you can specialize as needed. This is called a "blanket
implementation."

As you can see, this is why we need to be able to have multiple `impl`
statements.

I'm confused about what happens when there are two implementations of
the same method in two different traits. How do we choose? I think Rust
requires us to disambiguate. I think this is part of the reason why we
can only use trait methods if the trait has been brought in scope. I
think it's also why we can only implement a trait in a module if either
(1) the trait has been defined in the module or (2) the class being
implemented on is defined in the module.

**TODO**: Exactly how do trait methods become methods of the object? I
think it's similar to Scala, and maybe also Go in the sense that traits
do not imply vtables. Thus traits can be applied to primitive types.

They've added some new syntax:

    fn notify(item: impl Summary) {
        // ...
    }

This is just pure syntactic sugar for:

    fn notify<T: Summary>(item: T) {
        // ...
    }

There is also syntax to say:

    fn do_something() -> impl MyFavoriteTrait {
        // ...
    }

This is useful when the return type may have no name known to you
(like the type of a closure!). It is **not** a way to do runtime
polymorphism. `do_something` must return only *one type* of thing. It
cannot sometimes return `X: MyFavoriteTrait` and others return `Y:
MyFavoriteTrait`.

## Validating References with Lifetimes

Seems simple enough?

    fn longest<'a>(x: &'a str, y: &'a str) -> &'a str {
        if x.len() > y.len() {
            x
        } else {
            y
        }
    }

The result is only good for the period of time *both* `x` and `y`
references are valid. Note: even if `x` and `y` do have different
"concrete" lifetimes, when calling `longest`, we will set `'a` to be
the shared overlap of the two.

You can do similar for a struct:

    struct ImportantExcerpt<'a> {
        part: &'a str,
    }

This says: the lifetime of the struct must have a lifetime that does not
exceed the stored reference.

Inference rules for functions are simple:

1. Every parameter assumed to have its own distinct lifetime.
2. Output lifetimes are assumed to be same as parameter lifetime, if all
   parameters have same input lifetime.
3. Otherwise, if a *method* on a reference, then output has same
   lifetime as the reference.

There is a special lifetime called `'static`, which means lives forever.
Typically not the answer to your problems.

## Writing Automated Tests

You make a module named `tests` and apply `#[cfg(test)]` to it. Then you
add an anotation `#[test]` to each fn you want in the module. You can
use `cargo test` to run them. You use macros like `assert_eq!` and the
like.

For unit tests, they usually put the tests modules either (1) right in
the source file of the module, or (2) in a submodule file or directory.

For integration tests, you can make a top level tests directory. You
don't need a `mod.rs` here; `cargo test` will run all tests.

## An I/O Project: Building a Command Line Program

Looked up how `collect` works. It's an iterator trait method. It is
templated on the return value, which should be a collection that
implements the `FromIterator` trait. An `Iterator` has an associated
type called `Item`; the `collect` method has a type bound where the
`FromIterator` should have as its template argument `Iterator::Item`.

In the example they use `std::fs::File`. To use fancy read methods on a
file, you need to include `std::io::Read`, which is the mixin that goes
beyond a simple `read` method into a mutable slice of `u8`. But you need
to have the trait in scope. The easiest way to load traits like that is
to use `std::io::prelude::*`. Another useful one is `BufRead` trait,
which gives `#lines`.

They use `Result::unwrap_or_else` to unwrap errors parsing arguments. I
also use an enum for errors. `unwrap_or_else` takes a closure; and
inside we use `process:exit`.

They use a pattern for their `run` function where it returns a
`Result<(), Box<Error>>`. The idea must be (1) there really is no return
value, but (2) there can be many kinds of `Error`. `std::error::Error`
is a *trait* (different from the result variant `Err`, btw), which
basically just has a description method. Must be boxed.

**TODO**: Why is an associated type needed? Doesn't `Iterator` have a
template argument; can't we just use that?

I built my own iterator. It needed some tricks.

* The iterator uses a boxed iterator with string references.
* You specify the associated type. But you also need to give the
  lifetime of the Iterator trait. That is: `Box<Iterator<Item=&'a str> + 'a>`.

**TODO**: Look up trait lifetimes when boxing.

## Iterators and Closures

You write closures like this:

    let f = |arg1| {
        arg1 + 123
    };

Closures get type inference on their args based on first use. But you
can also specify if need be. Their type is `Fn(i32) -> i32`.

Closures can either use a mutable or immutable reference, or may take
ownership of the closed over variable.

**TODO**: I'm a little confused about `FnOnce`, which is a trait for
when a closure takes ownership. Can the closure be called only once,
because after the memory is discarded? Or does the captured environment
live as long as the closure does?

They then show the `Iterator` interface and how to implement `next`.
Then they apply it to the Regexp project just like I already did.

## Cargo and Crates.io

Mostly boring. Some info on workspaces, which I should review, but
instead skipped because I am more interested in the language itself.

## Smart Pointers

Smart pointers are those that implement `Deref` and `Drop` traits. Your
main smart pointers are:

1. `Box<T>`
2. `Rc<T>`
3. `Ref<T>` and `RefMut<T>` via `RefCell<T>`.

Boxes store things on the heap, simple. No overhead. They are implicitly
coerced to references because they implement `Deref`, which has a method
`fn deref(&self) -> &T`. `Deref` has an associated type called `Target`.

Note that `String` implements `Deref` for `&str[]`.

There's a second trait called `DerefMut` that changes `&mut Box` to
`&mut Target`. You have to implement that separately.

`Drop` is a trait that just calls `#drop`. It's for RAII.

`Rc` is another smart pointer. It does reference counting. You can start
out by `Rc::new(x)`. This takes ownership. You can then use `Rc::clone`
on the reference to increment the shared count. We could just say
`my_rc_obj.clone()`, but this looks like a deep clone, so the Rust folks
recommend using `Rc::clone(my_rc_obj)` style for clarity of purpose.

`Rc` only lets us share immutable references, for the obvious reason
that a mutable reference is supposed to be exclusive of any immutable
reference.

They talk about `RefCell` and "interior mutability." This is when you
have an immutable value, but you want to mutate something inside. I
think it's like marking a field mutable in C++. They give an example
of a mock object: the trait it might implement may have an immutable
API, but the mock object wants to record how many times a method is
called, or what arguments it is sent.

`RefCell` works by returning smart pointers called `Ref<T>` and
`RefMut<T>`. `RefCell` enforces that it will panic when you ask for a
second simultaneous `RefMut<T>`. But it also needs to know when those
things die.

`Cell` works similarly, but appears to copy the value in and out of
the cell. This is basically a non-heap version of `RefCell`. But it
doesn't need to do run-time enforcement that there are not multiple
mutable references out because you you never have a reference to
it. You just swap in/out. So it's more efficient.

`RefCell` and `Cell` are not thread-safe. The `Mutex` type is the
thread-safe alternative you'll want.

**TODO**: Are there other good examples of wanting to use `Rc` and
`RefCell` together?

**TODO**: Write an implementation of `Rc`.

They talk about `Rc::downgrade`, which gives you a *weak* reference. You
can use `Rc::upgrade` to convert a weak reference to an `Option<T>`.

## Fearless Concurrency

`thread::spawn`, and you give a closure. Gives you a `JoinHandle` and
you can call `join` on it. You can call `unwrap` to get the return
value.

They show how to use `mpsc::channel`. It gives a `(tx, rx)` pair. You
can then move the `tx` to another thread which can send down values. The
`rx` has methods like `recv` and `try_recv`. You can call `tx.clone` to
have multiple producers.

They also have a `Mutex` class. When you call `lock` you get a smart
pointer called `MutexGuard`. When dropped the mutex is unlocked.

Now, if you want to share a `Mutex` across threads, you can't, because
no one will have ownership of it. So you need to put it in an `Rc`.
Except not an `Rc`, because `Rc` is not thread safe. So you need to use
`Arc`, which is the thread-safe version of `Rc`. Of course it is even
slower than `Rc`. The "a" is for "atomic."

They hint at the similarity of `Rc` and `RefCell` to `Arc` and `Mutex`.

They talk about the `Send` trait. Pretty much everything can be moved
across threads. An exception is `Rc`, which has shared mutable
stateinside its immutable interface.

`Sync` is another important trait. If `T` is `Sync`, that means that
`&T` is `Send`. That is, a value is `Sync` if threads can share
references to the object. We already know that `Rc` is not `Sync`,
since it's not even `Send`. `RefCell` *is* `Send`, but it is not
`Sync`, because the checking that `RefCell` does is not thread
safe. This is the point of `Mutex`. Note that `Arc<T>` is not `Sync`
unless `T` is `Sync`.  Otherwise, even though we would be doing ref
counting correctly, we would have data races on `t`. (Correct. `Arc`
should only hold things that are `Sync`).

You don't normally implement `Send` or `Sync` yourself, because those
are automatically derived when a struct is composed of `Send` and `Sync`
stuff. But you can write unsafe code to do this; but we'll have to talk
about that another time...

## Object Oriented Programming Features of Rust

Doesn't really have inheritance. They mention that you can use override
the default methods of traits, which is sort of like code reuse. They
also talk about how inheritance relates to generic programming. They
talk about "bounded parameteric polymorphism." Basically, they mean that
trait bounds are like your interfaces, which isn't exactly inheritance,
but is a related concept.

So you can use traits like interfaces, and they will perform dynamic
dispatch for you. Not every trait is usable for trait objects. For
instance, take the `Clone` trait:

    pub trait Clone {
        fn clone(&self) -> Self;
    }

This clearly can't work because an implementer must return a value,
rather than a pointer or reference. Likewise, interfaces with generic
parameters can't be used, because they won't have been generated for
every instance that implements the trait.

They then show us how to store and use a `Vec<Box<dyn
MyTrait>>`. Presumably you cannot have `Vec<MyTrait>` because
`MyTrait` isn't sized. But then my question is: how can we store a
`MyTrait` in our own structures? As in: how does `Box` work with a
trait template parameter. Is it ever necessary to worry about that?

It looks like the syntax is `Box<dyn MyTrait>`. The `dyn` is
presumably for "dynamic."

## Patterns and Matching

**TODO**: Up to p511.

Places pattern matching happens:

1. `match` (duh)
2. `if let`
3. `while let`
4. `for`
    * `for (index, value) in v.iter().enumerate()`
5. `let (x, y, z) = (1, 2, 3);`
6. `fn print_coordinates(&(x, y): &(i32, i32)) {`

`let`, `for`, and function parameters must be *irrefutable* patterns.
Others are conditional so can be *refutable*.

You can use `|` to "or" patterns as in Haskell.

They note the need to destructure references sometimes:

    let points = vec![
        Point { x: 0, y: 0 },
        Point { x: 1, y: 5 },
        Point { x: 10, y: -3 },
    ];

    let sum_of_squares: i32 = points
        .iter()
        .map(|&Point { x, y }| x * x + y * y)
        .sum();

If you tried to destructure with just `Point { x, y }`, you'd get an
error: "expected &Point, found struct `Point`".

They mention using `..` to ignore all other parts of a value. They
mention **match guards**, which are like in Haskell.

    let x = Some(456)
    let val = 123

    match x {
        Some(num) if num == val => {
            println!("Good work!");
        },
        _ => {
            println!("Doees not match!");
        }
    }

They also mention `@` which is like Haskell.

They mention the old `ref` keyword:

    fn my_robot_function(robot_name: &Option<String>) {
    match robot_name {
        &Some(ref name) => println!("Found a name: {}", name),
        None => (),
    }

You used to need to usee `ref name` because otherwise Rust would think
you were taking ownership of the string. Which isn't possible if
`robot_name` is itself a reference.

Rust now assumes that if you are matching a reference, that all
bindings are borrows too. The keywords are much less needed. But these
keywords are still potentially useful if you want to borrow just parts
of a struct for mutability and others immutably. But that is much more
rare.

## Unsafe Rust

Can:

* Dereference raw pointer.
* Call unsafe function or method. Typical example: an extern C function.
* Access/modify a mutable static variable.
* Implement an unsafe trait: Sync or Send.

They show an example where you want to "split" a slice into
two. Normally you can't have two mutable references to the same slice.

## Advanced Lifetimes

They show lifetime subtyping. Basically this is just a way to assert
that a lifetime `'a` outlives a lifetime `'b`. It is necessary in some
scenarios. They gave an example like:

    A context object stores a reference to a string.
    We pass a context object *reference* into a function.
    The function wants to return a reference to the underlying string.
    The string lifetime should be able to be used even after the context
    goes away.

They show the *anonymous lifetime*. It is just syntactic sugar:

    fn wrap_str(s: &str) -> StrWrap<'_>
    ==
    fn wrap_str<'a>(s: &'astr) -> StrWrap<'a>

This just saves on some typing. It's kind of like the elision rule,
but you get to use it in generic parameters.

## Advanced Traits

They talk about associated types. These are types declared inside a
trait:

    pub trait Iterator {
        type Item;

        fn next(&mut self) -> Option<Self::Item>;
    }

    impl Iterator for Counter {
        type Item = u32;

        fn next(&mut self) -> Option<Self::Item> {
            // --snip--

The only reason to do this versus `Iterator<T>` is that it limits to
one implementation. This means that we can call `next` on a `Counter`
without having to specify which kind of `Iterator` we are talking
about.

You can have different versions of the same `fly` method defined in a
`impl Pilot for Human` and `impl Human` directly. To clarify which you
mean, you can write: `Pilot::fly(&my_human_instance)`.

They mention that `trait OutlinePrint: fmt::Display {` means that
`OutlinePrint` relies on `fmt::Display` having also been implemented.

They mention the idea of the *newtype pattern*. Here you just create a
simple wrapper:

    struct Wrapper(Vec<String>);

    impl fmt::Display for Wrapper {
        // ...
    }

This lets you implement external traits for external types. But the
downside is you don't have automatic conversion. You can do that if
you implement the `Deref` trait.

## Advanced Types

They talk about type aliases: `type Kilometers = i32`. This is just a
synonym. It's not like `struct Kilometers(i32)` which is a
newtype. The point is for when you have complicated types and you
don't want to repeat the name constantly.

Nothing else in this chapter is very interesting...

## Advanced Functions and Closures

You can pass around function pointers. Their *type* is `fn(i32) ->
i32`, which is slightly different from the *trait* `Fn(i32) ->
i32`. Note, however, that the type implements the trait.

They give a nice example:

    let list_of_numbers = vec![1, 2, 3];
    let list_of_strings: Vec<String> = list_of_numbers
        .iter()
        .map(ToString::to_string)
    .collect();

Integers are `Display`, and `ToString` has a blanket defition for any
`Display`. Anyway, there isn't really any difference here once you get
down to compiling, but it's a preference thing.

They note that you can return a closure like so:

    fn returns_closure() -> Box<dyn Fn(i32) -> i32> {
        Box::new(|x| x + 1)
    }

But you can also use this syntax:

    fn returns_closure() -> impl Fn(i32) -> i32 {
        |x| x + 1
    }

This presumably will *not box*. The `impl Trait` idea is helpful when
the name of the type is complicated or unknown.

## TODO

Wow there sure is a lot to read about Rust...

* Things I learned via BST:
    * Box.
    * formatting with debug_struct
    * A bunch of annoying shit about match guards. If a match guard
      takes ownership, can't use an if. You can't take ownership of
      internal part for a match...
    * `take` on an option is very useful! Then I wrote my own for BST!
    * `mem::swap` is often essential. I think otherwise you can't swap
      without taking ownership, which is seldom what you want.
    * Box patterns were a helpful language extension. Would be
      interesting to investigate what others exist.
    * Looks like you often have to write two versions of methods that
      return either an immutable or mutable reference (`find` vs
      `find_mut`). No generics over the mutability of a reference.
      Really? That seems horrible.
    * Methods that return internal references (like I did with my BST)
      can be very powerful. For instance, to do remove, I did a find,
      then called delete on the reference to the node. Similar for
      insert.
* TODO: Read the standard library and some major libraries to see how
  Rust is used.

## Notes from Rust By Example

The `format!`/`print!`/`println!` macros let you use named args. As in:
`println!("hello {dog}; I am {cat}" cat="gizmo", dog="henry")`.

TODO: Up to https://doc.rust-lang.org/rust-by-example/types.html

## Other Sources

* Finish Rust book review.
    * Finish ch20 and review macros.
* Read Rust by Example.
* Look over Rust Reference?
* Look over Rustonomicon?
* Look over "Too Many Lists?"
    * https://github.com/rust-unofficial/too-many-lists
