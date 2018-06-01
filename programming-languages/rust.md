Install rustup.

Use the VS Code extension.

You can start new projects with cargo. If you install the cargo edit
plugin, you can say `cargo add` to add deps.

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

## Ownership

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

## Structs

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

## Methods

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
Presumably you can specialize as needed. This is called a "blanked
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

## Lifetimes

Seems simple enough?

    fn longest<'a>(x: &'a str, y: &'a str) -> &'a str {
        if x.len() > y.len() {
            x
        } else {
            y
        }
    }

I believe this says: the result is only good for the period of time both
`x` and `y` references are valid. You can do similar for a struct:

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

## Testing

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

## TODO

Wow there sure is a lot to read about Rust...

**TODO**: Up to p272 in the book; working with environment variables.

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
