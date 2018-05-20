Install rustup.

Use the VS Code extension.

You can start new projects with cargo. If you install the cargo edit
plugin, you can say `cargo add` to add deps.

Unwraps errors with `expect`.

`match` can unwrap sum types.

`parse` is weird. Has something to do with templates...

You can have tuples `x = (1, 2, 3)`. You can destructure tuples `let (x,
y, z) = (1, 2, 3)`. You can access tuple fields `x.0`.

You also have arrays. Rust checks indexing into arrays.

Blocks can be expressions and their value is the last value without a
semicolon.

Functions can be given a return type with `fn num() -> i32`.

`if`/`else` can be an expression.

You have `loop`, `while`, and `for`. For works like this:

```
fn main() {
    let a = [10, 20, 30, 40, 50];

    for element in a.iter() {
        println!("the value is: {}", element);
    }
}
```

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
the difference between `int* x` and `*xp`.

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

They talk about `#[derive(Debug)]`, which lets you use `println!("{:?}",
user)`. You can also use `{:#}` which indents.

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
associated field names! Of course, `Option<T>` is the most important
enum of all.

They talk about match, how you can match variants, and how you can
destructure. You can use `_` to handle any cases not enumerated. You can
also potentially use `if let`, which works just like Swift.

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

## Vec/String/HashMap

You can say `let v: Vec<i32> = Vec::new()`, or just `let v = vec![1, 2,
3]`.

You can get references to values like `&v[1]`. Of course if they're
copyable that's fine too. But a reference like `&v[1]` will block
mutable ops like `v.push`. You'll have to use `{}` if you want to do
that...

It looks like you can pass a `&String` for a `&str` argument. Rust calls
this a "deref conversion," but it's not explained yet. Kinda
interesting: `+` takes ownership of the first argument. Makes sense;
this must be an efficient version.

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

The hash map takes ownership of keys/values. You *can* have references
as your keys/values, but they must have lifetime at least as long as the
HM.

## TODO

Wow there sure is a lot to read about Rust...

https://doc.rust-lang.org/book/second-edition/ch09-00-error-handling.html
