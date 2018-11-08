## Hello World: Formatting/printing

`format!` is the heart of it all. Format strings can use just `{}`, or
`{0}, {1}, {2}` if you want a different sequence or reuse args. Or
even can do this:

    println!("{subject} {verb} {object}",
             object="the lazy dog",
             subject="the quick brown fox",
             verb="jumps over");

They note that you can use `{:?}` for printing debug stuff. And
`{:#?}` for pretty printed debug stuff (mostly indenting I think).

You can write your own `Display` and `Debug` implementations. You'll
be passed a formatter. I used the `Formatter::debug_struct` method
once upon a time.

## Conversion

They mention "turbofish"; for instance the `parse` method:

    let num = "10".parse::<i32>().unwrap();

Type inference could have figured this out:

    let num: i32 = "10.parse().unwrap();

## Flow Control

You can give labels to loops, which let you break out from deep nesting:

  'outer: loop {
    'inner: loop {
      break 'outer;
    }
  }

I think you can use this to give names to scopes whenever you want!

You can even use a loop as an expression like an `if/let`. The `break`
is the expression's value!

**For loops**

Anything which has the `IntoIterator` trait can be turned into an
`Iterator`. Vec has implementations of `into_iter` for `Vec`, `&Vec`,
`&mut Vec`.

Also, beacuse a `Vec` can be dereference converted to a slice, it gets
the methods `iter` and `iter_mut` from slice.

`Iterator` trait just has a `next` method. Any `Iterator` or anything
that implements `IntoIterator` can be for looped over.

**TODO**: Read about Deref conversion.

**Match**

They let you do multiple cases per match arm using pipes just like in
Haskell.

    match num {
      2 | 3 | 5 | 7 | 11 => println!("This is a prime"),
    }

Matches can have *guards*, which are associated if blocks. Again like
Haskell. And you can do `@` bindings like Haskell too!

Matches with only one useful arm can be written as `if let`. There's
even a `while let` (not that this seems too useful...).

**Destructuring**

You can destructure tuples (and Enum tuples). You can destructure
structs (and Enum structs).

We know you can capture `ref`, and `ref mut` when matching what would
otherwise be a value. You might capture `mut` if you are matching the
right type, but just want it to be mutable.

If you are matching a reference but want to capture a value you can do:

    match ref {
      &val => ...
    }

Rust now does this simple thing. If you are matching a `&Person`, then
all the destructures of `name` and `age` are references (no need to say
`ref`). Unless you say `&name` or `&age`, which captures by value.

TODO: Up to ch9 https://doc.rust-lang.org/rust-by-example/fn.html
