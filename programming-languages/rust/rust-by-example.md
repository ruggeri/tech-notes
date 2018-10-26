## Formatting/printing

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

TODO: Up to https://doc.rust-lang.org/rust-by-example/types.html
