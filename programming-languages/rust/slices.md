# Slices, Chars, Strings

Interesting. A Rust char is a unicode code point. Does that mean every
Rust char is 4 bytes or something???

Destructuring is fancy. Consider:

```rust
fn first_word(s: &String) -> usize {
    let bytes = s.as_bytes();

    for (i, &item) in bytes.iter().enumerate() {
        if item == b' ' {
            return i;
        }
    }

    s.len()
}
```

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

```rust
let s = String::from("hello world");

let hello = &s[0..5];
let world = &s[6..11];
```

Notice the use of `&`. Presumably the lifetime of these references must
not exceed the lifetime of the base variable.

Presumably

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
