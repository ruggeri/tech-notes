# Ownership

Every value in Rust has a variable that is its owner. There can be only
one. When the owner goes out of scope, the value is dropped.

When an owner goes out of scope, the `drop` method is called.

# Strings

There is a difference between a string literal (`&'static str`) and a
`String`. String literals are immutable and have "static lifetime" (are
constants with lifetime equal to the program), so we do stuff like
`String::from("abc")`.

Here's an example

```rust
    {
      let s1 = String::from("abc");
      let s2 = s1;

      // This does not do a deep copy. It does a shallow copy. And the
      // `String` has an underlying memory reference, so they can't both
      // own the data. In fact, `String` doesn't implement the `Copy`
      // trait, so an assignment like this means "move."
    }
```

# Clone and Copy

There is no deep copying by default in Rust.

```rust
let s1 = String::from("abc");

// Doesn't do any kind of implicit copy; this takes ownership of the
// string.
let s2 = s1;

// Perform a clone. `Clone` is the interface to implement if you want to
// clone objects.
let s2 = s1.clone();
```

All the primitive types implement the `Copy` trait. You can tell Rust
that your new type implements `Copy`, but only if none of the
constituents implements the `Drop` trait. Presumably because if you do
something special with `Drop` (like free memory), then copying would do
it twice.

# Passing Ownership To/From Functions

Functions can take ownership. Like so:

```rust
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
```

Functions can take ownership, but likewise when they return they can
give ownership.

```rust
fn gives_ownership() -> String {             // gives_ownership will move its
                                            // return value into the function
                                            // that calls it

    let some_string = String::from("hello"); // some_string comes into scope

    some_string                              // some_string is returned and
                                            // moves out to the calling
                                            // function.
}
```

# Passing a Reference

Of course, often we don't want a function to take ownership. In that
case, we want to use a reference.

```rust
fn main() {
    let s1 = String::from("hello");

    let len = calculate_length(&s1);

    println!("The length of '{}' is {}.", s1, len);
}

fn calculate_length(s: &String) -> usize {
    s.len()
}
```

If we hadn't used `&String`, we would not have been able to use `s1`
later in the main function, as it would have been dropped by
`calculate_length`.

When a function borrows, notice how we must explicitly pass `&s1`.
Likewise, if a function wants to do a mutable borrow, we must explicitly
pass `&mut s1`.

# Mutable References

Rust enforces that a mutable reference is exclusive to any simultaneous
references (immutable or mutable).

If you hold a reference to a value, someone with a mutable reference
could modify or even deallocate fields of the object. This isn't just
about threads. Consider holding a reference to a vector, iterating it,
but then you call some code that clears the vector. Now your iteration
may not be safe to continue. And imagine holding a reference to a value
stored at a position in a buffer that got deallocated by the vector
clear.

Rust prohibits that, since it prohibits simultaneously holding any
reference alongside a mutable reference.

On the other hand, Rust reference enforcement can sometimes make it
impossible to have a "less safe" but more convenient API. That is, *if
you rely only on Rust references*. Sometimes you have to create your
"own" reference-like values to allow for more flexibility (see my
doubly-linked list code).

# No Dangling References

Rust will prohibit any dangling references. As in:

```rust
fn dangle() -> &String {
    let s = String::from("hello");

    &s
}
```

This is obviously unsafe. Rust knows the lifetime of every variable and
of every reference. The reference to a local cannot be passed out
because the lifetime of the reference would outlive the lifetime of the
referred to value.
