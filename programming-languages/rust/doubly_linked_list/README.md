The goal of this project was to experiment with the "arena" idea. You do
need to pass around something like a `NodeId`, because otherwise no one
can hold any mutable Rust reference into the list so long as anyone else
has any other reference.

Basically, it seems like Rust will protect you from *memory unsafety*.
You can't do anything that will deallocate memory underneath a reference
you still hold. But if you want to allow someone to delete a link from
the list while someone else might still hold a reference to that link,
then you cannot avoid the possibility of deleting something from
underneath someone.

This is part of your API. You must handle the possibility: either with
"try" `Option`/`bool` methods that report failure if the reference is no
longer live, or methods that assume the reference is live, but `panic!`
if necessary.

The key learning is: you will tie yourself in knots with Rust references
if you use them. But Rust references aren't the beginning/end to
*handles*. You can't expect *too* much from Rust reference safety; it
can't both (1) prohibit *every* kind of safety/reference misuse, AND
(2) be flexible enough to support any reasonable API.

In the end, I'm not sure this Rust code is more complicated than correct
code in a language like Go or Java.

# TODOs

- Write a `Cursor` and `MutCursor` implementation.
