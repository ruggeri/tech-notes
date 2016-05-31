## `static_cast`

`static_cast<Y>(x)` converts an `X` instance to a `Y` instance. A
common use of this is to do an unchecked downcast:
`static_cast<Cat*>(animalp)`. Of course, this may not be safe.

This is also used for conversion between ints and floats.

You could do such a cast via `(Kitten*) animalp`, but this is unsafe,
since C allows you to cast to any kind of object.

## `dynamic_cast`

`dynamic_cast<Y>(x)` converts an `X` to a `Y`, but checks to make sure
that the `X` is truly a `Y` before doing so. Else, it throws an
exception.

For this to work, the `X` class must have RTTI recorded. I believe you
can force this to happen by adding a virtual destructor method.

## `reinterpret_cast`

This is used to literally reinterpret one object as another. For
instance, if you have a byte array representing an object, you might
try to do a reinterpret cast to interpret as the object.

## With Multiple Inheritance

Note that `static_cast` and `dynamic_cast` must sometimes perform
pointer fixups in the presence of
multiple-inheritance. `reinterpret_cast`, in that case, would do the
wrong thing.

## `const_cast`

This removes const `const T* tp; const_cast<T*>(tp)`.

## References

http://www.cplusplus.com/doc/tutorial/typecasting/
