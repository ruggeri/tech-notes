## Copy Elision

Say you have:

```
X f() {
  X x;
  return x;
}

X f2() {
  return X();
}

void g() {
  X x = f();
}
```

There is an optimization called *return value optimization*, which
says that `f` can sort of be "passed in" the location where to build
the `X` inside `g`, so that a copy can be avoided. This avoids an
unnecessary copy of `X`. `f` shows "named" return value optimization,
while `f2` shows "regular" return value optimization (this distinction
seems arbitrary, but maybe one is harder to implement than the
other?). The C++ standard allows this optimization (does not require
it). Note: C++ is allowed to do this even if the copy-constructor has
side-effects!  That's very unusual!

Running `g` would involve one construction, and one destruction (at
the end of the scope of `g`). No copies are made.

Basically, we're saying that RVO allows us to "build an object in
place".

However, RVO can fail when an object cannot be built in-place:

```
X f(int i) {
  X x1;
  X x2;

  if (i > 0) {
    return x1;
  } else {
    return x2;
  }
}

void g() {
  X x = f(1);
}
```

Here, `f` doesn't know whether it will be returning `x1` or `x2`. So
it can't know who to be building in-place. So running `g` would
involve:


0. Construction of `x1`
1. Contsruction of `x2`
2. Copy-construction of either `x1` or `x2`
3. Destruction of `x1`
4. Destruction of `x2`
5. Destruction of `x` in `g`

C++11 helps this by bringing in the idea of an "rvalue
reference". Basically, you can write a new constructor `X(X&&)`. This
takes in an "rvalue reference", which is a temporary that you can
modify, with the knowledge that the temporary will never be used
again. In fact, the temporary will not even have its destructor called
if it is "moved" like this.

This gives you power that a copy-constructor `X(X&)` could not have,
since that would need to maintain the validity of the reference you
are constructing from.

## Extern Templates

There's a mechanism now by which you can tell the compiler that a
template will already have been generated in another compilation
unit. You write `extern template class std::vector<MyClass>`. Prolly
requires you to have a special file that instantiates all the versions
you need; that is, you can't simply use extern *everywhere*.

## Initializer Lists

C++03 already had taken the C idea of initializer lists:

```
struct Object {
  float first;
  int second;
}

Object one = { 4.3, 2 };
Object[] many = {{4.3, 2}, {-1.1, 5}};
```

But this only worked on POD in C++03. Now there's a
`std::initializer_list<>` class which can be used to do this anywhere.

## Auto keyword

This will infer the type of the variable. A `decltype(x)` keyword is
also added, so we can compile-time recover the type of an `auto`
declared object.

I think the goal was this transformation:

```
for (std::vector<int>::const_iterator itr = myvec.cbegin(); itr != myvec.cend(); ++itr) {
  // ...
}

for (auto itr = myvec.cbegin(); itr != myvec.cend(); ++itr) {
  // ...
}
```

## Range based looping

In fact, we can go further:

```
for (auto &x : myvec) {
  // ...
}
```

C++11 now allows this range based loop for anyone with appropriate
`begin` and `end` methods.

## Lambdas

Here we can define a function object:

```
[](int x, int y) -> int { return x+y; }
```

We can often elide the return type, which can be auto-inferred.

```
[](int x, int y) { return x+y; }
```

Of course, we needn't return anything, in which case this is used for
its side-effect.

The exciting thing is that we can do closures. That's what the `[]` is
for: `[x, y]() { return x+y; }` captures the variables `x` and `y` by
value, and calling the closure will return their sum.

Of course, closures can outlive their scope. In the above example, I
capture `x` and `y` by value. In fact, future modifications of the
variables won't have any impact on the closure value!

OTOH, we can capture variables by reference: `[&x]() { x += 1 }`.

As a convenience, rather than naming all closed-over variables, we can
simply say `[=]() { ... }`, in which case all closed-over variables
are captured by value. In fact there's a bunch of ways to mix and
match default closure rule and closure rule for specific variables.

## Class Improvements

In C++03, when you write a constructor, if you want to use another
constructor for this class (maybe with some default values) you're
shit out of luck. In C++11, you can put that constructor on the
initialization list.

It's easy to try to override a method, but actually introduce a new
method because your signature is a little off:

```
struct Base {
  virtual void f(float f) { }
}

struct Derived {
  // Oops! Adds a new method
  void f(double d) { }
}
```

For that reason there is now an `override` keyword you can optionally
place after an override declaration. This will check that you are
overriding the method.

They also added a `final` keyword to prevent overrides.

## Enums can now have types

They're not just all integers. For instance: `enum class X {
... }`. This is good at preventing mixing up enums.

## Multithreading

TODO!

## TODO

There are a shit-load of features and I am tired.
