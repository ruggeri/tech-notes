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

Here's an example of using `std::move`, which is a cast that takes a
reference and makes it an rvalue reference, so that you can call a
move constructor.

```
template <class T>
swap(T& a, T& b) {
    T tmp(std::move(a));
    a = std::move(b);
    b = std::move(tmp);
}

//Equivalent, with static cast!
template <class T>
swap(T& a, T& b) {
    T tmp(static_cast<T&&>(a));
    a = static_cast<T&&>(b);
    b = static_cast<T&&>(tmp);
}
```

This here avoids the use of any assignment constructors, using only
move constructors.

## auto_ptr vs unique_ptr

* `unique_ptr` has a move constructor, but not a copy constructor. It
  has an assignment constructor, but it only takes an rvalue
  reference.
    * This prevents accidental use of a unique ptr after it has been
      transfered.
* `auto_ptr` existed before move constructors, so it used copy
  construction to transfer ownership.
    * The original `auto_ptr` was resest to NULL, but this allowed for
      possible null pointer problems.
* Indeed, I think that `unique_ptr` was invented entirely to prevent
  this silent stealing of a ptr from you, leaving you a null
  unexpectedly.

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

They also introduce *uniform initialization*: `MyClass mc{arg1,
arg2}`, which is an alternative to `MyClass mc(arg1, arg2)`. This
makes it more even more like C, where structs can be initialized this
way. This is called *uniform*, since you can initialize a struct and
class the same way.

## Auto keyword

This will infer the type of the variable; it just looks at what the
variable is initialized with, so it's not that sophisticated. A
`decltype(x)` keyword is also added, so we can compile-time recover
the type of an expression. I see this as being useful in template
code, prolly.

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
initialization list. You can also "import" base class constructors to
a subclass, if your subclass just wants to construct itself in the
default way of the base class.

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

You can also now explicitly tell C++ to not generate certain
constructors (assignment, copy) with the `delete` keyword. You can
tell C++ to generate the default constructor even if you add other
constructors with the `default` keyword.

They've had an `explicit` keyword since C++98 to prevent improper
conversions. For instance, you may not want `myClass == 1` to compile,
but if `MyClass` has a constructor with a single int, this will! You
can mark that as `explicit`. C++11 takes this up a notch, so that you
can mark *conversion functions* as explicit: for instance, `explicit
operator bool()`. Then you can say `if (myClass)` without worrying
about `if (myClass == myClass2)` converting both sides of `==` to bool
and then comparing them true. This is a minor change, but I put it
here because C++ is so fucked.

## Enums can now have types

They're not just all integers. For instance: `enum class X {
... }`. This is good at preventing mixing up enums.

## Polymorphic Function Wrappers

Used to be a major PITA to try to assign a `bool function(long, long)`
to a `bool function(int, int)`. If you try to do this with function
pointers, you'll still get yelled at. But if you use `std::function`,
it will be okay when the parameters/return type are convertable.

## Tuple Types

In the old days you had `Pair<X, Y>` and `std::make_pair`. Now they
let you do:

```
auto record = std::make_tuple("Ned", "Ruggeri", 29, true);
// Still have kinda bullshit record access...
cout << std::get<0>(record) << "\n";
```

Still, this could be pretty convenient at times!

## Multithreading

* Had to define a memory model.
* Can define thread local storage via `thread_local`.
* Added `std::thread(fn_obj, ...args)`. Added `#join` method.
    * I think `thread` is considered very low level for common use.
    * One problem is exception safety; an exception thrown in another
      thread will kill the whole program.
    * If you don't call `join` you have to call `detach` to release
      the resource? Why isn't that just done in the destructor??
* Added `std::mutex` and `std::condition_variable`.
    * `std::mutex` can be simple: you use `#lock` and `#unlock`.
    * This can make exception safety difficult, so you can use a
      `std::lock_guard`.

```
struct Counter {
  std::mutex mutex;
  int i;

  void increment() {
    // lock_guard releases mutex on destruction
    std::lock_guard<std::mutex> guard(mutex);
    i += 1;
    // Any other code that might throw an exception...
  }
}
```

* There's a recursive mutex if you need a function to be re-entrant.
* To use a condition variable (for instance, you want to remove an
  item from a queue, but need to wait until there is one to remove).
    * I have an example of that in another file.
* There are futures and promises.
    * `std::future` is returned by an async operation to the caller.
    * `std::promise` is used by the worker thread to set the value.
    * Since `std::promise` has a `get_future` method, I think the
      intent is to create a promise, and then pass this to a user.
    * I believe that `std::async` basically does this for you.
    * Finally there is `packaged_task`, which packages up a functor;
      you can get the future from the task; and give another thread
      the task, which they can start executing at their leisure.
    * I.e., unlike `std::async`, `packaged_task` does not begin
      execution immediately.
* Last are atomics, so that you don't have to do locking. But I'm lazy
  and won't review this right now.

## Randoms

* Regex added to language.
* unordered_set added to language.
* `MyClass<MyClass<int>>` finally doesn't try to use the shift
  operator.
* Static assertions so you can assert properties at compile-time. For
  instance, maybe that a template type argument must not have too big
  a size.
