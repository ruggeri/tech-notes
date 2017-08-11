# The CPP Programming Language

**Basics/Essentiails**

* Hello World
    * `iostream`, `std::cout`, `cin`.
    * Use `vector` to read in several items, and then print them back.
    * Use range based for iteration. When ranging use a reference.
    * `auto` keyword
* Write your own IntVector
    * namespaces
    * Constructor and destructor. Use curly brace initializers.
    * Methods
    * Use `new` to allocate space on the heap.
    * Operator overloading for `operator[]`
    * `delete` vs `delete[]`: it's about destructors!
    * Implement `begin` and `end` with pointers for range based
      iteration.
    * Implement non-standard copy and assignment constructors.
* Templatize your vector
    * Header guards.
    * `tpp` file needs to be included in header.
* Inheritance: Animals and Cats and Dogs
    * Virtual base class for Animal: pure virtual methods.
    * Can pass Cat or Dog to method that takes an Animal reference or
      pointer.
    * Need a virtual destructor. Else the Animal destructor might be
      used.
    * Always use `override` to ensure you are properly overriding. Use
      `virtual` keyword even in subclass, so further derivation is
      clear.
    * Note: all Ruby methods are effectively virtual.
    * vtbl implementation.

**Move Constructors**

What if you assign a large datatype which is a temporary? You don't
want to do a copy. You want to sort of "move" the temporary into space
allocated by a variable.

Return-value optimization captures many use cases, but there are
others where it fails. See my C++11-features document.

One way is to write a copy constructor that takes in a reference. The
new vector can take control of the pointer provided by the old
vector. To make sure two vectors don't think they own the same store,
you must clear out the argument vector's store field.

Now, C++ won't let you pass in temporary variables to such a reference
copy-constructor. It would if this were a `T(const T&)` constructor,
because then you promise not to modify `T`. But the point is we *do*
intend to modify the argument!

The reason C++ doesn't let you is because it doesn't want you to
mistakenly modify temporaries. That seems a little odd because you can
actually call non-const methods on a temporary... But I think they
don't want you to get too far removed from the temporary.

We have an even bigger problem: when we want to copy a temporary, we
can mutate the temporary to perform a "move". But when we want to copy
a stored value, we need to do a "true copy". That is: `vector v2 = v1`
requires a copy, while `vector v3 = v2 + v1` shouldn't require a copy.

To distinguish the cases, we can use `T(const T&)` and `T(T&&)`. For
lvalues, which are things that can be assigned to, we will use the
former. Rvalues, which are those things not lvalues, use the
later. Note that `T&&` means rvalue reference and not "T reference
reference".

**`std::move` and swap**

C++ will be careful to use the possibly-destructive move constructor
only when it is safe: when you have an rvalue reference.

Sometimes you want to force the use of the move constructor. The
prototypical case is `std::swap`.

    template<typename T>
    void swap(T& t1, T& t2) {
      T t3 = t1;
      t1 = t2;
      t2 = t3;
    }

This results in several full-copies. First, `t3` must copy `t1`. But
then when `t1 = t2`, this also does a copy! And finally for `t2 = t3`!

    template<typename T>
    void swap(T& t1, T& t2) {
      T t3 = static_cast<T&&>(t1);
      t1 = static_cast<T&&>(t2);
      t2 = static_cast<T&&>(t3);
    }

And an alias for this is `std::move`.

**Why references??**

References seem like a lot of trouble. But they do make some things
nice. Here are a few.

First, swap can just take in variable names. We don't need to take a
pointer to pass something by reference. References can never be null,
nor can they point to the start of an array of object, so they are
less ambiguous. It is clear that you are not transferring ownership by
giving a reference.

In cases where you do intend mutation to be performed (e.g., swap), a
pointer seems like maybe not a bad idea. But if it's just about
*performance*, that seems silly. For instance, let's say I write:

    Matrix operator+(Matrix* m1, Matrix* m2);

Now I need to take addresses just so I can avoid copies of the
arguments. That seems silly.

Last example. Returning an lvalue reference lets someone modify your
object easily. For instance, a vector might implement:

    T& operator[](size_t idx);

This way people can write `v[123] = 456;`. The equivalent way to do
this would be to return a pointer, but again that is burdensome.

**unique_ptr**

There used to be `auto_ptr`. When you copied it, the original had its
memory set to null. That could be unexpected and result in NPE. The
`unique_ptr` now only has a move constructor, so that copies are
forbidden under circumstances where a null could be left.

This matters because consider:

    vector<auto_ptr<X>> v;
    v.push_back(new X());
    auto_ptr<X> xp = v[0]; // just nulled out the auto_ptr in the vector

`unique_ptr` solves the problem by disallowing this because `v[0]` is
not an rvalue-reference and there is no lvalue-reference constructor
for a `unique_ptr`.

`shared_ptr` also exists, but it is reference counted and way more
sophisticated.

Now a Rust-type question: how do I pass a `unique_ptr` so someone can
use it, but I'm still the owner? For instnace:

    unique_ptr<X> x;
    f(x); // you can borrow my x
    *x; // I still own x

The answer is to make sure that `f(unique_ptr<X>& x)`: it takes a
reference to a `unique_ptr`.

One last thought. You can say `make_unique<X>(arg1, arg2)` and now you
didn't even call `new` yourself!

**weak_ptr**

This is for weak references. You can upgrade them to a shared_ptr by
calling `lock()`, but this may give you a null pointer of course.

How is this implemented? Here's the idea (I think). A `shared_ptr` has
a reference to the object, and a *control block*. The control block
has the (1) use count and (2) weak ptr count.

When the use count hits zero, the managed data is deleted. But the
control block *is not*. When the weak ptr count hits zero, then the
control block is delted.

The weak ptr can know the managed object is deleted by seeing that use
count is zero.

**Containers of references**

We can write a container which stores values. We can write a container
which stores `unique_ptr`, in which case it is responsible for
deletion. Same for `shared_ptr`, in which case it shares
responsibility.

If we want a container that *doesn't* own the contents, then we can
have a container of `T*`. But that kinda sucks, because this makes
everyone deal with pointers now. Also: if we give this container to
someone, do they know whether they are being given ownership of these
things?

It might be nice to be able to have a container of `T&`, but this is
not possible. I believe the problem is that when a container tries to
shuffle around `T&`, it will be writing to these, which will result in
mutations of the underlying objects, which is not what is intended.

What you want is to use `std::reference_wrapper`. This is basically
like a pointer, but it can implicitly convert to a `T&`. That gives
you the convenience of storing references. Note that assignment to a
`reference_wrapper` results in a rebinding, so:

    v[idx] = t;

changes what `v[idx]` refers to; it does not mutate the `T` object
that `v[idx]` refers to. That is different from: `cat_vector[idx] = {
.name = "Gizmo" };`, which does change the cat at that position.

This is one advantage to pointers, which are maybe clearer:

    cat_ptr_vector[idx] = &gizmo;
    *cat_ptr_vector[idx] = gizmo;

Are different!

**Conversions and Operators**

You can define `operator int()` so that your class can be converted to
an int. But this can cause problems where implicit conversions are
done, so you may prefer `explicit int()`. Likewise, you may prefer
`explicit T(int)`, so that ints are not implicitly converted!

When do you use a member function for an operator, vs an outside
nonmember operator? One possibility is like this. Say you want to
concatenate vectors. You write `vec1 + vec2 + vec3`. You don't want to
create many temporaries. Then, you can write

    Vec operator+(Vec&, Vec&)
    Vec& operator+(Vec&&, Vec&)
    Vec& operator+(Vec&, Vec&&)

Here, what this lets you do is "reuse" one of the vecs if it is an
rvalue.

Another note: you need nonmember operators if you want to be able to
do `int + MyComplexClass`.

**Derived Classes**

If you want to override, suffix with `override` to make sure you match
a virtual function. You can use `final` if you don't want anyone to
override. They note that a `final` override in `T` can increase
performance when calling the method on a `T*` because that can be
dynamically dispatched.

Non-virtual functions cannot be overrided, only hidden.

If you do multiple inheritance, you might still implement an abstract
base class as a `virtual` derivation, so there is only one copy of
this.

They note that you can provide overrides that are resolved via dynamic
resolution. Like `f(Shape*)`, `f(Circle*)`, `f(Square*)`.

**Lambdas**

* We wrote versions of `each` and `map` for the vector. This involved
  `decltype` and `auto`.
* Talked about how lambdas are allocated.
* It's worth noting that if you use `[=]`, it will capture the `this`
  pointer *by-value*. So it will *not* copy fields by value if you say
  `myField`, because it means `this->myField`. So the best thing to
  do, according to Scott Meyers, is to always explicitly list
  captures.
* They mention *init* captures. They work like this:
  `[value = whatever]`. This is particularly useful for moving:
  `[value = std::move(value)]`. This is C++14, else you could use
  `std::bind` and pass it in as a parameter.

**Bind**

`std::bind` will partially apply a function. The way you can specify
unbound arguments is using placeholders:

    int plus(int, int);
    auto f1 = bind(plus, 3);
    f1(123);
    auto f2 = bind(plus, _1, 3);

There is also a function called `std::mem_fn`. Calling a pointer to a
member function works like this: `(objp->*member_functionp)(arg1,
arg2)`. By using `mem_fn` this is simple.

But of course these can be done with lambdas...

**Member Type Aliases**

It is common, so that generic code can be easily written, to do
something like this:

    template<typename T>
    class Vector {
      using value_type = T;
      // ...
    }

First, this lets me use `value_type` everywhere in preference to
`T`. But also, let's say I want to write a method that works on
any kind of enumerable thing:

```
template<typename Enumerable>
auto get(Enumerable& e, int idx) -> typename Enumerable::value_type {
  return e[idx];
}

vector<int> is{1, 2, 3, 4};
cout << get(is, 3) << endl;
```

See how this uses `typename`, which is needed to access member types?
I believe I could also do `decltype(e[idx])`. Yet a more verbose way
would be:

    decltype(declval<Enumerable>()[declval<int>()])

Becausde that type is hard to write:

    template <typename Enumerable>
    using EnumerableValueType =
      decltype(declval<Enumerable>()[declval<int>()]);

Last: I think `auto` could have figured this out at the definition,
but not at a declaration, I believe.

**Hiding**

You can redefine methods in derived classes. This is not an *override*
of a virtual method. This is *hiding*. The only valid use case is if
you can write a specialized version to use when you know the exact
type of the object, but that the original version is still okay to
call. That's because:

    Base* b = new Derived();
    b->f();

will call `Base::f` instead of `Derived::f`. Note that you can't just
simply add new overloads in the derived class; even one redefine in
the derived class hides all the previous definitions in the base, even
if the types are different. You need to specifically import (`using
Base::f`) all the `Base` overloads, or selectively do this for those
overloads you want in the derived class.

**Type Erasure/Contravariance**

You can't pass a vector of square pointers to a function that takes a
vector of shape pointers. The problem is that you might try to store a
circle there. Moreover, a template instantiation might have been
*specialized* or otherwise had its methods optimized in a way that
makes it not possible to substitute. For instance, a `vector<Shape*>`
and a `vector<Square*>` could be implemented in totally different
ways!

A solution is like so:

    template <typename T>
    void sayHello(vector<T*>& shapes) {
      for (auto shape : shapes) {
        shape->speak();
      }
    }

But this doesn't work for *virtual* methods, because virtual methods
must not be templated. The reason is presumably that implementation
would be hard: if you use a template function in the derived class,
the compiler needs to generate a space in the base classes' vtable,
and also every other implementation. Java gets around this because
type erasure.

**Slicing**

There is trouble when you try to slice an inherited class:

    Derived d1;
    Derived d2;
    Base& b = d1;
    b = d2;

Note that this will call `Base::operator=`, and will not fully assign
from `d2`, unless the assignment operator is virtual, which it
probably should be. I would say this: if you have any virtual method,
you should have virtual `operator=`. If you have no virtual functions,
then this feels less bad.

**PIMPL**

The C++98 idea is this:

```
// .h
// include like no headers
class Widget {
 public:
  // Constructor declaration, methods
 private:
  struct Impl;
  Impl* pImpl;
};

// .cpp
// include many headers

struct Impl {
  // Bunch of fields.
};

Widget::Widget() : pImpl(new Impl()) {}
Widget::~Widget() { delete pImpl }

// Methods now use pImpl rather than this to access fields.
```

Using PIMPL you don't need to include headers (except insofar as you
need argument/return types, I guess.

For C++11, you can use a `unique_ptr`, but because the type is
incomplete, you'll need to declare `Widget::~Widget()` in the header
file; else C++ will try to inline generate the default, which can't
work because the type is incomplete and you can't destruct an
incomplete type. But in the CPP file you write `Widget::~Widget() =
default`, like normal. Here you finally have the complete type.

## Todos

* Placement new for backing store of array.

## Resources

* ISOC++ FAQ (formerly Parashift) (reviewed)
* C++ Programming Language (reviewed)
* Effective C++
    * **TODO** Review again?
* Effective Modern C++ (reviewed)
* C++ Concurrency in Action (reviewed)

## TODO

* Review Modern C++ Design by Andrei Alexandrescu.
