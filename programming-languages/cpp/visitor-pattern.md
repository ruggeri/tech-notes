## C++ Union

You know:

```cpp
union X {
  int32_t a;
  uint16_tt b[2];
  uint8_t c;
}
```

Now, can you have a destructor for a union? Holy Moses you can:

```cpp
#include <iostream>
#include <string>
#include <vector>

union S
{
    std::string str;
    std::vector<int> vec;
    ~S() {}
};

int main() {
    S s = {"Hello, world"};
    // at this point, reading from s.vec is undefined behavior
    std::cout << "s.str = " << s.str << '\n';
    s.str.~basic_string();
    new (&s.vec) std::vector<int>;
    // now, s.vec is the active member of the union
    s.vec.push_back(10);
    std::cout << s.vec.size() << '\n';
    s.vec.~vector();
}
```

Notice the explicit calls to the destructors, and the explicit use of
"placement new." Note that you *must* define a destructor here, since
the typical destructor is deleted. Wow.

C++11 has a "variant" class. Presumably it just keeps an unsigned int
which remembers which of the variants is currently being used.
`std::get` will get the value out, but throws an exception if not in the
right "mode". Wowzo.

```cpp
#include <variant>
#include <string>
#include <cassert>

int main()
{
    std::variant<int, float> v, w;
    v = 12; // v contains int
    int i = std::get<int>(v);
    w = std::get<int>(v);
    w = std::get<0>(v); // same effect as the previous line
    w = v; // same effect as the previous line

//  std::get<double>(v); // error: no double in [int, float]
//  std::get<3>(v);      // error: valid index values are 0 and 1

    try {
      std::get<float>(w); // w contains int, not float: will throw
    }
    catch (const std::bad_variant_access&) {}

    using namespace std::literals;

    std::variant<std::string> x("abc");
    // converting constructors work when unambiguous
    x = "def"; // converting assignment also works when unambiguous

    std::variant<std::string, void const*> y("abc");
    // casts to void const * when passed a char const *
    assert(std::holds_alternative<void const*>(y)); // succeeds
    y = "xyz"s;
    assert(std::holds_alternative<std::string>(y)); // succeeds
}
```

This post describes the visitor pattern:

  https://stackoverflow.com/questions/255214/when-should-i-use-the-visitor-design-pattern

The idea is described well. Say you want a way to add new methods to an
existing set of classes, without modifying those classes. In that case,
you can add a virtual method called `letsDo(Operation* o)` to each of
those classes. `Operation* o` is your operation you want to add.

Then, you have the `letsDo` implementation for `Robot` call
`o->hereIsARobot(this)`. And the `letsDo` for `Kitten` calls
`o->hereIsAKitten(this)`. Note that `Robot` and `Kitten` need not be
part of the same hierarchy at all.

C++11 has a `std::visit` function, which works on `std::variant`. It is
described here:

  https://en.cppreference.com/w/cpp/utility/variant/visit

```cpp
// the variant to visit
using var_t = std::variant<int, long, double, std::string>;

// helper type for the visitor #3
template<class T> struct always_false : std::false_type {};

// helper type for the visitor #4
template<class... Ts> struct overloaded : Ts... { using Ts::operator()...; };
template<class... Ts> overloaded(Ts...) -> overloaded<Ts...>;

int main() {
    std::vector<var_t> vec = {10, 15l, 1.5, "hello"};
    for(auto& v: vec) {

        // 1. void visitor, only called for side-effects (here, for I/O)
        std::visit([](auto&& arg){std::cout << arg;}, v);

        // 2. value-returning visitor, demonstrates the idiom of returning another variant
        var_t w = std::visit([](auto&& arg) -> var_t {return arg + arg;}, v);

        // 3. type-matching visitor: a lambda that handles each type differently
        std::cout << ". After doubling, variant holds ";
        std::visit([](auto&& arg) {
            using T = std::decay_t<decltype(arg)>;
            if constexpr (std::is_same_v<T, int>)
                std::cout << "int with value " << arg << '\n';
            else if constexpr (std::is_same_v<T, long>)
                std::cout << "long with value " << arg << '\n';
            else if constexpr (std::is_same_v<T, double>)
                std::cout << "double with value " << arg << '\n';
            else if constexpr (std::is_same_v<T, std::string>)
                std::cout << "std::string with value " << std::quoted(arg) << '\n';
            else
                static_assert(always_false<T>::value, "non-exhaustive visitor!");
        }, w);
    }

    for (auto& v: vec) {
        // 4. another type-matching visitor: a class with 3 overloaded operator()'s
        std::visit(overloaded {
            [](auto arg) { std::cout << arg << ' '; },
            [](double arg) { std::cout << std::fixed << arg << ' '; },
            [](const std::string& arg) { std::cout << std::quoted(arg) << ' '; },
        }, v);
    }
}
```
