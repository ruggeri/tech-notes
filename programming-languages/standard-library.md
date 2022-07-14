Let's consider the simplest C program compiled on OSX:

```C
#include <stdio.h>

int main()
{
  printf("Hello world!");

  return 0;
}
```

Great. Let's compile it:

```
gcc -std=c17 -Wall -Wextra -pedantic test.c
```

What version of GCC was that?

```
>> gcc --version
Configured with: --prefix=/Applications/Xcode.app/Contents/Developer/usr --with-gxx-include-dir=/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/c++/4.2.1
Apple clang version 13.0.0 (clang-1300.0.29.30)
Target: x86_64-apple-darwin21.1.0
Thread model: posix
InstalledDir: /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin
```

Oh. Looks like gcc is an alias for clang of course. No worries. Anyway,
where do we get the C standard library goodness from?

```
# Note that otool is the OSX version of ldd.
>> otool -L ./a.out
./a.out:
        /usr/lib/libSystem.B.dylib (compatibility version 1.0.0, current version 1311.0.0)
```

Cool. It needs `libSystem`. That combines Mac versions of (1) `libc`,
(2) `libm`, (3) `libpthread`. So that's where your C standard library
functionality is coming from. Also your libm and libpthread
functionality (no need for `-lm` or `-lpthread`).

Let's give an example. `malloc` is defined in the C standard. It's
provided by `libc`. How is `malloc` implemented? It needs the operating
system to give it virtual memory. Presumably it uses something like
`mmap`. That should be available, since OSX is a POSIX compliant system.
Where is that defined? Here it is:
https://opensource.apple.com/source/xnu/xnu-4570.71.2/libsyscall/wrappers/unix03/mmap.c.auto.html.

That references `__mmap`, which is presumably defined somewhere even
deeper. One presumes that it simply performs a system call.

The point is: `malloc` can be implemented using many different
approaches, even when targeting the same system (e.g., you can swap out
the libc malloc for jemalloc probably). But even if you use the same
`malloc` implementation, on different platforms, there may be different
ways to ask the operating system for memory (POSIX ways, and other fancy
ways). Last, even if you're using the same POSIX call, each kernel will
implement this functionality differently (linux vs darwin). Even the
method of invoking a system call can be different on Linux and darwin.

Let's do the same thing, but with C++:

```c++
#include <iostream>

using namespace std;

int main()
{
  cout << "Hello world!" << endl;

  return 0;
}
```

And compile it:

```
>> g++ -std=c++17 -Wall -Wextra -pedantic test.cc
```

Oh yeah, what version?

```
>> g++ --version
Configured with: --prefix=/Applications/Xcode.app/Contents/Developer/usr --with-gxx-include-dir=/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/c++/4.2.1
Apple clang version 13.0.0 (clang-1300.0.29.30)
Target: x86_64-apple-darwin21.1.0
Thread model: posix
InstalledDir: /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin
```

Cool. Let's check what libraries are linked against:

```
>> otool -L ./a.out
./a.out:
        /usr/lib/libc++.1.dylib (compatibility version 1.0.0, current version 1200.3.0)
        /usr/lib/libSystem.B.dylib (compatibility version 1.0.0, current version 1311.0.0)
```

Cool. What is libc++? This is where C++ specific standard functionality
is defined. Some of this functionality can be written in terms of
`libc`. But other stuff, like atomics, probably needs support from the
operating system.

Okay, let's compile using GCC.

```
[NedBookPro2020] | ~$
>> gcc-9 -std=c2x -Wall -Wextra -pedantic test.c
[NedBookPro2020] | ~$
>> otool -L ./a.out
./a.out:
        /usr/lib/libSystem.B.dylib (compatibility version 1.0.0, current version 1281.100.1)
        /usr/local/lib/gcc/9/libgcc_s.1.dylib (compatibility version 1.0.0, current version 1.0.0)
```

What do we see here? We see that we still link the OSX libSystem. Of
course it needs that, because that's where `malloc` and friends are
defined. It also links `libgcc_s`, which is where some GCC specific
functionality is defined. For instance, GCC has an extension for long
long division, and this needs runtime support. But the `libgcc_s`
functionality relies on the presence of a C standard library, which is
provided by `libSystem`.

Source: https://unix.stackexchange.com/questions/1812/what-does-libgcc-s-so-contain

Let's try the same for G++.

```
[NedBookPro2020] | ~$
>> g++-9 -std=c++17 -Wall -Wextra -pedantic test.cc
[NedBookPro2020] | ~$
>> otool -L ./a.out
./a.out:
        /usr/local/opt/gcc/lib/gcc/9/libstdc++.6.dylib (compatibility version 7.0.0, current version 7.28.0)
        /usr/lib/libSystem.B.dylib (compatibility version 1.0.0, current version 1281.100.1)
        /usr/local/lib/gcc/9/libgcc_s.1.dylib (compatibility version 1.0.0, current version 1.0.0)
```

As before, this also includes a specific `libstdc++`, written in terms
of a C standard library. In this case, it relies on `libSystem`, as
ever.

I want to note: `libstdc++` seems to incorporate another library called
`libatomic`. This is where the atomics functionality comes from. It
looks like `libatomic` is meant to support a variety of systems.
`libatomic` presumably _cannot_ be written just in terms of `libc`. The
build process for `libatomic` (which is stored in
`/usr/local/Cellar/gcc/9.3.0/lib/gcc/9`) presumably looks at what system
is being targeted. If the system is POSIX, then it can presumably rely
on POSIX atomic functionality. It will probably even want to know what
kind of hardware we are using; maybe there is hardware specific things
it can use.

This brings me to a final question: what if we don't like some aspects
of `libSystem`? Can we replace parts of it?

Let's start easy and try to see about how we can use two different
versions of readline.

```
[NedBookPro2020] | ~$
>> gcc -std=c2x -Wall -Wextra -pedantic test.c -lreadline
[NedBookPro2020] | ~$
>> otool -L ./a.out
./a.out:
        /usr/lib/libedit.3.dylib (compatibility version 2.0.0, current version 3.0.0)
        /usr/lib/libSystem.B.dylib (compatibility version 1.0.0, current version 1311.0.0)
```

Okay, that uses `libedit`, provided as part of the Mac system. Does it
matter that we used the base `gcc`? Nope:

```
[NedBookPro2020] | ~$
>> gcc-9 -std=c2x -Wall -Wextra -pedantic test.c -lreadline
[NedBookPro2020] | ~$
>> otool -L ./a.out
./a.out:
        /usr/lib/libedit.3.dylib (compatibility version 2.0.0, current version 3.0.0)
        /usr/lib/libSystem.B.dylib (compatibility version 1.0.0, current version 1281.100.1)
        /usr/local/lib/gcc/9/libgcc_s.1.dylib (compatibility version 1.0.0, current version 1.0.0)
```

Okay, let's try to use a different readline:

```
>> gcc -std=c2x -Wall -Wextra -pedantic test.c /usr/local/Cellar/readline/8.1.2/lib/libreadline.dylib
[NedBookPro2020] | ~$
>> otool -L ./a.out
./a.out:
        /usr/local/opt/readline/lib/libreadline.8.dylib (compatibility version 8.1.0, current version 8.1.0)
        /usr/lib/libSystem.B.dylib (compatibility version 1.0.0, current version 1311.0.0)
```

Cool! That's definitely linking the new version. But I probably want to
use functions like `rl_forward_byte`. To include the header that defines
this, I better toss on `-I/usr/local/Cellar/readline/8.1.2/include`.

Where are the headers of OSX libraries stored? Glad you asked:

    /Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX12.1.sdk/usr/include

Here you'll find `readline/readline.h`, but it's just a symlink to
`editline/readline.h`. And that doesn't define `rl_forward_byte`, since
that's readline-specific.

BTW, most standard libraries live at

    /Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX12.1.sdk/usr/lib

There is actually no file `/usr/lib/libedit.3.dylib`. There isn't even a
symlink. Some fanciness must know to look in the SDK to find this?

Okay, that shows us how to replace some functionality that ships with
the OSX system. But can we replace parts that would be defined in the C
standard library? Yeah maybe:

```c
// my_fake_malloc.c
#include <stddef.h>

// never return memory
void* malloc(size_t sz) {
  return 0;
}

// main.c
#include <stdio.h>
#include <stdlib.h>

int main() {
  void* ptr = malloc(123);

  printf("%p\n", ptr);

  return 0;
}
```

Now let's compile:

```
>> gcc -c my_fake_malloc.c -o my_fake_malloc.o
>> gcc -o main.o main.c my_fake_malloc.o
>> ./main.o
0x0
```

Freaky! However, I bet we won't replace calls to malloc in dynamically
linked libraries. We'll write:

```c
// my_real_malloc.h
void* real_malloc(size_t sz);

// my_real_malloc.c
#include <stdlib.h>

// never return memory
void* real_malloc(size_t sz) {
  return malloc(sz);
}
```

Then we'll build it shared style:

```
>> gcc -o my_real_malloc.o -fpic -c my_real_malloc.c
>> gcc -o my_real_malloc.so -shared my_real_malloc.o
```

Now, we'll update our `main.c`:

```c
#include <stdio.h>
#include <stdlib.h>

#include "my_real_malloc.h"

int main() {
  void* ptr = malloc(123);
  printf("%p\n", ptr);

  void* ptr2 = real_malloc(123);
  printf("%p\n", ptr2);

  return 0;
}
```

And let's build and run!

```
>> gcc -o main.o main.c my_fake_malloc.o my_real_malloc.so
>> otool -L main.o
main.o:
        my_real_malloc.so (compatibility version 0.0.0, current version 0.0.0)
        /usr/lib/libSystem.B.dylib (compatibility version 1.0.0, current version 1311.0.0)
>> ./main.o
0x0
0x600003a88080
```

We see that we can override standard library functionality within our
own code, but it doesn't quite work for dynamically linked code...
