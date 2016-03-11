## M4/Autoconf/Automake

Here's the story.

M4 is a macro processor. It just replaces text with other text. It is
fairly powerful though.

Autoconf is a tool which will generate a `configure` script for
you. You write `configure.ac`, which you then run autoconf on. I
believe that autoconf embeds M4 under the namespace `m4_*`. Autoconf
primarily checks for features needed for the compilation of your code.

At the same time, you want to write a complex Makefile. You write a
`Makefile.am` file, which is processed by automake (using
`configure.ac`). The output is a `Makefile.in`. The `configure` script
then converts the `Makefile.in` to a `Makefile`.

At the same time, you often want to have a `config.h` generated, with
various constants defined. This is started by `autoheader`, which
reads your `configure.ac` and produces a `config.h.in`. You can modify
this. It will be finalized when you run `./configure`, with a
`config.h` being produced.

You can write `autoconf.ac` by hand, but you may also want to run
`autoscan`, which produces `autoconf.scan`. This basically looks for
potential protability issues and asks autoconf to inquiry about those
variables. You can `diff autoconf.scan autoconf.ac` every once in a
while to see what it has added.

## Configure Options

* `build` specifies the machine doing the building.
* `host` specifies the machine where the application will run.
* `target` specifies the target architecture of the compiled
  application (typically for building compilers that will target a
  given system; prolly not necessary to use)

Other options are typically
`--enable-feature`/`--disable-feature`. For headers/dynamic libraries
to link, you might use `with-package=/usr/local/...`.

## Using Autoconf

Here's a basic `configure.ac`:

```
AC_PREREQ(2.69)

AC_INIT([foo], [2.0])
AM_INIT_AUTOMAKE([1.15])

AC_PROG_CC

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
```

This is basically just saying you need a C compiler. Here's a basic
`Makefile.am`:

```
bin_PROGRAMS = foo
foo_SOURCES = main.c foo.c foo.h
```

This is saying build `foo` with the given deps. Run `aclocal;
autoconf`. The first command just copies in M4 definitions
locally. Then autoconf should generate `configure`.

You can then run `automake --add-missing`. This will build your
`Makefile.in`, and also copy over default files like
`COPYING`. Basically, it wants you to do a typical GNU setup.

Having done this, you should be able to run `./configure` on a user's
machine to generate the Makefile.

Autoconf requirements are typically of the type:

* Check for programs (like gcc).
* Check for libraries (like libm).
* Check for headers (like `pthread.h`).
* Check for typedefs/structs (typically inside of headers).
* Check for functions.

Note that while you can put any M4 macro into the autoconf, you can
also put any Bourne shell code, because that is what is being output
to `configure.sh` at the end.

**How to generate configuration header file??** Autoheader?

## Automake

There are *primaries* like `_PROGRAMS`, `_SOURCES`.
