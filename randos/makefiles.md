Rules are like

```
all: foo
foo: foo.o bar.o

.c.o:
	$(CC) $(CFLAGS) -c $< -o $@
```

It should be clear that `$<` is the sourcefile and `$@` is the object
file (this is a *suffix rule*). The target will be rebuilt whenever a
dep has a more recent modification timestamp than the target. The
first rule is the "default rule". Now-a-days, we prefer pattern rules
(see below) to suffix rules; pattern rules are more powerful and
precise. Suffix rules are obsolete and only exist still for
compatability.

It is common to use variables:

```
OBJS = main.o kbd.o command.o display.o
all: $(objs)
	$(CC) $(CFLAGS) -o edit $(OBJS)
```

Sometines you clearly define something as a *phony target* by `.PHONY:
clean`. This says not to be confused by a phony file; if a file had
been there, since clean has no prereqs we would consider it up to date
and not run the rule. Also, we prefix `-rm edit $(OBJS)` with a `-` to
suppress errors.

You can `include` a makefile within a makefile. Backslashes are
frequently used either dependencies or commands into multiple
lines.

Typically you have one source prerequeisite, followed by several `.h`
prereqs. `$<` just means the first prereq (the source file), whereas
`$^` would be the list of prereqs. You might prefer `$<` to only
compile the source file (the headers will be `#include`'d).

Likewise you can have more than one target. This is possible to do if
you use the `$@` var, which will inject the target being built into
the rule. But I don't see a lot of use for this...

You can have multiple rules for one target. The prereqs are
merged. Only one recipe will be run: the last one. If you specify
multiple recipes, you'll get a warning. An example:

```
objects = foo.o bar.o
foo.o : foo.c
bar.o : bar.c
$(objects) : config.h
```

Static patterns are like this:

```
objects = foo.o bar.o
$(objects) : %.o : %.c
	$(CC) -c $< -o $@
```

Basically, the `%.o` says strip off the `.o` from the target, so that
you can append the `.c` afterward. This is a lot like *implicit
rules*, which we'll cover later. But this is just a little more
specific.

You can use `define method = ... endef` to define *canned recipes*,
which are sort of like lambdas. Typical variables are defined `xyz =
abc`, but `xyz := abc` (or, equivalently, `xyz ::= abc`) is expanded
only once at the time it was written. It's basically a different level
of quoting. You can also set `xyz ?= abc` to set `xyz` if it isn't
defined yet. You can also use `xyz += abc` to extend a
variable. `override CFLAGS += -g` allows you to add the `-g` option to
the CFLAGS specified on the command line arg.

Inside interpolation expressions there are a number of functions you
can call to manipulate text. It's not that interesting so I don't
describe it here. Basically, function call is like: `$(fn arg1,
arg2)`. I guess the most important is `(sources:.c=.o)`, which does a
replcaement of a suffix. There's a `foreach` function to perform an
operation to each word in a string; there are never any "arrays", just
whitespace separated words.

We've seen how recursive expansion lets you have "canned recipes". You
can also have "functions" by using `(call
fname,var1,var2,x...)`. Then, when expanding `fname`, the vars are set
to `$(1), $(2),...`. NB: it is common strip whitespace out of
subsequent args since this can add whitespace that changes semantics
in the fn.

You can have conditionals `ifeq/else/endif`. They're pretty limited in
scope just `ifeq/ifneq/ifdef/ifndef`. It looks like these can't use
any automatic (rule scope) variables; so they're only evaluated
once. However, you can use them inside a rule to control behavior
based on flags.

Another idea is `eval`, which can be used to generate *makefile*
code. That means if the result of your macro/function is Makefile
rules, these can be parsed and used by Makefile. This allows you to
templatize your build code.

They talk a little bit about how you can autogenerate prerequisites:
this is tedious for keeping up to date with a list of header
files. You can use `gcc -MM main.o` to generate `main.o: header1.h
header2.h`. They recommend you have a makefile command for every `.c`
file generate a `.d` file with these auto-gen deps. Then you also do
an `include $(sources:.c=.d)` to pull in all these defs. They also
like to modify the `.d` to also dep on the `.c` file, too. This sounds
*hard-core*; maybe automake has a way?

There are a lot of options pertaining to recursive calls of make.

An obvious failure of make is that its notion of what needs to be
rebuilt is pretty simplistic. That might trigger a lot of unnecessary
compilation (e.g., if a trivial change is made to a header).

Useful hint: when building your own development code and want as many
errors as possible, rather than quit early, use `make -k` to "keep
going" and build other targets that need to be rebuilt.

Implicit rules are common built-in rules. For instance, if you have a
rule for `foo: foo.o bar.o: cc -o foo foo.o bar.o`, but no rules for
`foo.o` or `bar.o`, then Make will use its implicit rule. You can even
say `foo.o: foo1.c foo2.c foo1.h foo2.h`, but still have Make figure
out what how to build this. There may be multiple ways to build a
target (`foo.o` from a C file, or a Pascal file), but the one which
applies is the one whose prereqs exist. There are even default rules
for linking object files.

Typical variables used in implicit rules are:

```
CC
CFLAGS
CXX
CXXFLAGS
LDFLAGS
LDLIBS
```

This ability to "fill in the chain" with *intermediate* files gives
rise to `.INTERMEDIATE` and `.SECONDARY`. They're not hyper-important
to know about.

Patterns are kind of like defining new implicit rules. They are
written like:

```
%.o: %.c other.h
	$(CC) -o $@ -c $<
```

Variables like `$@` and `$<` are called *automatic* because they are
automatically set when a rule is invoked. `$?` is a list of all
prereqs that are newer, while `$^` is a list of all prereqs.
