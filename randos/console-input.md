## ASCII

ASCII code is 7bit with a parity. That means value are 0-127. Values
32-126 are all printing characters. Values 0-31 and 127 are
non-printing.

What are the non-printing characters? They are things like:

- 0: null.
- 4: EOT (end of transmission). This is sent by the terminal when you
  type Ctrl-D. It is different from EOF, which is a return condition of
  the C library for character input.
  - Just because the terminal sends EOT doesn't mean that the
    application will treat it like an end of file.
  - For instance, with `cat`, it will only be treatedly like EOF if
    the EOT appears as the first character on a new line.
- 7: Bell. Rings a bell.
- 8: Backspace. Ctrl-H.
- 10: Line feed. Newline. Ctrl-J.
- 13: Carriage return. Ctrl-R.
- 27: Escape. Ctrl-[.
- 127: Delete. Ctrl-?.

Note: you can send any of these via the keyboard by using Ctrl-whatever.
Note that several have alternate ways to send: ESC key is equivalent to
Ctrl-[. Enter key is equivalent to Ctrl-J.

I picked some of the most important. But note that 0-31 and 127 are all
non-printing. Each can be input via CTR-X, where X is some appropriate
character. Note that 32-126 are all printing-characters, and are input
with the appropriate key.

Next, note that _some_ ASCII codes have C escape sequences: '\n' means
linefeed which is code 10. But most escape sequences do not have a C
code. For instance, Ctrl-Z (27) has no C code, because it's not intended
to be interpolated into a string.

Note that `^Z` is just a different notation to Ctrl-Z.

Note that `\e` is a non-standard extension to GCC to mean ASCII 27. But
you can't expect all languages to know this one. I think Python doesn't
have it, and thus prefers "\033" (more later).

Note further that programs may interpret these command characters in
various interesting ways. For example, `^A` and `^E` can be interpreted
by readline to mean start/end of line (or anything else you desire). But
that is not what they mean in ASCII. Likewise, `^K` means "Vertical Tab"
in ASCII, whereas the standard in readline is to kill the line.

## Escape sequences

The escape character `Ctrl-[` originally was used to "escape" characters
like `Ctrl-J` so that these could pass through uninterpreted. As in: it
was like escaping for any other embedding into a string.

Later, the `Ctrl-[` character was coopted to be used for proprietary
extensions to terminals. But this caused problems because different
terminals had their own different proprietary extensions. Thus the
library `termcap` (term capability) was developed (by Bill Joy).
`termcap` was extended by `terminfo`. These programs both let programs
look up what the escape sequences are for certain operations.

Eventually, however, people realized that the DEC VT100 (and its clones)
were dominating the terminal space. The VT100 control sequences were
thus standardized. This is nowadays called ISO/IEC 6429.

Applications like vi and emacs continue to use `termcap`/`terminfo`, as
does `curses`. In theory, these programs thus work on terminals outside
VT100. In practice, however, these programs/libraries are no longer
tested on terminals outside VT100, and are likely to have bugs.

Many programs thus assume that the ANSI escape codes are in use, and
will write and interpret them directly.

## CSI Sequences

So `^[` is the ASCII code for escape. There are a not very large list of
escape sequences. But the most common is `^[[` (i.e., `ESC[`). This is
the _command sequence introducer_ (CSI). The following command sequences
are standardized:

- `^[[` then `A` for cursor up. Also `B` (down), `C` (forward), `D`
  back.
- `^[[y;xH` moves cursor to row `y` column `x`.
- `^[[K` kills the line from the current point.

CSI also standardized commands to put the terminal in various color
modes. This is `^[[X;m`, where X is a number with a meaning defined by
SGR (Select Graphic Rendition). However, like other CSI sequences, you
can write a series of numbers like `x;y;z`. But let me return to SGR
another time.

**Other CSI**

- You can ask yourself: what about other key combinations? There are
  some that will be passed to the terminal. For instance, shift+tab will
  be rendered as `^[[Z`.
- But I don't believe that xterm will inform you of arbitrary
  combintions. For instance, I believe that `^a` is the same as `^A`.
  The shift key doesn't matter here.
- I believe that `Meta+X` is typically translated as `ESC X`.
- Source: https://gist.github.com/justinmk/a5102f9a0c1810437885a04a07ef0a91

## Readline commands

Here are several non-standardized commands, which Readline respects:

In emacs, ESC-b moves back a word, while ESC-f move forward one word.
ESC-DEL deletes the previous word. These are not ANSI standardized.

Likewise, we have commands like ^A, ^E, ^K, ^U to modify lines.

And also ^C and ^Z to send signals.

Some of these commands (especially the control/carrot ones) violate the
intention of the ASCII control characters. For instance, I believe ^U
means "Negative Acknowledgment".

When a user types Meta-x, this is typically delivered by iTerm as
`ESC-x`. That means you can say `Meta-[ D` for back arrow.

Anyway, I am not sure where (if anywhere) these are standardized. I
believe they are just conventions of readline, and bash uses readline.

## Brief non-`curses` example

First, let's see how to use simple escape sequences in Python. Let's
recall a few things:

- You interpolate a hex byte value in Python by "\xHH".
- You interpolate an octal byte value in Python by "\0XX"

Since we want to print code 27, this is 33 in octal, which is why "\033"
frequently comes up when printing escape sequences.

Here's a good one:

```python
import time

print("Hello")

time.sleep(1)

# Scroll up one line.
print("\033[A", end="")

print("Overwrite!")

# alternative; would kill the line
print("\033[K", end="")
# But note that I can't use `print("\013")`. That would say ^K, but the terminal would not do anything with that.
```

Next, let's consider some terminal input:

```python
x = input()
print([b for b in x.encode("ascii")])
```

If I input ^A, then "^A" is printed in the input space, and I get back
[1], which is the proper ASCII code. If I input ^E, then "^E" is printed
in the input space, and I get back [5], which is again the proper ASCII
code.

If I press the up arrow, then "^[[A" is printed in the input space. I
get back [27, 91, 65], which are the ASCII codes for `^[`, 91 for `[`,
and 65 for `A`.

Next, let's use readline via Python. It's stupid easy:

```python
import readline

# input will use readline to get the input. It'll respect your settings.
x = input()

print(x)
```

You'll have to do a little bit more to get/set history. But not much.
I'm not sure how to selectively use readline for some input but not
others, though... Maybe you need need to do a file read.

**Controlling The Terminal**

The C `termios` library can be used to control the terminal. For
instance, let's turn off echoing of user input:

```python
import sys
import termios
import time

fd = sys.stdin.fileno()
# Gets a list of the terminal attributes in order to restore things.
old_flags = termios.tcgetattr(fd)
# An exact copy
new_flags = termios.tcgetattr(fd)
# Turn ECHO off.
new_flags[3] = new_flags[3] & ~termios.ECHO

try:
    # Don't know/care what TCSADRAIN is right now.
    termios.tcsetattr(fd, termios.TCSADRAIN, new_flags)
    print("tell me a secret")
    your_cool_secret = input()
    time.sleep(2)
    print(your_cool_secret)
finally:
    termios.tcsetattr(fd, termios.TCSADRAIN, old_flags)
```

You probably want to be able to get character-at-a-time input, instead
of just line-buffered input. This is called canonical (line buffered)
and non-canonical (character-at-a-time) modes.

Sometimes people call canonical "line mode" or "cooked mode." Whereas,
non-canonical is sometimes called "raw mode". There is also an
intermediate, called "cbreak", which is basically raw but some control
characters are interpreted by the terminal (like `^C`). I believe
cooked/cbreak/raw come from Unix days, whereas canonical/non-canonical
come from POSIX standardization.

Anyway.

Let's see how to put ourselves in cbreak mode. You can simply use
`setcbreak` from the `tty` module. But I'll show you an annotated
version of that function (it uses `termios`):

```python
def setcbreak(fd, when=TCSAFLUSH):
    """Put terminal into a cbreak mode."""
    mode = tcgetattr(fd)

    # Turn off echoing and put us in non-canonical mode.
    mode[LFLAG] = mode[LFLAG] & ~(ECHO | ICANON)

    # Specify that when we call read, we want to get at least one character.
    mode[CC][VMIN] = 1
    # This would have allowed a time-based wait? I guess in case you want
    # to wait for a specified duration? But we don't want to.
    mode[CC][VTIME] = 0

    # Set it.
    tcsetattr(fd, when, mode)
```

And let's use it:

```python
import sys
import tty
import time

# Without this, nothing would happen until you press enter.  You could
# type multiple characters, and this would queue up multiple bytes to
# read. They would be read one-by-one. But it would wait until enter
# before any processing occurs (because that's when the terminal would
# deliver input to the Python program.
tty.setcbreak(sys.stdin.fileno())

print("start typing!")

while True:
    # Read one byte at a time
    x = sys.stdin.read(1)
    print(ord(x))
    time.sleep(1)
```

Guess what though? How will this cool program handle arrow keys? Of
course it will just process them as a multiple byte sequence.

Here's a random question possibly better suited for my unicode doc
probably. What if you input "🙂"? It actually reads that as a _single
character_ and prints `128578` (the correct code-point). Holy moses.
Presumably that's because `sys.stdin` is being interpreted as UTF-8, and
thus `sys.stdin.read(1)` asks for _one UTF-8 codepoint_.

However, if you replace with `sys.stdin.buffer.read(1)`, this actually
reads stdin in raw mode, and thus you get a series of bytes. I believe
this shows that the terminal is just passing on bytes to Python.

Note that if you paste a red heart into the terminal, this is actually
the combination of two unicode graphemes to produce a grapheme cluster:
heavy black heart (code point 10084) and a variation selector (code
point 65039).

But if I said `sys.stdin.read(2)`, it would get the two unicode
graphemes I need, and I could print out the string to produce the red
heart in the terminal.

Anyway, clearly we want a library to take care of this all for us.

**Curses**

Curses gives you some stuff. For instance, it gives you functions to
move the cursor around, enter cbreak mode, stop echoing... But we saw
how to do all that shit with CSI and termios.

It has a `getch` method that gets the next byte. This is similar to when
I read `sys.stdin.buffer.read(1)`. Similarly, it reads bytes in the
range 0-255. So it does handle non-ASCII characters.

Note that `getch` is much worse than reading a character in cbreak mode.
Of course multi-codepoint grapheme clusters won't work. But also any
non-ASCII character, even if it is a single code-point, won't work. That
includes all the characters in Latin-1 that could have fit in a 8bit
encoding! Of course, Latin-1 characters are encoded in a multi-byte
UTF-8 format, so they're returned as multiple bytes...

Luckily, there is a `get_wch`. This works codepoint-by-codepoint. Nice.
But of course it won't work with multiple codepoints...

```python
import curses

items = []
try:
    stdscr = curses.initscr()

    while True:
        x = stdscr.get_wch()
        items.append(x)
finally:
    curses.endwin()
    print(items)
```

Here's something unpleasant and weird. Let's print some stuff:

```python
import curses
import time

items = []
try:
    stdscr = curses.initscr()
    stdscr.addstr("🙂❤️✅")
    stdscr.refresh()
    time.sleep(1)
finally:
    curses.endwin()
    print(items)
```

Both the smiley and the heart will print. But not the checkbox! That's
odd. Note that ✅ is handled fine when calling `get_wch`. So it's weird
we can't put it back...

**`keyboard` and `pynput`**

So let's say we just want to handle keyboard input, but don't want to
use curses for managing the screen.

You could try using `pynput`. This lets you install listeners to listen
for key events. However, it listens _in all windows_. It doesn't just
put the terminal into cbreak mode. It actually talks with the operating
system to ask it to receive _all input events_. Wow. I think it can even
_write_ keyboard events to the operating system, simulating typing. Wow.

This requires special security permissions, plus it isn't what you want.

You can try the `keyboard` module. But this needs to be installed (or
run) as `sudo`. WTF? Why?

So basically, I think you just have to do shit yourself.

## C++

Let's use ncurses from C++:

```c++
// Asks ncurses to use widechar mode.
#define NCURSES_WIDECHAR 1

#include <locale.h>
#include <ncurses.h>

int main() {
  setlocale(LC_ALL, "");

  initscr();
  // prints fine. Even the red character.
  addwstr(L"🙂❤️\n");
  // doesn't print the check mark. Interesting...
  addwstr(L"️✅");
  refresh();
  getch();
  endwin();

  printf("%ls\n", L"✅");

  return 0;
}
```

## Character Width

The "width" of a character really depends. On a variable-width font,
characters have different widths. The width of a character depends on
the font.

However, on the console, we're usually dealing with monospaced fonts.
Each character takes up one column of the terminal. Or almost. Typically
fonts are actually _duospaced_. That means there are only two kinds of
width: 1 unit, or 2 units. These double width characters are sometimes
called "full width" (by implication, half-width is when you cram a Kanji
into space meant for a latin character).

This is necessary for CJK writing systems, where characters are square.
Also, many emojis display better if they are given double the width.
(E.g., a smiley face which is a circular face which needs a square space
to occupy). Note that the aspect ratio

I think most monospace fonts have ~1:2 aspect ratio (width to height)
for Latin characters and ~2:2 (square) aspect ratio for CJK and many
emojis.

Obviously, in determining the display width of a string, you cannot
count the number of bytes in the representation. We know that will vary
depending on the representation scheme chosen, and is never what you
want. You can't even simply count the number of Unicode codepoints. The
letter 'A' and the Kanji '漢' have different widths, despite each being
a single unicode codepoint. Likewise, '😊' is a double-width character.

To help, there is a function `wcwidth`.

```c++
#include <locale.h>
#include <iostream>
#include <string>
#include <wchar.h>

int main()
{
  setlocale(LC_ALL, "");

  wchar_t chars[] = {
      L'🙂',
      L'😊',
      L'a',
      L'♥',
      L'Ａ',
      L'✅',
  };

  int numChars = sizeof(chars) / sizeof(wchar_t);
  for (int i = 0; i < numChars; i++)
  {
    wchar_t ch = chars[i];
    std::cout << std::to_string(wcwidth(ch)) << std::endl;
  }

  // The loop prints 2, 2, 1, 1, 2, -1.

  return 0;
}
```

Note that this is basically correct. However, note that the green
checkbox is considered to be a non-printing character and given a length
of -1. That's why ncurses doesn't print it correctly.

There is also `wcswidth`, which does the same for a string. But this is
a lame one, it just runs a loop and accumulates. It returns -1 if it
encounters any non-printing character.

- https://github.com/bminor/glibc/blob/b92a49359f33a461db080a33940d73f47c756126/wcsmbs/wcswidth.c
- https://man7.org/linux/man-pages/man3/wcswidth.3.html

Note that `wcwidth` is provided as part of the standard library. This is
operating-system specific. GNU libc (glibc) has one implementation,
while OSX derives from the BSD libc. The OSX version might be quite
old/simple/out-of-date/incorrect.

I rented an AWS box with Debian and glibc 2.31, and it appears that the
green checkbox has its width properly computed as 2 there. My test
program runs satisfactorily there. So it appears to be atypical case of
OSX being out-of-date.

Quick question: if ncurses can't print the green checkbox, how can tmux?
The answer is in the Homebrew formula
https://github.com/Homebrew/homebrew-core/blob/HEAD/Formula/tmux.rb. If
you look, you'll see that it wants to use utf8proc on MacOS. This is
further explained in the tmux build scripts:
https://github.com/tmux/tmux/blob/b566cd57bfa0d28fcc2e44e5a7b2e88433a5e016/configure.ac#L839.

They say that MacOS Unicode support is poor. They want you to
`--enable-utf8proc` when building on MacOS, which is what Homebrew does.

I wrote up a little test program using utf8proc:

```c++
#include <locale.h>
#include <iostream>
#include <string>
#include <wchar.h>

#include <utf8proc.h>

int main()
{
  setlocale(LC_ALL, "");

  wchar_t chars[] = {
      L'🙂',
      L'😊',
      L'a',
      L'♥',
      L'Ａ',
      L'✅',
  };

  int numChars = sizeof(chars) / sizeof(wchar_t);
  for (int i = 0; i < numChars; i++)
  {
    wchar_t ch = chars[i];
    int width = utf8proc_charwidth(ch);
    std::cout << std::to_string(width) << std::endl;
  }

  return 0;
}
```

I built and ran this.

```
>> g++ -o utf8proc_test utf8proc_test.cc -lutf8proc
>> ./utf8proc_test
2
2
1
1
2
2
```

This is correct!

It seems like iTerm2 handles emoji widths without using wcwidth:
https://github.com/gnachman/iTerm2/blob/aabb9cc7ab727e688d745c278b602187c37ee3a6/sources/NSCharacterSet%2BiTerm.m.
Not that this is perfect either.

Is there any way to ask OSX to use a different version of wcwidth? I
don't know yet. I explore that possibility in my notes on the standard
library.

Last, I'll mention an HN post regarding emoji in the terminal:
https://news.ycombinator.com/item?id=30113521. What do we see there?
They mention that each terminal tries to replace wcwidth with its own
mapping of emoji to width. But if the program running in the terminal
doesn't have the same opinion as the terminal, then the cursor gets
confused.

It appears that maybe ZSH handles emojis more nicely than bash?

## Sources

- https://en.wikipedia.org/wiki/ASCII#Control_code_chart
- https://en.wikipedia.org/wiki/ANSI_escape_code#CSI_(Control_Sequence_Introducer)_sequences
- https://www.man7.org/linux/man-pages/man3/termios.3.html
  - Man page for termios.
- https://github.com/python/cpython/blob/3.10/Lib/tty.py
  - Python's tty module. Simply uses termios.
- Python Curses docs
  - https://docs.python.org/3/howto/curses.html
  - https://docs.python.org/3/library/curses.html
