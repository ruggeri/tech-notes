A lot of software has been written in C. There are C compilers for
basically every system out there, so it is kind of a lingua
franca. Also, it can be very performant.

We may want to run this code inside a browser. This way it could be
distributed through the web and run with minimum installation work by
the user. Or it could be that we want to write functionality in the
browser around the program. Or the C program might just be a library
that we want to use, like audio encoding or whatever.

Let's take as an example running Python code in the browser. Here are
some possibilities:

* Write a Python-to-JS transpiler.
    * This is done for CoffeeScript.
    * This is what Opal does for Ruby.
    * Makes some sense for interpreted languages, less so for C
      code. High impedence mismatch there: e.g., no such thing as a
      pointer in JS.
    * Now your code is executed as JavaScript, so you get JavaScript
      errors. You're not really in the world of Python anymore.
    * Modules that rely on native interop with the Python world can't
      work. No clear way to translate Numpy, for instance.
* Rewrite Python interpreter in JS.
    * Now your Python code can run "first class", in an interpreter.
    * You have the same problems with modules like Numpy. You don't
      just need to be able to interpret Python code, but also to run
      compiled C that interfaces with Python. Also, you may have code
      that needs to interact with the OS via C standard library, not
      just Python stdlib.
    * Massive undertaking, needs to be done for each project to port
      over (Ruby? Haskell?). Won't exactly mimic the original; will
      have annoying differences; really hard to be faithful.
* Compile C program to bytecode, then write a VM in JavaScript that
  can execute this bytecode.
    * The clang frontend for LLVM compiles C programs to LLVM
      bitcode. Bitcode is the intermediate language that an LLVM
      backend compiles to native code (e.g., to x86_64 assembly).
        * LLVM is broadly similar to GCC, which also has the notion of
          frontend, intermediate form, and backend.
        * It sounds like LLVM maybe just has a cleaner, newer design,
          which makes it easier to add new frontends/backends. It also
          sounds like LLVM tends to compile code faster, and the clang
          frontend produces better error messages. GCC seems to
          produce faster code.
        * Finally, LLVM is attractively licensed (BSD).
        * It looks like, as I guessed, most optimization of code is
          done with the intermediate representation (the "middle
          end"). These optimizations can apply to a variety of
          compiled languages.
    * Emscripten provides a backend to produce a subset of JavaScript
      code, called asm.js.
        * This new backend is called, somewhat cryptically, "fastcomp"
          (it's a faster version of their original backend). It sounds
          like it is not ready to merge into the LLVM codebase, so is
          distributed separately. Basically, they forked LLVM to add
          the backend, and they also distribute their own version of
          clang (looks to be unpatched, maybe just so that the version
          of clang is ensured to work with their LLVM).
    * asm.js is a strict subset of JavaScript.
        * Thus, it can run in any browser. It is literally JavaScript.
        * However, because it is very restrictive, ASM.js is ammenable
          to AOT compilation and optimization (by the browser), if the
          browser offers that. Right now only Firefox does, but IE is
          working on this too.
        * Basically: asm.js is no more/less powerful than JS, just
          more readily optimized.
        * asm.js has a great FAQ. It explains that it's not a bytecode
          format because that doesn't make writing a compiler backend
          easier; and it's easier to read this way. An advantage over
          PNACL is that it's available everywhere now. It has no GC,
          so the compiled ASM doesn't need to be running that (though
          presumably if executed as JS you'd see GC performed).
    * Basically, you're compiling C code to IR, and doing
      optimizations in clang/LLVM. Then you emit asm.js, which is
      readily compiled to assembly except it's JS. If you ran it as
      JS, it'd be slower, but a capable browser can compile asm.js to
      basically native code.
    * But C code doesn't live alone. It needs an environment,
      glibc+POSIX. The Emscripten project also provides implementation
      of much of this functionality. In particular, they have their
      own libc.
    * There is one big catch: asm.js still has the limitations of JS.
        * No threads with shared memory (yet! workers with shared mem
          is maybe coming to JS). Other minor nuisances
        * Most importantly, JS is still async! Synchronous APIs, by
          and large, don't exist on the browser, so there's no way to
          do this.
        * Sometimes you can do sync stuff; there's a fake FS you
          preload files into, and access to these files can be
          synchronous. But if you ever really do need to get data from
          the true outside world, that needs to be done in an async
          way.
        * They spend a lot of time talking about game loops. But those
          needed to be async anyway, for input, duh. I think a better
          example is a text based adventure which is typically
          entirely synchronous.
        * Emscripten provides header files exposing async IO
          functionality, for your convenience! To run a game loop, you
          can tell Emscripten to keep calling a function that runs one
          turn of the loop.
        * In Node, technically speaking, you can do sync IO. But that
          doesn't seem to be well implemented (read from stdin seems
          to block for *all* the input?); it's not going to work on
          the browser.
        * So you need to rewrite code at least enough to be async.
    * ASM is *not* actually bytecode.
        * I lied in the heading. ASM.js is JS; it's run as normal
          JS. There is no virtual machine when using ASM.js; all code
          is JS code. There are big arrays which effectively represent
          the heap, and pointers which are indexes in the array.
        * LLVM bitcode is more or less translated straight to
          ASM. This limits ability to solve the sync IO
          problem. Emscripten doesn't try to cleverly rewrite your
          sync code with continuations. It doesn't try to pause in
          some bytecode stream to wait for the blocking operation to
          complete. It's not actually that sophisticated.
        * There is a technology called the Emterpreter. Basically,
          this is a bytecode, which is then interpreted. But this is
          slower than ASM.js, because ASM.js has received special
          optimization, and because the bytecode can't even be well
          optimized like JS, since the JS runtime can't understand the
          bytecode.
        * Right now, I think this is a definitive coverage of
          Emscripten's sync IO options...
    * WebAssembly
        * ASM.js still needs to be parsed by the JS interpreter. That
          can be very slow for long programs. Also, very large
          codesize; that's bad for transfer over the internet.
        * This appears to be broadly similar to PNACL, which is
          basically LLVM bitcode. The differences between the two seem
          mostly technical and minor.
        * I guess the idea is that WebAssembly can easily be turned
          into ASM.js, meaning any browser can support it? Also,
          ASM.js is more readable than assembly. It sounds like it is
          also a bytecode intended to be easily embedded into a JS
          runtime, which means it'll be easier to implement (sounds
          like PNaCL was harder to do this with?).
        * Means that WebAssembly can live independent of JS, which
          means that features can be added to WebAssembly independent
          of JS. E.g., `Math.imult` was added to ES6 especially for
          ASM.js. This doesn't need to happen in the fuuture.
* Google Native Client
    * Basically, you compile LLVM bitcode. This is shipped.
    * Google compiles this to the native architecture on arrival.
    * This runs natively, but in a sandboxed environment. There is
      also an API provided to allow communication between your native
      code and the browser environment.
    * Only available for Chrome; no one else says they want to
      implement this.
    * Runs truly native code, unlike ASM.js. ASM.js sounds like it
      can be very performant too, but not quite as fast.
    * Doesn't run in the JS environment: can run sync code, can run
      multiple threads.
    * Apparently the tricks to acheive the sandboxing are quite
      impressive.

## Example of C=>ASM

#include <stdio.h>

```
#include <stdio.h>

int main() {
  for (int i = 0; i < 10; i++) {
    printf("%d", i);
  }
  return 0;
}

function _main() {
 var $0 = 0, $1 = 0, $2 = 0, $3 = 0, $4 = 0, $5 = 0, $i = 0, $vararg_buffer = 0, label = 0, sp = 0;
 sp = STACKTOP;
 STACKTOP = STACKTOP + 16|0; if ((STACKTOP|0) >= (STACK_MAX|0)) abort();
 $vararg_buffer = sp;
 $0 = 0;
 $i = 0;
 while(1) {
  $1 = $i;
  $2 = ($1|0)<(10);
  if (!($2)) {
   break;
  }
  $3 = $i;
  HEAP32[$vararg_buffer>>2] = $3;
  (_printf(128,$vararg_buffer)|0);
  $4 = $i;
  $5 = (($4) + 1)|0;
  $i = $5;
 }
 STACKTOP = sp;return 0;
}
```

## Asm Controversy

ASM.js is not really readable; it's like reading assembly code. People
feel that if it gains traction, it will work against the oppenness of
the web platform. They also say that we're going to get dragged into
ASM.js because we're going to add optimizations for ASM.js while
neglecting optimizing JS code; that is, that ASM.js will starve
JS. OTOH, ASM.js is clearly more readable than machine
code/assembly. And we already have JS minification...

People were a little upset about using a subset of JS rather than just
bytecode (bigger code size, slow to parse), but that's being addressed
by WebAssembly. Note that WebAssembly can easily unpack to ASM.js, so
it's not *really* less readable than ASM.js.

Other people say this is like Java applets all over again. But others
say that there is less impedence mismatch than with Java; you can
write ASM.js routines, and then directly call out to your JS code, or
call the ASM.js methods from your JS code. You don't really have two
levels of abstraction. There's a higher level of
interoprability. Also, because ASM.js was chosen because it runs
pretty good *even without specific support*, it's very cooperative
with existing JS; it's less like a "plugin".
