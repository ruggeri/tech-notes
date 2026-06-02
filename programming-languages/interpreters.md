Ruby and Python both have reference implementations written in C. These
are **MRI** and **CPython**. MRI and CPython are both extremely popular.

Both have versions written in Java. These are **JRuby** and **Jython**.
JRuby is fairly popular: in 2026 it aims to support Ruby 4.0 (MRI is on
4.0.5), and claims to run Rails. Jython is stuck on Python 2.7 as of
2026; it's basically dead. The biggest downsides to JRuby are (1)
startup speed (doesn't matter for servers) and (2) compatibility with
existing native C extension modules. A major advantage to these
implementations is they remove  the Global Interpreter Lock. You also
get to use Java code from Ruby, if you want that.

Both CPython and MRI use a GIL so that no two threads can be
interpreting Ruby code simultaneously. This is (1) for simplicity and
(2) because fine-grained locking adds overhead. When there is only a
single core, fine locking doesn't buy you anything, but you do pay for
it.

In Python and Ruby, you can still fork threads despite the GIL. They are
typically real native threads that can be scheduled by the operating
system. They just won't simultaneously execute Ruby or Python code.
However, Python/Ruby functions that block on IO will release the GIL.
For workloads where IO dominates and single-core CPU utilization doesn't
hit 100%, this is already basically ideal. Typical way to use more CPU
in Python and Ruby is to spawn more processes, though this can use more
memory. (This is covered in my concurrency document).

Note that C module code can be multi-threaded under the hood. This can
be good, but it also lets in concurrency problems if you use the C
module the wrong way. Also, synchronous IO initiated from the C module
might block the interpreter if the module forgets to release the GIL.

JRuby is complete enough to run Rails. It is basically an interpreter
written in Java, but it also has the ability to do both JIT and AOT
compilation of Ruby code to Java bytecode.

**PyPy** is a JIT interpreter for Python written in Python. It can
compile Python code to native code. It has better performance than
Python, and can run major projects like Django. PyPy still has a GIL,
though. PyPy supports Python 3.11 and 2.7 as of 2026 (latest CPython is
Python 3.14.5).

**Rubinius** seems much in the same vein as PyPy, but while PyPy is very
popular, Rubinius seems to have died. Its last major release was 2024
(this is 2026), and in 2022 `ruby-install` package proposed removing
Rubinius as no longer actively maintained or used.

**Cython** is a compiled language. You write Python, which is translated
to C, and can now be loaded by Python as a C module. In particular, you
provide type hints, which allow for type-aware compilation. Otherwise, I
believe what you write is basically just going to end up being
interpreted. Cython only does AOT, and its ability to analyze and
optimize your program is limited by your type declarations. Cython is a
fork of a project by SageMath, and is popular for numerical users of
Python.

**IronPython** runs on the CLI. It primarily finds use as a way to write
scripts for a CLI framework. It supports Python 3.4 as of 2026, which
was released in 2014. That suggests IronPython is far behind, though it
continues to have new releases as late as 2026...

**TODO**: **TruffleRuby** and **GraalPy** (runs Python 3.12 on JVM as of
2026)

## JavaScript

Both Node.JS and Deno both use V8. Bun uses JavaScriptCore, which is
from Apple/Safari.

Node.JS does allow you to launch true concurrent threads ("worker
threads"), even if that is not the default.
