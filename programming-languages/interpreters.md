Ruby and Python both have reference implementations written in
C. These are MRI and CPython. MRI and CPython are both extremely
popular.

Both have versions written in Java. These are JRuby and Jython. JRuby
is fairly popular, while Jython is not. The biggest downsides to JRuby
are (1) startup speed (doesn't matter for servers) and (2)
compatibility with existing C modules. A major advantage to these
implementations is the remover of the Global Interpreter Lock. You
also get to use Java code from Ruby, if you want that.

Both CPython and MRI use a GIL so that no two threads can be
interpreting Ruby code simultaneously. This is (1) for simplicity and
(2) because fine-grained locking can be slower for single-threaded
code. Note that C module code can be multi-threaded. Also, you can
still fork threads, and when they block on IO, other threads can be
running. A lot of software is mostly IO bound, so the GIL doesn't
detract from the performance of these applications.

JRuby is complete enough to run Rails. It is basically an interpreter
written in Java, but it also has the ability to do both JIT and AOT
compilation of Ruby code to Java bytecode.

PyPy is a JIT interpreter for Python written in Python. It can compile
Python code to native code. It has better performance than Python, and
can run major projects like Django. PyPy still has a GIL, though.

Rubinius seems much in the same vein, but while PyPy is very popular,
Rubinius seems to be going nowhere.

Cython is a compiled language. You write Python, which is translated
to C, and can now be loaded by Python as a C module. In particular,
you provide type hints, which allow for type-aware
compilation. Otherwise, I believe what you write is basically just
going to end up being interpreted. Cython only does AOT, and its
ability to analyze and optimize your program is limited by your type
declarations. Cython is a fork of a project by SageMath, and is
popular for numerical users of Python.

IronPython runs on the CLI. It primarily finds use as a way to write
scripts for a CLI framework.
