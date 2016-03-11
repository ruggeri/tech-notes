## My Own Preliminary

Basic Idea. We want to be able to express logic. We also want to be
able to do math. Math can be expressed as a series of logical
operations on discrete, fixed length bits.

Note that we want to be able to calcualte *nonlinear* things. Think
about a 2x2 grid of T/F, which are real values in the range `(0,
1)`. What is linearly separable? Not even OR! Consider a possible
function:

    (0.5*x_1)+(0.5*x_2)+0.25

This properly maps `(0.5, 0)` and `(0, 0.5)` to `>=0.5`. But it fails,
in that if we choose, `(0.25, 0.25)`, this also maps `>=0.5`!

We can fix this to an arbitrary level with some kind of sigmoid
amplifier. Then you can effectively binarize your inputs. Once you do
that, you can separate.

This is one reason NN might use a sigmoid, BTW. That nonlinearity can
recover logic/math. At least, it is expressable.

Let's evaluate what a single neuron could express. Once we binarize to
`(0,0)`, `(0,1)`, `(1,0)`, and `(0,0)`, what is possible to linearly
separate? Surely `OR` and `AND` (they're three points on one side of a
line). And NOT, too (since that's just the negative function). But you
still can't linearly separate for XOR. That's why a neuron can't learn
XOR. But a more complicated net could.

## Boolean Logic

Okay, let's say we have NAND. It turns out this is all you need for
logic (and thus math).

Let's think of all boolean functions:

* A function which either sets all of the four possibilities to true
  is constant; it doesn't really have two inputs, it has none!
* AND gives you one thing on. With NOTs on the inputs, you can move
  this around to build a circuit where just one input gives a true
  output.
    * And then you can take a NOT of the output to get just one thing
      off; three things on. Again, with nots on the inputs, you can
      choose what doesn't turn on.
    * For instance, let's build OR: `NOT(AND(NOT(x_1), NOT(x_2)))`.
* Last, let us consider when two things are set. There are `4 choose
  2` options: six options.
    * XOR gives you one. Not of the XOR output gives another.
    * Just using one input gives you another. NOT gives you a fourth.
    * Just using the other input gives you the fifth, and NOT of this
      gives the sixth.

I can build an XOR with AND, NOT, and OR (itself made of NOT and
AND). It's:

    AND(NOT(x_1), x_2)` OR `AND(x_1, NOT(x_2))

And then the last question is whether we can build both from `NAND`!
Clearly `NOT` comes out, since `NOT(AND(X, X)) = NOT(X)`. But that
means we can get `AND`, by applying a `NOT(NAND(X, Y))`.

Okay! We have the universality of NAND!

**Transistors to Gates!**

A transistor takes two inputs: IN1 (called the *base*) and IN2 (called
the *collector*). Basically, the conductance for IN2 is proportional
to the amount of voltage provided to IN1. This makes transistors very
basic amplifiers!

To do a NOT gate, you wire a battery to the collector. In addition,
you wire an out wire between the battery and collector. Wire a ground
to the transistor output (*emitter*). Now, if the input is driven
high, then battery will want to flow through the transistor, since
there is no resistence that way. Otherwise, it wants to flow through
the out wire, onward to some resister.

To get a NOR gate, you can just wire the battery to *both* transistors
(basically, two transistors in parallel). Alternatively, you can
combine the two inputs into a single transistor. Of course, NOR is
functionally complete.

Alternatively, wire the transistors *serially*. Now you need both
inputs to drive the battery voltage through the transistors. Else it
goes out the out wire.

BAM! Physics to logic! Fuck yes!

Source: http://hyperphysics.phy-astr.gsu.edu/hbase/electronic/trangate.html

## Arithmetic

A half adder takes to input bits and outputs a ones bit and a
carry. You can do this with an XOR (for the ones bit) and an AND (for
the carry).

After the first bit, you have three inputs: the two bits, plus a
possible carry. Do a half adder with the two inputs, then do a half
adder with the ones output and the carry input. You can OR the carries
(at most one can be set) and just use the ones output. Boom.

You can chain these, which is called a "ripple-carry adder". The
problem with these is that you have pretty bad delay.

The point of two-complement, BTW, is that you can just do normal
bitwise addition and get the right answers.

## Sequential Logic

Our chips don't have a notion of state or time or memory. To do that,
we need to introduce time.

To do this, we use feedback. Consider two NOR gates, with two inputs
`R` and `S`. They are "cross-couped", which means the output of one
NOR is an input to the other. Call the outputs `Q` (from the `R` NOR)
and `Q'` (from the `S` NOR).

Consider if `S=R=LOW`. Then, if `Q=LOW`, then the output of one of the
NORs is `HIGH` and the output of the other `NOR` is `Q=LOW`. Consider
if `Q=HIGH`: then the output of one of the NORs is `0`, so the output
of the other is `Q=HIGH`. We say that `S=R=LOW` is a *hold* operation,
of whatever the value of `Q` is.

Next, consider, if we drive `R=HIGH`: then the output of this NOR is
`LOW`. `Q` is *reset* to `LOW`.

Next, consider if we instead drive `S=HIGH`. Then the output of this
is `LOW`, and the output of the other NOR is `HIGH`. So `Q` is *set*
to `HIGH`.

Here is what we are accomplishing: we can either *hold*, *set*, or
*reset* the `Q` output. In this way, we are starting to get a notion
of time. This is called an **SR latch**.

We can consider a **D latch** (the D is for "data"). The idea here is
that you will store the input `D` if the input `E=HIGH`. We'll reuse
the SR latch. We wire `R=AND(NOT(D), E)` and `S=AND(D, E)`. So, if
`E=LOW`, then `R=S=LOW`, which holds. If `E=HIGH`, then if `D=LOW`,
`R=HIGH`, while `S=LOW`. That resets the SR latch, storing a zero.

OTOH, if `D=HIGH`, then `R=LOW` and `S=HIGH`, storing a one.

Let's build a circuit which will take in its own output
sequentially. Hook the circuit up so its output goes into a D
latch. Save the output in the D latch. Next, have this feed into a
*second* D latch. In the next step, save this into the second D latch,
which will then be wired into the input.

Put another way. Your circuit has some output. You SAVE this in the
first latch. You then start HOLDing the first latch; the value stored
here won't change regardless the output of the circuit. You start to
SAVE into the second latch; this changes the output of this latch,
which is fed into the circuit. The circuit changes its output. Now
start HOLDing the second latch. Now you can start SAVEing the second
value into the first latch, without changing the output of the second
latch.

Now you've got sequential logic!

Incidentally, I think this is how RAM works. Basically, it can either
maintain its current state or set it to something else. Other
circuitry will recall the value stored in the relevant cell.

## Machine Logic and Computer Architecture

* Load: direct (you give memory address), immediate (you give value),
  and indirect (you give register name that contains address.
    * Talks about how array indexing might work.
* Talks about control flow: unconditional jump, conditional jump.
* Talks about representation of instructions as machine code. They
  give us an assembler for translating assembly to binary.
* Mentions memory maps; screen and keyboard are available as memory
  maps.
* Doesn't really talk about what address space we have rights to.
    * How does a program, on launching, know its address space?
    * How does a program know where a library will get linked in? What
      does the linker really do?
    * I'm not sure I'm *really* that interested in these questions...
* Talks about program counter.

The book is a lot less interesting right now, since I know this stuff
about as well as I care to.

## Assembler

* Translates a symbolic version of the instructions.
* Provides variables and labels. Comments.
    * It's just going to reserve some memory space for the variables.
    * That space doesn't get reused, I guess. It must be in the data
      segment of the program, I suppose.
* Basically: the assembler is a *really* simple compiler.
* The assembler they suggest is very simple: first scan for all
  symbols and labels, building a table. Then assemble the whole thing.

## Virtual Machine

* Very simple, stack based VM. Arithmetic operations. Program control
  (if/else type stuff) and subroutine calling.
* They're going to talk about how to emulate calling a function.
* They mention about emulating the VM, but I think they may also do
  native compilation.
* They talk about compiler backends, frontends, intermediate
  languages.
* They are implementing the stack.

## High Level Language, Compiler I and II

* Recursive descent parsing: expressions/statements are made of other
  expressions, statements. You recursively parse.
    * In LL(1) grammars, the first token disambiguates amongst the
      possibilities. These are easy to parse, generally.
    * They parse to XML.
* The compilation to VM code doesn't seem that sophisticated. Since
  all types are always known, method resolution should be easy
  (static).

## Operating System

* They do memory allocation. They do a free list.
* They don't do any multiprocessing, though.
* That's it!

## Circuit Components

* Resistor
* Transistor
* Capacitor
* Switch
* Diode
* Inductor
* Transformer
* Variable resistor
* Potentiometer

## Projects

* AM/FM Radio
* AM/FM Transmitter
* Amplifier
