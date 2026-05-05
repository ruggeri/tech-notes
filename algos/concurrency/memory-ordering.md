Before reviewing this, possibly review `cache-coherency.md` if the
definition of cache coherency is fuzzy.

# Volatile

First: if a variable is not marked `volatile`, then C/C++/Java will not
know that it must be able to be accessed from multiple threads. If so,
the compiler can choose not to back this variable with a memory
location. The variable may just live in a register, which is thread
local. Updates to the variable may then not be written to RAM, so an
update from one thread will never be seen by another.

Here's an example:

```C
// Marking these variables volatile is necessary!
//
// BTW, still probably wrong/unsafe because of memory reordering.
volatile bool flag = false;
volatile int x = 0;

function f() {
  while (!flag);
  println("%d", x);
}

function g() {
  x = 42;
  flag = true;
}
```

If you don't mark the variables `volatile`, then `flag` and `x` might be
thread-local, meaning that a thread that runs `g()` may not change the
`flag` value in another thread running `f()`, so that the loop may never
exit.

Even _if_ the compiler does store `flag, x` in RAM so that threads can
see updates from other threads, the compiler is allowed to _reorder_ the
instructions to write `flag, x` if you don't mark them `volatile`. The
compiler can reorder things if it doesn't think it will change the
program's behavior. It is allowed to assume that a regular
non-`volatile` variable will _not_ be observed by another thread. So it
is allowed to reorder writes to non-`volatile` variables. This is
usually to allow performance optimization.

That is the function of the `volatile` keyword: require that writes to
the variable will be visible to other threads, and that write
instructions will be issued in order. (Soon we'll see that just because
one core writes in one order doesn't mean another core will _read_ the
values in that order).

# Sequential Consistency

The cache consistency property tells us that if a core issues a series
of writes to a variable, (1) those writes will eventually be able to be
read from the other CPUs, (2) the writes will become available to be
read in the same order that they were written. That is: you cannot read
the value of a later write before you can read the value of an earlier
write.

Almost all CPU programming is done with the assumption of cache
coherency. There are some historical or research designs which are not
cache coherent. And there are some NUMA chips where the caches in
different "nodes" are not coherent. But these are niche siutations. x86,
SPARC, POWER, ARM are all cache coherent.

But what about operations that read/write _different_ locations in
memory? The ideal would be _sequential consistency_: that reads/writes
are processed as if there were a single total order interleaving the
reads/write operations issued by the individual cores. Later reads would
always return the result of the most recent write to that location.

However, instructions to read/write different memory locations can be
"asynchronous". For instance:

- `store X at addr1; store Y at addr2`
  - If store-store reordering is allowed, then the value of `Y` may be
    written to `addr2` before the value of `X` is written to `addr1`.
  - That is despite issuing the instruction to write `X` _before_
    issuing the instruction to write `Y`.
  - This is called store-store reordering. x86 promises not to do it.
- `read X at addr1; read Y at addr2`
  - A value of `Y` may be read from `addr2` may be read before the value
    of `X` is read from `addr1`.
  - This is called load-load reordering. Again, x86 promises not to do
    it.
  - If the value for `X` is a cache-miss but the value for `Y` were a
    cache-hit, it would be a performance optimization if we could start
    working with `Y` and wait for `X` to come in later. But that's what
    load-load ordering prohibits.
- If you have read-read and store-store order respected, then you cannot
  read the result of a later write but then subsequently fail to read
  the result of an earlier write.
  - SPARC and ARM _do_ allow re-ordering of load-load and store-store,
    which can be a performance benefit.
  - So they allow later writes to be read before earlier reads.
  - That puts additional burden on the programmer when they need tighter
    ordering. They have to use fencing.
- `load X at addr1; store Y at addr2`
  - A value of `Y` might be stored at `addr2` before the value `X` is
    loaded for `addr1`. This is called load-store reordering.
    Specifically: it's when an earlier load instruction takes effect
    _after_ a later store instruction.
  - x86 promises not to do this.
  - Loading `X` _might_ be slow if it's a cache miss. But storing `Y` is
    itself slow, because it needs to be put in memory and there needs to
    be cache traffic to invalidate other cache lines.
    - If stores don't generally finish faster than loads, you don't gain
      much by allowing this reordering. For instance, if the store
      _never_ finishes before the load, then in practice even if you do
      them concurrently, the store never overtakes the load.
    - And if you allow this to _ever_ happen, then you _always pay_
      increased programming complexity, because programmers must now
      assume that load-store reordering _could_ happen.
- `store X at addr1; load Y at addr2`
  - A value of `Y` might be loaded from `addr2` even before the value of
    `X` is stored at `addr1`. This is despite issuing the store
    instruction before the load instruction.
  - This is the one that x86 _does_ allow. Basically, it treats store as
    an async write to RAM. But the load can easily finish first
    especially if the cache line is already pulled in.

C/C++ `volatile` will only insure that reads and writes to the variables
are _issued_ in the program order. But the actual order in which the
operations take effect depends on the processor. When reordering is
allowed, different processors may even see different results.

**Our `while` loop example**

If the C/C++ compiler compiles our program naively, then on x86 we can
expect that it would run correctly. This is because x86 respects the
write-write order of storing `x = 42; flag = true`. And it respects the
read-read order of loading `flag` and only afterward reading `x`.

Still, I believe that the unsynchronized access of the variables is
still undefined behavior, and the compiler is allowed to do whatever it
wants with your program.

On ARM or SPARC or RISCV, even if the compiled code issues the
instructions in the expected order, it will be wrong. The store of `x =
42` may end up ordered behind `flag = true`, in which case the other
thread can print `0` for `x`. Likewise, because of read-read reordering,
`x` may be read speculatively before the final read on `flag`.

Again, this is about how the CPU processes instructions. Even if the
compiler issues instructions in the order we expect, the loads/stores
may not "occur" in that same order.

# Atomic

So how do we impose order on updates to different addresses? We need to
use `atomic`.

```C++
volatile Object *ptr;

function f() {
  Object *val = new Object()
  val->x = 3

  ptr = val;
}

function g() {
  while (ptr == NULL) {
    // spin
  }

  // It prints 3, hopefully? Not for sure unless we use atomics!
  printf("%d\n", ptr->x)
}
```

How do we make sure that when a thread running `g` sees the new pointer
written into `ptr` by a thread running `f`, that it _also_ sees the
memory for `val->x` properly set to `3`?

As we mentioned, x86 has a pretty strong model: if you see the result of
a later store, you can also see the result of all the prior stores
issued by that core. This is just the default load and store behavior.

So if a thread really does write `3` into `val->x` _before_ it writes
`val` into `ptr`, then this really does work on x86.

However, C/C++ is allowed to reorder store instructions. It often does
this as part of other optimizations. Now, stores/loads to/from a
`volatile` variable are _not_ allowed to reorder with respect to other
store/load operations on a `volatile`. _BUT_ non-`volatile` writes may
be moved across a write to a `volatile` variable.

If that happens, a thread `g` can see the pointer published to `ptr` but
not see the value `3` that should have been written into `ptr->x`. This
can absolutely happen even on x86.

To avoid this, we use atomics:

```C++
#include <atomic>

atomic<Object *> ptr{NULL};

function f() {
  Object *val = new Object()
  val->x = 3

  ptr.store(std::memory_order_release)
}

function g() {
  Object *val = NULL;
  while (true) {
    val = ptr.load(std::memory_order_acquire)
  }

  // It prints 3!
  printf("%d\n", val->x)
}
```

The use of `std::memory_order_release` says that earlier writes may
_not_ be moved across the call to `ptr.store`. The use of
`std::memory_order_acquire` says that `ptr.load` must be able to read
everything that was required to be written before the corresponding
`ptr.store`. You can say that "release" is releasing all the prior
writes to the next "acquirer" of this location.

On x86, this is mostly about restricting the compiler from reordering
instructions. The use of atomic in this way does not need to issue
special synchronization instructions on x86.

Some architectures like POWER and ARM have weaker ordering rules. Then
you need `atomic` even more so because special instructions must be
issued.

Note: `mutex` has `lock` do an "acquire" and `unlock` do a "release".
Thus, by the time you unlock, a later locker will see everything that
"happened before" a prior unlock. So `mutex` is not just doing mutual
exclusion for you. It is also making sure that memory access across
cores are synchronized.

# A Store-Load Issue

```C
volatile int x;
volatile int y;

function f() {
  x = 1;
  printf("%d\n", y);
}

function g() {
  y = 1;
  printf("%d\n", x);
}
```

Here, if stores are not reordered after load, we expect to see at least
_one_ of the two threads print `1`. Possibly _both_. However, this is
_not_ guaranteed on x86!

To prohibit the reordering of the store operation after the load, you
must use a _memory barrier_, also called a _memory fence_.

In x86, you have instructions like `sfence` (stores prior to the fence
instruction must happen before later stores). This is designed for
special kinds of memory, where store-store reordering is allowed. But
that is niche and `sfence` is not needed for regular memory, because x86
already prohibits store-store reordering.

Similarly, `lfence` requires that loads prior to the fence instruction
complete before later loads. This is again niche, because x86 enures
load-load order. I believe it is used to control speculation/timing.

`mfence` is the most useful: it says all loads/stores before the fence
was issued should not be reordered with any instruction _after_ the
fence.

So, we want to add

```c++
#include <atomic>
std::atomic_thread_fence(std::memory_order_seq_cst);

// OR,
#include <x86intrin.h>
_mm_mfence();
```

You can do an `atomic_thread_fence` with `std::memory_order_release` (it
basically does an `sfence`), or with `std::memory_order_acquire` (it
does an `lfence`). But on x86 you don't typically need those.

Fencing can be expensive. You need to wait for prior writes to drain and
be globally visible. You "stall" the pipeline because you can't actually
load until the fence completes.

Java `volatile` variables _always_ have memory fences inserted. You
don't have to issue fences yourself. That makes it easier on the
programmer certainly, but it imposes a cost everywhere. You can opt out
of this cost if you use `VarHandle`, which allows you to update a
variable without issuing a memory fence.

- Source: mostly ChatGPT in 2026-04-XX.
