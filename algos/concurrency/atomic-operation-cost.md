# Atomic Operations

- Sometimes atomic operations are called "read-modify-write" or RMW.
- That's because they normally claim a cacheline, and read the value,
  modify it, and write it back out, all before yielding the cacheline,
  even if someone else wants it.

# Aligned Load/Store

- Aligned load/store of 1/2/4/8byte values are atomic.
- Unaligned can span a word, so that usually requires multiple
  instructions, and is not atomic.
- Basically it relies on cache line ownership. You need to own the
  cache line exclusive before you can write it. No one else can modify
  the cache line until you yield it.

# Atomic Exchange AKA Test-and-Set

- Test-and-set/"atomic exchange": `XCHG`.
  - You don't have to write `LOCK XCHG`. `XCHG` is implicitly atomic.
- Atomic exchange is the better name. Test-and-set makes it sound like
  there is some conditional logic being performed by the instruction...
- Instruction is `XCHG addr, new_value`. It sets `*addr` to `new_value`,
  and it returns `old_value`.
- To claim a binary semaphore, you exchange `new_value = 1` into the
  `addr`, and you check if the `old_value == 0`, to see if no one had
  claimed it.
- The `XCHG` instruction cannot "fail" as an instruction, though you can
  "fail" to acquire the binary semaphore if `old_value == 1`.
  - So you might have to re-enter a busy loop after an attempt to claim.
- Works basically the same way as load/store; you need to lock this
  cache line just long enough to remove the old value and set it. You
  won't yield the cacheline until both operations are done, which is how
  you make this atomic.

## TTAS: Test-and-Test-and-Set

- Discussed in Art of Multiprocessor Programming.
- In "test-and-test-and-set" concept, you first test `*addr == 0` with a
  normal load/comparison. Then, only if flag is available, you do `XCHG
  addr, 1`.
- If result is `1`, it means that someone acquired the cache line
  exclusive (and invalidated your copy) after you acquired it for read.
- In that case, you spin, reading the cached value (marked `SHARED`). So
  long as your cached value is not invalidated, it means no one has
  updated this value.
  - `XCHG` cannot return anything except `1` until your cached value is
    marked `INVALID` (by someone else writing it).
  - If you tried to `XCHG`, that would claim the address `EXCLUSIVE`,
    which would generate coherency traffic.
  - That consumes coherency bandwidth, and makes other cores inspect
    their copy, despite no possibility of this core making progress.
  - You will only try to `XCHG` and generate cache traffic when your
    copy has been invalidated: when someone released the mutex.
- Still, when someone finally does release the lock, (invalidating your
  copy), all waiters will generate a burst of traffic.
- Of course, in most cases it is better to use an OS provided
  lock/synchronization object (like `futex`) so that the OS can
  deschedule the thread and run other things until the thread can make
  progress again.
  - But it depends on how long you expect to wait.
- There is a `PAUSE` instruction for use inside spin loops.
  - Basic idea is to reduce power/heat while doing something useless,
    lets an SMT/Hyper-Threading thread use more of the pipeline
    resources.
  - This is much lighter-weight than parking the thread by entering the
    kernel. It's an optimization at CPU level to do a very "light" kind
    of sleep.

## Cache Line Placement

- It might be ideal if the synchronization target (for instance, the
  address that stores a mutex), be placed on a different cache line than
  the data.
- Of course, other users may use TTAS on the mutex.
- But if the owner is modifying data on the same cache line, this will
  invalidate other cores' cached `SHARED` line.
- That breaks the other cores out of TTAS, generating cache traffic. And
  their attempts to grab the mutex will invalidate the *holder's* line,
  so that they have to fight to get the line back to keep modifying the
  locked data.
- Source: Art of Multiprocessor Programming
- On the other hand, you might try to put variables that will be
  modified together in the same cache line. This is a performance
  enhancement orthogonal to multiprocessor programming.
  - If you can fit the variables in 16bytes, then you can even CAS
    16bytes (if aligned).

# Atomic Increment

- `LOCK ADD`, `LOCK INC`, `LOCK DEC`
  - `LOCK` prefix is used because `ADD`/`INC`/`DEC` are all non-atomic
    instructions.
  - `LOCK ADD` takes an address and a value. It reads the address,
    modifies the value, and writes it back out before yielding cache
    line.
  - `LOCK INC` and `LOCK DEC` are specializations that
    increment/decrement by one. There is also `LOCK SUB`.
  - These all work *unconditionally*. That makes them useful for
    counters.
- `LOCK XADD` is similar to `LOCK ADD`, but returns the old value.
  - You might use `LOCK XADD` for a semaphore `release`, because if the
    old value were `0`, that might mean that there are people waiting to
    acquire the lock and you should maybe do some kind of wakeup logic.
  - You can't use `LOCK XADD` for `acquire` because acquisition of a
    semaphore is conditional on the count being `> 0`. You need a CAS
    loop.
- Uses typical RMW trick to hold onto this cache line until operation is
  complete.
- Can't fail. Typically no need for retrying/looping.
  - But because the atomic instruction basically enforces a
    serialization of updates, it is common to shard hot counters, so
    that there isn't constant coherence traffic to claim a single line.

# Compare-And-Swap AKA Compare-Exchange

- `LOCK CMPXCHG` takes a target `target`, and two operands `expected`
  and `new_value`.
  - If `target == expected`, then it sets `target = new_value`.
  - If `target != expected`, then it sets `expected = target`.
- You need to use `LOCK` or else this is not atomic.
  - That's a weird ISA wrinkle relative to `XCHG` which does not need
    `LOCK`.
  - I think `XCHG` predates introduction of `LOCK` prefix? Still, `XCHG`
    was always atomic because that's the point of the instruction.
- This works on 1/2/4byte operands. On 64-bit, also works on 8byte
  operands.
  - There are wide versions: `CMPXCHG8B` for comparing 8byte operands on
    32-bit x86. And `CMPXCHG16B` for comparing 16byte operands on 64-bit
    x86.
  - To avoid spanning two cache lines, these must be aligned.
- This works the usual way. Get the cache line exclusive, do the test,
  do the set.
  - This finally is conditional, so it can "fail".
  - But it is in the same broad cost class as all the others.
  - Like `XCHG` (test-and-set/atomic exchange), you may have to
    loop/retry.
- The use case is typically optimistic update attempts to lock-free data
  structures.
- The "ABA problem" can occur with naive compare-and-swap.
  - Basically, it's when memory is "reused". So if you're CAS-ing a
    pointer, you don't know if the underlying object is the same, or if
    it just happens to live at same memory address.
  - One trick is to keep an update counter side-by-side. Increment this
    on every update. Then rely on double-width CAS.
  - In theory, if update counter overflows, then ABA *might* still slip
    by (if memory also reused). But for a 64bit counter, even at 1bil
    updates/sec, it would take 500+ yrs to wrap...

# Atomic Bitwise Operations

- These are RMW instructions that work bitwise.
- Take `LOCK BTS`: bit-test-and-set. It sets a single bit `i` to `1`, and
  returns the old value of that bit.
- There's `LOCK BTR` for bit-test-and-reset. It sets bit `i` to `0`, and
  returns the old value of the bit.
- Last, there's `LOCK BTC`, which is bit-test-and-complement. It flips
  bit `i` and returns the old value.
- These work in the usual simple RMW way. Claim the line exclusive, do
  the operation, and don't give the line back until you're done.
- Useful for flags/bitmasks that need synchronization.

# Linked-Load/Stored-Conditional

- Abbreviated `LL`/`SC`. Also called load-reserved/store-conditional
- Naming comes from MIPS. Exists on ARM (`LDXR`, `STXR`), RISC-V (`LR`,
  `SC`), and Power (`ldarx`, `stdcx`). Not on x86 or x86-64.
- When you load the cache line with `LL` and read the value, the line
  gets "marked". When you do `SC` later, this only succeeds if the line
  is still reserved by you (and thus not modified elsewhere).
  - This detects *any* modification. Whereas CAS cannot detect an ABA
    style modification.
  - I believe that when you do `LL` you will just get the line shared.
    Only when you do `SC` will you upgrade to exclusive.
- `LL`/`SC` can have spurious failure for a variety of reason. A simple
  reason is: some other part of the cache line may be written by another
  core, in which case the "mark" is lost even if the value of the
  address didn't change.
- You can put arbitrary, standard instructions between the `LL` and `SC`
  calls.
  - In particular, you can implement CAS out of comparison,
    test/if/branch, and move instructions.
  - There are restrictions, so you can't do *just anything*, but we
    won't dive into that.
- You can't nest `LL`/`SC` pairs. Probably the "marking" is done in a
  special register which gets reset by a second `LL`, I would guess.

# DCAS: Double Compare-And-Swap, AKA CAS2

- This is an extension of the CAS idea.
- You have *two* targets: `target1, target2`. You have *two*
  `expected1, expected2`. And you have *two* `new_value1, new_value2`.
- If `target1 == expected1` and `target2 == expected2`, then update:
  `target1 = new_value1`, `target2 = new_value2`.
- Else, set `expected1 = target1` and `expected2 = target2`.
- The main extension is that this instruction *spans cachelines*.
- The fundamental difficulty with DCAS: claiming two lines exclusive.
  - If you must lock-and-hold two cachelines, how do you acquire them?
    You could deadlock if you `DCAS(A, B)` while someone else
    `DCAS(B, A)`. How do you detect potential deadlock and back-off?
  - You might avoid deadlock if always claim lines in increasing memory
    order.
  - ChatGPT agrees with me that there's no "partial visibility" of the
    transaction before all updates complete, so long as you hold
    exclusive on the lines until transaction completes.
- The main people who wanted DCAS were lock-free datastructure
  researchers.
  - You can make more powerful, but still simple, lock-free
    datastructures with DCAS.
  - DCAS is basically a two-operation atomic transaction. Lock-free
    structures are all about optimistically attempting atomic
    transactions.
  - But this is still fairly niche. Most of what you want to do can be
    accomplished via standard RMW atomic instructions.
- History
  - Motorola gave us true CAS2 in their 68k architecture (in 1984).
  - But others did not adopt it, and Motorola abandoned it. Because of
    the complexity, and niche use case.
  - You get a lot of what you want simply from double-wide CAS
    operations, where targets are aligned and adjacent. The double-width
    CAS especially helps prevent ABA problems.
  - Eventually Intel introduces hardware transactional memory (in
    2012/2013). But there are early bugs found in 2014. And by 2019 some
    security vulnerabilities are explored. It is largely
    disabled/removed around that time.

# Atomic Costs beyond Cache Coherence Traffic/Contention

- I have focused on coherence traffic as the major cost of atomic
  instructions. Indeed, this is the dominant cost when there is
  heavy contention.
  - "Successful" operations don't cost any more CC traffic than a write
    would.
  - If you *already* own the line exclusive, then atomic operation costs
    *nothing* in coherence traffic.
  - But instructions that can "fail" under contention may need to loop,
    and that *does* imply (1a) spinning OR (1b) calling into OS to park
    thread, and (2) additional coherence traffic.
  - Any hot line shared between cores will bounce (regardless whether it
    is the target of atomic/RMW ops), and that costs a lot of coherence
    traffic.
- The second big cost pertains to *write buffers*.
  - Atomic ops that involve writes (most of them) will not allow
    subsequent reads to be reordered before the write is propagated.
  - Thus the write buffer must drain before the thread can make
    progress.
  - That exposes write latency.
- Atomic/RMW also hurts pipelining/speculative execution.
  - A common technique in CPU architecture is to assume a conditional
    result, and begin executing instructions along that path, so long as
    the "effect" of these instructions is all "local" to the core.
  - But atomic operations cause fencing to be performed, which syncs
    state to outside world. So they cannot be speculatively executed.

# Sources

- A lot of ChatGPT discussion in 2026-05-XX.
- Older sources follow. I haven't super-reviewed them in 2026. They are
  good, but probably less valuable than ChatGPT communication.
- http://davidad.github.io/blog/2014/03/23/concurrency-primitives-in-intel-64-assembly/
  - It sounds like basically doing a LOCK operation necessarily
    means bus traffic to tell everyone to give us exclusive access
    (and further to lock them out from future access).
  - We can avoid this if we have _shared_ access already, and then
    we can just test locally and see the lock cannot be acquired.
  - When a lock is freed, our local copy will be invalidated, and
    _then_ we can acquire the lock.
- http://www.cse.iitm.ac.in/~chester/courses/15o_os/slides/9_Synchronization.pdf
- http://stackoverflow.com/questions/2538070/atomic-operation-cost
  - In Intel 486 days, apparently they did an entire lock on the memory
    bus. Starting with Pentium Pro they do a cache lock on just that
    line.
