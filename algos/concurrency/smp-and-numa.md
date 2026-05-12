# SMP vs Asymmetric Multi-Processing

SMP means that all processors are peers. Examples of SMP are traditional
multiple cores on a die, or multiple sockets on a motherboard. In an SMP
system, you typically have:

- Any CPU can run kernel code,
- One shared address space,
- OS can schedule any task on any node.

Examples of asymmetric processing are:

- When OS uses some cores or processors for a subset of tasks. They may
  be the same processor/core, but the OS uses them for different tasks.
- When there are "high-performance" cores and "efficiency" cores on a
  die.
- A GPU offers asymmetry: it has a different instruction set, and can do
  certain kinds of processing faster/more efficiently.
- Microcontrollers (control sensors, radio) or coprocessors (network,
  storage controllers, audio processor)...

Multi-processing has the best benefit when there are many tasks to run
simultaneously. This happens when there are many users to the machine
(maybe a multi-user server), or many requestors (a web or database
server). SMP helps when you want to distribute processing load, and you
have many independent tasks.

Scope for benefit from many processing units is less when there is a
single user usually performing a single task. It matters how
parallelizable the user's task is. If the user commands the machine to
perform many independent tasks simultaneously (like compressing 1k high
resolution images), then this task *can* benefit from many processors,
so long as there is enough storage/memory bandwidth so that
reading/storing the images is not a bottleneck...

# SMT: Simultaneous Multi-Threading

You can have a multicore processor. But you can *also* have something
called *simultaneous multi-threading*. This involves a *single* core
running two threads at the same time. Intel's SMT implementation is
called *Hyper-Threading* (2002), AMD just calls their SMT (2017).

SMT exposes two logical cores, but the OS is not going to use the
capability well unless it understands this is really *one* core. For
instance, if there are only two tasks, schedule these on two distinct
cores, not as two SMT tasks on a single core.

The basic idea of SMT is that core when one part of pipeline is stalled,
will automatically start running the other thread. In some ways, it
helps if tasks are "complementary", so that one can use a free resource
while the other is stalled doing something else. On the other hand, they
share a cache, so it helps if they want the same lines. Last, note that
this has a security side-channel implication, which the OS might want to
be aware of.

# Uniform Memory Access

When you have multiple processors/cores all with similar access to
memory, we call it UMA. This is typically implemented by a shared bus to
RAM. The major problem is that the shared bus limits memory bandwidth.
Latency also often degrades as you try to scale up the number of
processors connected to memory.

To increase memory bandwidth, you can add more memory channels. That
allows for more requests and more responses in parallel, increasing
bandwidth. SDR to DDR transition went from 1x to 2x 8byte channels to a
memory module. With more modules (and thus more channels), there is more
RAM bandwidth.

Still, everything must go through a single memory controller. So when
memory controller bandwidth becomes the bottleneck, you start to want
one per processor. Also, you can put each memory controller "closer" to
the processor, which improves latency. Over time, memory controller was
moved on die (AMD in 2003, Intel 2008), which demonstrates that
proximity really does matter.

But it's really not just about the controller. It's the whole memory
subsystem. It gets harder-and-harder to increase memory bandwidth, even
if one component (DRAM modules) keeps getting faster. As just one
example, there's also more-and-more coherency traffic, which is limiting
memory subsystem scalability.

Sometimes, SMP and UMA seem to be used synonymously. Partly that is
historical: SMP used to often imply UMA. And NUMA degrades how
"symmetric" SMP systems truly are.

# Data Starvation

Over time, clock rate has pushed higher. But if the CPU is starved for
data, that doesn't really help.

If aggregate bandwidth stays the same, then adding more cores may not
help. While bandwidth *has* substantially increased even through 2026,
per-core bandwidth may have gone down slightly. And memory latency is
not improving very quickly at all. We expect cores of 2026 to be *even
more* starved for data than ever.

On the other hand, we benefit from better branch predictors and larger
caches. And, if we write code that has "memory-level parallelism", we
can do multiple requests to RAM simultaneously (rather than serially),
which helps mask latency. Similarly, if we use vectorized instructions
(SIMD: single instruction multiple data, Intel AVX is an example), or
even more complicated matrix operations (Intel AMX: advanced matrix
extensions), we can do *more* compute per cache line.

That said, once vector or matrix operations become the primary workload,
you may want an entirely different processor architecture (GPU).

The takeaway may be: some tasks (especially those with a lot of pointer
chasing) may be *even more* bottlenecked than ever by starvation for
data, though aggregate performance *might* improve because of improved
bandwidth and more cores. But tasks that do a lot of SIMD or matrix
operations might be accelerated more relative to other software.

# NUMA

This is _non uniform memory access_. The basic idea is that some
memory is closer to some processors. This means not everyone is using
a single shared bus, reducing contention. Of course, this only
improves things if different tasks are working mostly with a subset of
the data.

Basically, you can see NUMA as a tightly-coupled form of cluster
computing.

To benefit from NUMA, you need independent tasks that won't touch each
other's pages very often. Virtualization is a good example. The OS needs
to know to put sibling tasks in the same NUMA domain, and independent
tasks on different domains. If it needs to move a task across a domain,
then the OS needs to know to migrate the VM page, too, to avoid access
across the domain.

Most of the NUMA awareness is at the OS level. But performance-critical
software where there are many threads, lots of memory, and shared state
will want to be NUMA aware. Definitely databases, and also hypervisors
managing many virtual environments. Maybe HPC simulations/finite-element
solvers. ML inference servers.

Examples of programs that are *not* NUMA aware include games, web
browsers, desktop applications. Applications which are multi-threaded
(including games) may not be *NUMA* aware if (1) they target users who
are unlikely to have NUMA, so added software complexity is not
justified, and (2) web browsers tasks may mostly be independent, so you
can just let the OS choose where to run the task and get most of the
benefit. The benefit of NUMA awareness comes when the program knows
something the OS cannot infer.

# Modern Multi-Socket in 2026

On modern (2026 era) processors, the cores on a processor may have
somewhat non-uniform access to the memory. But usually the entire
processor is still exposed as a single memory domain to the operating
system.

NUMA continues in 2026 to be about multiple processors in multiple
sockets. Now that core counts on a single die are crazy high (up to
192), and you can run 12x DDR5 memory channels, one processor may give
you all the performance you need. On the other hand, if memory bandwidth
is still the bottleneck, adding a second socket can help, because not
all cores are contending for the same memory. But you only get that
benefit if tasks are really independent and you need very little
communication across memory domains (like virtualization).

In truth, you start to run into problems running many sockets when each
socket can draw 500W by itself, and you need to remove that heat. So
boards with more than two sockets are even rarer than ever.

# ccNUMA: Cache-Coherent NUMA

This is about cache-coherence across NUMA domains.

## non-ccNUMA

The classic non-ccNUMA machines were supercomputers in the 80s and 90s.
This includes the BBN Butterfly and the Cray T3D. These machines might
share an address space, but RAM across the NUMA domain might not be
cached at all. To communicate/coordinate across NUMA domains, you might:

- Use message passing to send messages (including requests) across
  domains, but not otherwise read/write another domain's memory,
- You might be able to read/write from a remote address, but the data
  might not be cached. That has an obvious performance cost, but it
  should be "correct."
  - A variant is where you get an API that allows you to request a chunk
    of remote memory. This batches up the remote read/write work.
- You might have explicit instructions to invalidate a line. So you let
  values be cached across NUMA domains, and you rely on software to tell
  you when remote values should be invalidated.

Multi-threaded programs would need to know they are running on hardware
without cache-coherency, since coherency is often assumed (and thus
necessary) for program correctness.

## ccNUMA

In part, NUMA shouldn't need cc across domains because cross-domain is
always going to be slow. If you need cc, maybe you can schedule in a
single domain, and if you don't need cc, don't pay for it.

But if you can't avoid some incidental sharing across domains, then it
might keep things easy if you do have cc across domains. You won't gain
high performance when you reach across domains, but you can keep things
simple. This allows incidental/occasional cross-domain without making
this part of the program obnoxiously difficult and soaking up
engineering effort.

This also ensures that NUMA-unaware software can be scheduled across
domains without breaking the software.

ccNUMA is typically implemented with directory based cache coherency
strategy. See `cache-coherency.md`. This is an alternative to snooping
all the traffic. The idea is that you have limited bandwidth/high
latency between sockets, so you can't waste it on a bunch of unnecessary
coherency traffic. And most coherency traffic is only relevant to the
socket it is generated on.

## NUMA History

In the 90s, Intel multiprocessor systems were typically SMP/UMA. Only in
2003 did AMD integrate the memory controller in Opteron and introduce
ccNUMA to x86. Non-cc NUMA was never a thing in x86, or really past the
80s/early 90s even with other architectures.
