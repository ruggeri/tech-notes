`fork` creates a new process. It either returns -1 to indicate an
error, 0 to indicate you are the child process, or the pid of the
child process.

`fork` is the original (and in the beginning only) way to create a
child process.

`fork` gives an entirely new copy of the original program's memory. As
you expect: different processes are isolated from each other. The
memory is treated copy-on-write style.

A very common use of `fork` is to immediately call `exec` so that you
can run a different command. This is what shells do for sure. Almost
everything is *replaced* by `exec`: machine code, data section, heap,
and call stack.

Interesting note on `exec`: it does return a value, but only if it
fails! Otherwise it cannot: the process was replaced!

One thing that is passed to the subprocess is any open file
descriptors (unless they were marked as `O_CLOEXEC` on open or with
`fcntl` and `FD_CLOEXEC`).

Aside: if you call `fork`, you should call `wait`. Because the parent
might want the exit status, an entry in the process table won't be
"reaped" until someone `wait`s on it. If you don't get `wait`ed,
you'll be a resource leak of pids. One good thing is that zombie
processes do have their memory deallocated so they truly only take up
space in the process table.

`wait` can be called in the sequential code that did the `fork`, or
alternatively it can be done in a signal handler for `SIGCHLD`.

Now, what if the parent dies? Then the parent process of the `fork`ed
process becomes `init`, which always `wait`s its children to reap
them. So the only kind of program that can leak child pids are
long-running programs that create lots of children without `wait`ing
them. AKA: shells.

Briefly, some more info on the copied file descriptors. The parent and
the child processes can both use the file descriptors, but they better
be careful not to do uncoordinated writes to the same file. This is
desirable if (1) the parent wants to hand off control for writing open
files to the child (at least until maybe the child exits), or (2) the
parent wants to use an open pipe to communicate with the child.

The `system` command is a convenience. It does a fork-exec for you
(and also wildcard expansion shell-style). `system` is a C standard
library command. POSIX uses fork and exec, but presumably other OSes
can do things differently (the standard talks about "command
processor" I think). `system` waits for the subprocess to finish.

There is another command called `posix_spawn`. It also does a
fork-exec, but can be a little smarter. For instance: if overcommit of
virtual memory is turned *off*, then forking really *does* copy all
the memory. `posix_spawn` avoids that. Obviously `posix_spawn` is part
of POSIX and not part of the C standard, which is different from
`system`. Even if you do want to wait synchronously, `posix_spawn` can
be a lot faster than `system` because it doesn't invoke the shell
(presumably), and of course its standard benefit over fork-exec.

But what you want to know about next is threads. They weren't
originally part of the C standard; they were in pthreads (aka, POSIX
threads). You have `pthread_create` and `pthread_join`. You have a
`pthread_t` object. You also have `pthread_exit`; when called this can
provide a value back to the user (it's implicitly called when
returning out of the original function). You can `pthread_detach` if
you're like: I'm never going to bother joining this, but I don't want
to leak memory.

pthreads gives you mutexes, rwlocks, condition variables...

How is `pthreads` implemented on Linux? Using the `clone` system
command. `clone` creates a new process, just as before, but this one
shares virtual memory. I believe the new process has the same pid, but
different thread_id or somesuch.

You don't say "fork a thread". You say "spawn a thread." You might
also say that fork "spawns a new process." Or you can say "fork a
process".

Note: calling `fork` when using threads is dangerous. When forking,
only the calling thread will be duplicated. But that means that in the
new address space, there are none of the other threads that
existed. The problem then is: what if one of those other threads held
a mutex? That mutex is never going to get released. For instance:
`malloc` takes a lock. So `fork` is probably not what you want when
using threads.

Sources:

* Many Wikipedia entries (fork, exec, fork-exec, zombie process).
* https://unix.stackexchange.com/questions/91058/file-descriptor-and-fork
* https://news.ycombinator.com/item?id=8204350
* https://thorstenball.com/blog/2014/10/13/why-threads-cant-fork/
