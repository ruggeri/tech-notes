## Static Libraries

When compiling you produce a bunch of object files. These are your
binary code.

When you link statically, all those object files are merged into an
executable. Any unused code in the object files is stripped out. Heavy
link time optimization can occur at this point.

## Dynamic (Shared) Libraries

Dynamic libraries are loaded dynamically when the program starts using
the `dlopen` command. One copy of the library can live on disk, and be
paged *once* into memory, where it will be used by multiple
applications.

## Pros/Cons of Static/Dynamic Libraries

Because dynamic libraries are shared, they take up less space on disk
and RAM (smaller executable size). It also allows the system to easily
upgrade that dynamic library, and all the programs that dynamically
load it will get access to new versions. This is especially useful for
security fixes; otherwise, every application that statically compiles
this library in will have to be recompiled.

Dynamic libraries can be a little slow to load on program start. Also,
you lose the opportunity to do post-link optimization and
inlining. Importantly, they reduce reproducability; the version of the
dynamic library is not necessarily the one you built the program
against. If the provider is not careful, they can subtly change the
API of the library causing unintended side-effects in your program.

In olden days (esp Windows), the implementation of shared libraries
was really bad, causing serious DLL hell. In particular, only one
version of a library could be in use at any time; whichever version
was loaded first won.

Google and Facebook both statically compile all their dependencies,
but much end-user software uses dynamic linking. Some Google/Facebook
binaries are up to a GB in size! Libraries like X/Qt/KDE/WebKit can be
very large. Accumulated over many programs, this bloat can add up to a
large amount of disk space.

## Further Discussion

* https://news.ycombinator.com/item?id=9629663
    * Discusses: http://harmful.cat-v.org/software/dynamic-linking/
* https://news.ycombinator.com/item?id=4111642
* http://harmful.cat-v.org/software/dynamic-linking/versioned-symbols
    * Further DLL hell...

## glibc

`glibc` provides the POSIX and C standard library headers. To
implement this functionality, `glibc` uses Linux kernel
functionality. To do this, it calls into kernel code by pushing a
"system call number" into a register and firing an interrupt that
triggers a transition to kernel mode. The kernel can then figure out
what method the syscall the application wanted by the call number
pushed on the stack/into the register.

For that reason, glibc can be statically linked into the
application. However, this will not be portable to other operating
systems if the relevant system calls have different syscall numbers.

In particular, you can't use `glibc` on OSX. `libc`/`glibc` are all
about abstracting the kernel syscall interface. Different kernels are
going to have different syscall interfaces.

## X11

A good question is about binary inter-process communication. A good
example might be X11; X11 runs a server, and programs try to connect
to the server and tell it to draw stuff.

In this case, even if the user API is staying the same, the internal
data structures might be changing. That is, if I call `drawRect()` in
my client program, that might send different binary messages to the
server in different versions. For that reason, it might be preferable
to use a dynamically linked library here to maximize compatibility.

This is total unjustified speculation. It's just that I've read about
how dynamic linking can also increase modularity. It just seems that
other software which is "part of" the operating system (like X11)
should provide shared libraries just like glibc.

I think also DLLs are also sometimes used to add after-the-fact
functionality to existing software. These can be modules that can be
configured to be loaded. I've read about text files that you can
change to load new binary modules. I think this is sometimes called
*dynamic loading* (as oppposed to linking). I think this is actually
what `dlopen` is about.

## Microkernel Note

Kernels do a lot of the interaction with the hardware. User code does
syscalls into the kernel; this is done from C programs primarily
through glibc.

A CPU usually has two *modes*: supervisor and user. Supervisor is the
kernel, which can directly manage memory, access any physical
location, yadda yadda. User mode is what programs run in; they are
isolated.

Over time, functionality accreted into the kernel. This included lots
of low-level, hardware type stuff needed by many programs. This mean
the kernel was large, complex, and kinda hard to develop. Microkernels
were an idea where the kernel would *only* be responsible for IPC,
memory management, and thread scheduling. Other services, like TCP/IP
would be in user space programs running in daemon mode; programs that
needed that functionality would do IPC. Then, any crash in that module
could be fixed by restarting the daemon, not taking down the entire
system.

It was relatively hard to make this performant, because of the
indirection required.

NB: CPUs often have other ring modes, with layers of permission, but
these turned out not to be useful to OS developers.
