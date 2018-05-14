Everyone says that native threads don't scale because memory. Stack
size is typically 2MB. So thousands of threads means GBs of memory for
the stacks.

But that is crazy, because VM. Uses 2MB of *address space*, but not
2MB of physical memory. More like 4KB prolly. With 64bit system,
address space is virtually unlimited.

It seems like the bigger problem is that context switches of native
threads go through the kernel, which requires kernel mode/syscall.

You do need system call for at least thread creation. For context
switch?

Honestly, I think to resolve this issue I would have to read more of
the Linux source code...

http://www.inf.ed.ac.uk/teaching/courses/os/slides/04-thread18.pdf
https://www.quora.com/Why-are-system-calls-expensive-in-operating-systems
