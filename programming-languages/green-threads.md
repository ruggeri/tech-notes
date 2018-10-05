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

Possibly quite useful?
https://stackoverflow.com/questions/9964899/why-are-os-threads-considered-expensive

## The Green Place Review

Hgih quality resource! He says that thread switching on Linux is now
super fast. He does indicate that Goroutines seem to be many orders
less overhead. BUT he also notes that the amount of *actual* work done
should be orders of magnitude more than the switching cost. His
synthetic benchmark did basically nothing.

Source: https://eli.thegreenplace.net/2018/measuring-context-switching-and-memory-overheads-for-linux-threads/

This next post then is confusing. He talks about split stacks, and I
don't understand why. He talks about switching cost, but I don't know
why.

Source: https://eli.thegreenplace.net/2018/go-hits-the-concurrency-nail-right-on-the-head/

The HN responses aren't super helpful. But someone does mention: that
"switchto" support in the kernel would make kernel thread switching
competitive with goroutine switching.

Source: https://news.ycombinator.com/item?id=18141461

## Briefly reviewed

https://www.youtube.com/watch?v=KXuZi9aeGTw&feature=youtu.be (didn't watch)

I read the slides at:

https://blog.linuxplumbersconf.org/2013/ocw/system/presentations/1653/original/LPC%20-%20User%20Threading.pdf

Basically says that the scheduling decision takes the most time? But
why?

## Other

https://www.codeguru.com/cpp/sample_chapter/article.php/c13533/Why-Too-Many-Threads-Hurts-Performance-and-What-to-do-About-It.htm

One problem that seems to be indicated here is that a thread can be
slept while it still holds a lock other people need.

Personal correspondence with Jonathan/Haseeb.

    https://mail.google.com/mail/u/0/#sent/KtbxLvHPwVfWrWMTbvBQjWsJTznxrnRbjq
