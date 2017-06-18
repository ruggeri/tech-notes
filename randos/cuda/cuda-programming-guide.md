You break up into blocks of work, which the CUDA program schedules on
stream processors. These are independently executed, could be done in
parallel, or maybe sequentially if more blocks than there are stream
processors. Each block can consist of threads, which share memory and
can use barrier synchronization.

A thread block typically can run up to 1024 threads. It sounds like
you want to run on all SMs (stream multiprocessors) and run with all
threads.

To have a multi-dimensional thread:

    dim3 threadsPerBlock(N, N);

That gives you `N*N` threads.
