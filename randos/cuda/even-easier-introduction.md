There is now a *unified memory* that CUDA offers. Allocate memory via
`cudaMallocManaged` and this memory is accessible via both CPU *and*
GPU!

One of the advantages is that this lets memory be *migrated* from host
to device as needed. So if you have deeply nested datastructures, you
may not have to move over all allocations. OTOH, that sounds like
something you don't want: you don't want to slowly traverse a
datastructure in the device and then fault and then need to access the
host memory...

Because you can just get the answer back at the host whenever, you may
want to use `cudaDeviceSynchronize` to *wait* for the answer. But I'm
not sure what would happen if the kernel is not done and you try to
read the CPU data? Is part of `cudaDeviceSynchronize` that it
reconciles host and device memory?

Indeed the CPU must not use access unified memory while the GPU is
running tasks. You must use synchronize.

But this does make things much easier to port over, at cost of making
a new implicit requirement of the programmer.

Note: if you start multiple kernels, they just enter a queue.
