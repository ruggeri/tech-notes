## Intro: Basics

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

## Structure

There are language extensions that nvcc handles. Then it will link in
a runtime; CUDA calls that runtime functionality. The runtime itself
loads a CUDA driver and uses that to actually perform the work. You
can use the driver API if you want, but that is atypical.

The nvcc tool seperates the host and device code, compiling the host
code to PTX, which is the low level assembly of CUDA, or cubin, which
is executable CUDA code. It also changes the `<<<...>>>` syntax in the
host code to appropriately load and run the device code.

nvcc will produce object code you can run, or you can use the CUDA
driver API to explicitly dynamically load and run the PTX or cubin
codes. If you load PTX it will be compiled to cubin by the driver. I
believe the idea is that if you distribute PTX, then you automatically
get the benefit of new compiler without having to recompile. More
importantly, it is the only way to run on GPUs newer than the
original.

You can give the `-code=sm_35` flag to specify producing cubin for
compute capability 3.5. Some PTX instructions only exist on newer
hardware. Therefore you can specify `-arch=sm_35` to produce PTX that
takes advantage of features in compute capability 3.5. The generated
PTX will work for newer compute capability versions, but not for
older.

Here is an example:

```
nvcc x.cu \
        -gencode arch=compute_20,code=sm_20
        -gencode arch=compute_30,code=sm_30
        -gencode arch=compute_35,code=\'compute_35,sm_35\'
```

The first two lines say: use features of compute 2.0 and 3.0, and
build object code for 2.0 and 3.0. No PTX will be generated. The third
line says: build *both* PTX and object code for 3.5, using all
features of 3.5. The host code will automatically use whichever is
appropriate.

Note that as a shortand you can use `-arch`, which used alone will set
code to be both object and PTX, or you can use `-arch` and `-code`
both. I'm not sure whether you can use `-code` alone.

## Cudart

This is the CUDA runtime. It is linked statically or dynamically.

The first thing it provides are `cudaMalloc` and `cudaFree`.

There are also `cudaMallocPitch` and `cudaMalloc3D`; these appear to
try to align 2d and 3d arrays appropriately?

They discuss `__shared__`, which is used to mark static allocations
that are shared by all threads in a block. They do a cool example with
matrix multiplication. Here the threads use `__syncthreads` in a loop,
which is interesting!

They talk about *page locked* memory. This is host memory that has
been marked not to be paged out by the OS. CUDA gives you a function
for this. Obviously this is a limited resource. Now: why? It looks
like maybe there is higher bandwidth? It also looks like you can then
maybe map this memory into the device address space? I suppose it
won't go through the OS then, because it is locked? It'll go over
PCIe? The docs mention unified address space here, but that will be
discussed in an appendix it looks like?

Moving on to asynchronicity. Launching kernels and their computation
is asynchronous. Memory transfers can be async. For compute capability
2.x and higher, multiple kernels can be run concurrently.

So if these operations are concurrent, how do you then sequence some
operations that must be sequenced? For instance, say you want to copy
over some data to the device, then launch a kernel. The answer is a
*stream* concept. Here, stream means stream of instructions
that will be performed one-by-one.

For most functions, there is an async version which lets you specify
the stream. When launching a kernel, you can also specify the
stream. If you don't, the *default stream* is used, so if you only
have one stream of commands, things run in sequence.

To synchronize streams, you can use `cudaDeviceSynchronize` which
syncs all streams. Else you use `cudaStreamSynchronize` for a single
stream to wait for it to be done.

I will admit that the rules seem terribly complicated. It looks like
async copies can be done concurrently. And it looks like often kernels
can be run concurrently. But for reasons I don't understand, sometimes
not...

You use `cudaSetDevice` to specify which device you want to use.

## UVA and Unified Memory Etc

From the beginning you could pin a page. If you did this, then I think
copy was faster.

Next, you could use `cudaHostAlloc` with the flag
`cudaHostAllocMapped`. This gives you a pointer accessible from both
host and GPU. Memory will still be moved from host to GPU via PCIe,
but this is not coordinated by the user now. Note: `cudaHostAlloc`
will page-lock for you.

Now, if you do random access, the GPU is going to have to load a bunch
of pages and page in/out the device memory, causing performance
problems. But you were gonna do that anyway for `cudaMemcpy`, right?
It's just that `cudaMemcpy` made this more explicit, I guess. This is
called *zero-copy*. This came in CUDA 2.0.

UVA was added in CUDA 4.0. Now, memory allocated via `cudaMalloc` and
`cudaHostAlloc` share an address space, and thus can be differentiated
by CUDA. Now when you use `cudaMemcpy`, it can just be told
`cudaMemcpyDefault`. In particular, you can now copy between devices
easily without going through the host. This requires you to use a
feature called *peer-to-peeer*, which lets GPUs directly communicate
without host.

To be clear: you can just dereference in a kernel either CPU or
another GPU pointer. That's provided you used `cudaHostAlloc` for a
host allocation, in which case it is *pinned*.

Now, I believe that a read from the CPU might be *cached*, but it will
not be *migrated* by UVA. So if the cache is flushed out (and the
cache is fairly small: certainly not 12GB!), then you need to do an
access over PCIe again.

This, I believe, is where *unified memory* comes in. Here, you now no
longer use `cudaHostAlloc` for the host. Instead you use
`cudaMallocManaged`, which gives you memory that can be dereferenced
at host or device. You can modify this at host or device. You need not
use `cudaMemcpy`, though you must make sure not to mutate at host
while mutating at device.

The advantage over UVA is first that this memory need not be
pinned. Second, access is not always over PCIe; instead, the memory is
paged into the device's global memory as needed.

Sources:
http://www.drdobbs.com/parallel/unified-memory-in-cuda-6-a-brief-overvie/240169095
https://devblogs.nvidia.com/parallelforall/unified-memory-in-cuda-6/

## TODO

* 4: Hardware Implementation
* 5: Performance Guidelines
* All leterred appendices. Especially J: Unified memory programming.

## Primary Source

http://docs.nvidia.com/cuda/cuda-c-programming-guide/index.html
