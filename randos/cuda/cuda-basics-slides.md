## Running code on device

You mark functions you'll call from host but want to run on device
with `__global__`. You use the `mykernel<<<1, 1>>>()` syntax to
call. You use `nvcc` to compile. You need to add an extension of
`.cu`.

Now, let's start writing a function to add two numbers:

```
__global__ void mykernel(int* a, int* b, int* c) {
  *c = *a + *b;
}
```

Great! Next, `a, b, c` all need to be pointers to *device*
memory. It's no good to give *host* memory! There's a `cudaMalloc,
cudaFree, cudaMemcpy`.

So we can write a main like so:

```
int main() {
  // Host
  int a, b, c;
  // Device pointers
  int *d_a, *d_b, *d_c;

  // Allocate memory on device. Store pointers on host.
  cudaMalloc((void**) &d_a, sizeof(int));
  cudaMalloc((void**) &d_b, sizeof(int));
  cudaMalloc((void**) &d_c, sizeof(int));

  // Initialize host values.
  a = 4;
  b = 3;

  // Copy host values to device.
  // Not sure why I need void** above but not here?
  cudaMemcpy(d_a, &a, sizeof(int), cudaMemcpyHostToDevice);
  cudaMemcpy(d_b, &b, sizeof(int), cudaMemcpyHostToDevice);

  // Run kernel.
  mykernel<<<1, 1>>>(d_a, d_b, d_c);

  // Copy device values to host
  cudaMemcpy(&c, d_c, sizeof(int), cudaMemcpyDeviceToHost);

  // Deallocate memory.
  cudaFree(d_a);
  cudaFree(d_b);
  cudaFree(d_c);

  printf("Result: %d\n", c);

  return 0;
}
```

## Running parallel code

We can change `mykernel<<<1, 1>>>` to run `mykernel<<<N, 1>>>` to run
`N` times in parallel. We're going to try to do vector addition.

Each parallel invocation is called a *block*. They are arranged in a
grid, with an `x` and a `y` I believe.

We can change

```
__global__ void mykernel(int* a, int* b, int* c) {
  c[blockIdx.x] = a[blockIdx.x] * b[blockIdx.x];
}
```

Here `blockIdx` is a block-local special variabl to give the idx of
the block.

```
// Probably needs to be a define since we'll use it in <<<
#define N 512

int main() {
  // Host
  int *a, *b, *c;
  // Device pointers
  int *d_a, *d_b, *d_c;
  int arraySize = N * sizeof(int);

  // Allocate and initialize host memory.
  a = (int *) malloc(arraySize);
  b = (int *) malloc(arraySize);
  c = (int *) malloc(arraySize);
  for (int i = 0; i < N; i++) {
    a[i] = i;
    b[i] = N - i;
  }

  // Allocate memory on device. Store pointers on host.
  cudaMalloc((void**) &d_a, arraySize);
  cudaMalloc((void**) &d_b, arraySize);
  cudaMalloc((void**) &d_c, arraySize);

  // Copy host values to device.
  cudaMemcpy(d_a, a, arraySize, cudaMemcpyHostToDevice);
  cudaMemcpy(d_b, b, arraySize, cudaMemcpyHostToDevice);

  // Run kernel.
  mykernel<<<N, 1>>>(d_a, d_b, d_c);

  // Copy device values to host
  cudaMemcpy(c, d_c, arraySize, cudaMemcpyDeviceToHost);

  for (int i = 0; i < N; i++) {
    printf("Result %d: %d = %d * %d\n", i, c[i], a[i], b[i]);
  }

  // Deallocate memory.
  free(a);
  free(b);
  free(c);
  cudaFree(d_a);
  cudaFree(d_b);
  cudaFree(d_c);

  return 0;
}
```

Now, you can simply say `mykernel<<<1, N>>>` to flip things
around. You'll now need to use `threadIdx.x`. The idea is that many
threads run on each block. Blocks and threads run in parllel. You can
do up to 512 threads per block. All blocks will have the same number
of threads. So `<<<1, 512>>>` means one block with 512 threads.

The reason for threads inside a block run on the same *stream
processor*. They can communciate with each other via shared memory,
and they can use synchronization primitives with each other. Blocks
cannot do that, and I guess run totally independently.

Now, we can change the kernel slightly to work with multiple blocks
*and* threads:

```
__global__ void mykernel(int* a, int* b, int* c) {
  int idx = (blockIdx.x * blockDim.x) + threadIdx.x;
  c[idx] = a[idx] * b[idx];
}
```

Here `blockDim.x` means the number of threads in a block. Now you can
set things up however you like:

    mykernel<<<NUM_BLOCKS, NUM_THREADS_PER_BLOCK>>>(d_a, d_b, d_c)

## Using Threads

So you can use `__shared__` to allocate some "on-chip" memory, which
is shared by all threads in the block, and is closer than the on
device memory. This is like a user managed cache. Threads in other
blocks cannot see this, though!

Here's the kernel:

```
__global__ void mykernel(int *xp, int *result) {
  int globalIdx = (blockIdx.x * blockDim.x) + threadIdx.x;
  int localIdx = threadIdx.x + WINDOW_SIZE;

  // Keep a local buffer that's faster to read from.
  // nvcc really wants this to be a constant sized array. Thus I had
  // to make BLOCK_SIZE a constant.
  __shared__ int buffer[BLOCK_SIZE + 2 * WINDOW_SIZE];
  buffer[localIdx] = xp[globalIdx];
  // At edge of block may have to copy extra elements. Unless at the
  // edge of the entire array.
  if ((threadIdx.x < WINDOW_SIZE) && (0 <= (globalIdx - WINDOW_SIZE))) {
    buffer[localIdx - WINDOW_SIZE] = xp[globalIdx - WINDOW_SIZE];
  }
  if (((threadIdx.x + WINDOW_SIZE) >= blockDim.x)
      && ((globalIdx + WINDOW_SIZE) < NUM_ELEMENTS)) {
    buffer[localIdx + WINDOW_SIZE] = xp[globalIdx + WINDOW_SIZE];
  }
  // Need this so everyone pauses and doesn't race ahead.
  __syncthreads();

  int resultValue = 0;
  for (int i = -WINDOW_SIZE; i <= WINDOW_SIZE; i++) {
    resultValue += buffer[localIdx + i];
  }

  result[globalIdx] = resultValue;

  return;
}
```

## Other

Launching a kernel is *async*. When you do a `cudaMemcpy` it will wait
until all kernels are done. You can also use `cudaMemcpyAsync` but I
don't know why you would want to.

Cuda calls always return an error code. But sometimes this is an error
that is just now realized because it was an async error!

You can also have CUDA use multiple GPUs. There are simple functions
to select devices.

Last, we didn't look at `gridDim`. This would tell you *how many
blocks there are*.

## Sources

http://www.nvidia.com/docs/io/116711/sc11-cuda-c-basics.pdf
