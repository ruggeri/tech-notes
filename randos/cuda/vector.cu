#include <stdio.h>

__global__ void mykernel(int* a, int* b, int* c) {
  int idx = (blockIdx.x * blockDim.x) + threadIdx.x;
  c[idx] = a[idx] * b[idx];
}

// Probably needs to be a define since we'll use it in <<<
#define NUM_BLOCKS 8
#define NUM_THREADS_PER_BLOCK 64

int main() {
  // Host
  int *a, *b, *c;
  // Device pointers
  int *d_a, *d_b, *d_c;
  int numElements = NUM_BLOCKS * NUM_THREADS_PER_BLOCK;
  int arraySize = numElements * sizeof(int);

  // Allocate and initialize host memory.
  a = (int *) malloc(arraySize);
  b = (int *) malloc(arraySize);
  c = (int *) malloc(arraySize);
  for (int i = 0; i < numElements; i++) {
    a[i] = i;
    b[i] = numElements - i;
  }

  // Allocate memory on device. Store pointers on host.
  // Pretty sure I don't need void** cast here.
  cudaMalloc((void**) &d_a, arraySize);
  cudaMalloc((void**) &d_b, arraySize);
  cudaMalloc((void**) &d_c, arraySize);

  // Copy host values to device.
  cudaMemcpy(d_a, a, arraySize, cudaMemcpyHostToDevice);
  cudaMemcpy(d_b, b, arraySize, cudaMemcpyHostToDevice);

  // Run kernel.
  mykernel<<<NUM_BLOCKS, NUM_THREADS_PER_BLOCK>>>(d_a, d_b, d_c);

  // Copy device values to host
  cudaMemcpy(c, d_c, arraySize, cudaMemcpyDeviceToHost);

  // Deallocate memory.
  cudaFree(d_a);
  cudaFree(d_b);
  cudaFree(d_c);

  for (int i = 0; i < numElements; i++) {
    printf("Result %d: %d = %d * %d\n", i, c[i], a[i], b[i]);
  }

  return 0;
}
