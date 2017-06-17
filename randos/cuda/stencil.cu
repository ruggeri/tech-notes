#include <stdio.h>

#define NUM_BLOCKS 8
#define BLOCK_SIZE 64
#define WINDOW_SIZE 3
#define NUM_ELEMENTS (NUM_BLOCKS * BLOCK_SIZE)

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

int expectedResult(int *xp, int position) {
  int expectedResult_ = 0;

  for (int i = -WINDOW_SIZE; i <= WINDOW_SIZE; i++) {
    if ((position + i) < 0) continue;
    if ((position + i) >= NUM_ELEMENTS) continue;
    expectedResult_ += xp[position + i];
  }

  return expectedResult_;
}

int main() {
  int arrayBytes = NUM_ELEMENTS * sizeof(int);

  // Allocate and set host memory.
  int *xp = (int*) malloc(arrayBytes);
  int *result = (int*) malloc(arrayBytes);
  for (int i = 0; i < NUM_ELEMENTS; i++) {
    xp[i] = i * i;
  }

  // Allocate and set device memory.
  int *deviceXp;
  int *deviceResult;
  cudaMalloc(&deviceXp, arrayBytes);
  cudaMalloc(&deviceResult, arrayBytes);
  cudaMemcpy(deviceXp, xp, arrayBytes, cudaMemcpyHostToDevice);

  // Run kernel and copy result back to host.
  mykernel<<<NUM_BLOCKS, BLOCK_SIZE>>>(deviceXp, deviceResult);
  cudaMemcpy(result, deviceResult, arrayBytes, cudaMemcpyDeviceToHost);

  // Verify result.
  for (int i = 0; i < NUM_ELEMENTS; i++) {
    int expectedResult_ = expectedResult(xp, i);
    if (result[i] == expectedResult_) continue;
    printf("result[%d] = %d != %d\n", i, result[i], expectedResult_);
  }

  // Free all the memory!
  free(xp);
  cudaFree(deviceXp);
  free(result);
  cudaFree(deviceResult);

  return 0;
}
