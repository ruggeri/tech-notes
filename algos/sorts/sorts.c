// The problem with this approach is that it (1) unnecessarily copies
// left and right sides, as well as (2) uses 2n extra memory cells,
// since the space allocated for the merge is set aside even while
// we're sorting the left/right.
//
// Another way is to allocate a single extra array, and merge 1-el
// subarrays into the tmp space, then merge 2-el subarrays back into
// the original, etc. This only copies as part of the merge, and uses
// n extra memory cells.
//
// I would like a recursive definition of that procedure.

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

typedef int (*SortFn)(void* obj1, void* obj2);

typedef struct {
  void* arena;
  size_t length;
  size_t objectSize;
  void* parent;
  SortFn sortFn;
} MergeSortStack;

void _merge(MergeSortStack sortStack, MergeSortStack leftSortStack, MergeSortStack rightSortStack) {
  size_t leftIndex = 0;
  size_t rightIndex = 0;

  while ((leftIndex + rightIndex) < sortStack.length) {
    void* target =
      sortStack.parent + ((leftIndex + rightIndex) * sortStack.objectSize);
    void* leftEl =
      leftSortStack.parent + (leftIndex * sortStack.objectSize);
    void* rightEl =
      rightSortStack.parent + (rightIndex * sortStack.objectSize);

    if (leftIndex == leftSortStack.length) {
      memcpy(target, rightEl, sortStack.objectSize);
      rightIndex += 1;
      continue;
    } else if (rightIndex == rightSortStack.length) {
      memcpy(target, leftEl, sortStack.objectSize);
      leftIndex += 1;
      continue;
    }

    int comp = sortStack.sortFn(leftEl, rightEl);
    if (comp < 0) {
      memcpy(target, leftEl, sortStack.objectSize);
      leftIndex += 1;
      continue;
    } else {
      memcpy(target, rightEl, sortStack.objectSize);
      rightIndex += 1;
      continue;
    }
  }
}

void _mergeSort(MergeSortStack sortStack) {
  if (sortStack.length <= 1) {
    return;
  }

  /*
  printf("START\n");
  for (int i = 0; i < sortStack.length; i++) {
    printf("%d\n", ((int*) sortStack.parent)[i]);
  }
  */

  MergeSortStack leftSortStack = sortStack;
  leftSortStack.length = sortStack.length / 2;
  leftSortStack.parent = sortStack.arena;
  leftSortStack.arena =
    sortStack.arena + (leftSortStack.length * sortStack.objectSize);
  memcpy(leftSortStack.parent,
         sortStack.parent,
         leftSortStack.length * sortStack.objectSize);
  _mergeSort(leftSortStack);

  MergeSortStack rightSortStack = sortStack;
  rightSortStack.length = sortStack.length - leftSortStack.length;
  rightSortStack.parent =
    sortStack.arena + (leftSortStack.length * sortStack.objectSize);
  rightSortStack.arena =
    sortStack.arena + (sortStack.length * sortStack.objectSize);
  memcpy(rightSortStack.parent,
         sortStack.parent + (leftSortStack.length * sortStack.objectSize),
         rightSortStack.length * sortStack.objectSize);
  _mergeSort(rightSortStack);

  _merge(sortStack, leftSortStack, rightSortStack);
}

void* mergeSort(void* array, size_t length, size_t objectSize, SortFn sortFn) {
  void* result = malloc(length * objectSize);
  memcpy(result, array, length * objectSize);

  MergeSortStack sortStack = {
    .arena = malloc(2 * length * objectSize),
    .length = length,
    .parent = result,
    .objectSize = objectSize,
    .sortFn = sortFn
  };

  _mergeSort(sortStack);
  free(sortStack.arena);

  return result;
}

int cmp(int* i, int* j) {
  if (*i < *j) {
    return -1;
  } else if (*i == *j) {
    return 0;
  } else {
    return 1;
  }
}

static int length = (1 << 23);
int main() {
  int* ip = malloc(length * sizeof(int));

  for (int i = 0; i < length; i++) {
    ip[i] = length - i;
  }

  int* result = mergeSort(ip, length, sizeof(int), cmp);

  /*
  for (int i = 0; i < 8; i++) {
    printf("%d\n", result[i]);
  }
  */
}
