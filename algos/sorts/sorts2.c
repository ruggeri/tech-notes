#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

// This approach uses n extra memory cells. It also only copies once
// per merged element, so makes nlog(n) copies.

typedef int (*SortFn)(void* obj1, void* obj2);

void* merge(
  void* target,
  void* sortedLeft,
  size_t leftLength,
  void* sortedRight,
  size_t rightLength,
  size_t elSize,
  SortFn sortFn
) {
  size_t leftIdx = 0;
  size_t rightIdx = 0;

  while ((leftIdx + rightIdx) < (leftLength + rightLength)) {
    void* leftEl = sortedLeft + (leftIdx * elSize);
    void* rightEl = sortedRight + (rightIdx * elSize);
    void* targetEl = target + ((leftIdx + rightIdx) * elSize);

    if (leftIdx == leftLength) {
      memcpy(targetEl, rightEl, elSize);
      rightIdx += 1;
    } else if (rightIdx == rightLength) {
      memcpy(targetEl, leftEl, elSize);
      leftIdx += 1;
    } else if (sortFn(leftEl, rightEl) <= 0) {
      memcpy(targetEl, leftEl, elSize);
      leftIdx += 1;
    } else {
      memcpy(targetEl, rightEl, elSize);
      rightIdx += 1;
    }
  }

  return target;
}

// A merge sort. This is kind of a mindfuck. Note that the only
// copying done is in the merge function.
void* _mergeSort(
  void* tmp1,
  void* tmp2,
  size_t length,
  size_t elSize,
  SortFn sortFn
) {
  if (length <= 1) {
    return tmp1;
  }

  size_t leftLength = length / 2;
  size_t rightLength = length - leftLength;

  void* sortedLeft = _mergeSort(
    tmp1,
    tmp2,
    leftLength,
    elSize,
    sortFn
  );
  void* sortedRight = _mergeSort(
    tmp1 + (leftLength * elSize),
    tmp2 + (leftLength * elSize),
    rightLength,
    elSize,
    sortFn
  );

  // If the left and right are both placed in tmp1, then we merge into
  // tmp2. Vice versa, if both are placed in tmp1, we merge into
  // tmp2.
  //
  // If the number of elements is not a power of two, then the last
  // level of the recursion tree is not full. This will result in the
  // left and right sometimes being placed in different sides, since
  // the depth of the recursion on left and right may be different
  // sometimes.
  //
  // I solve this by just copying the left over to the right's
  // side. However, this can be inefficient. I think the optimal way
  // is to count how many leaf nodes are in the final level; if this
  // is greater than the number in the penultimate level, do an extra
  // copy for the penultimate element base cases. If are fewer nodes
  // in the final level, copy these extra in the base case.
  //
  // That required more trickiness so I ignored it.

  if (sortedRight == (tmp1 + (leftLength * elSize))) {
    if (sortedLeft != tmp1) {
      memcpy(tmp1, tmp2, leftLength * elSize);
      sortedLeft = tmp1;
    }

    return merge(
      tmp2,
      sortedLeft,
      leftLength,
      sortedRight,
      rightLength,
      elSize,
      sortFn
    );
  } else {
    if (sortedLeft != tmp2) {
      memcpy(tmp2, tmp1, leftLength * elSize);
      sortedLeft = tmp2;
    }

    return merge(
      tmp1,
      sortedLeft,
      leftLength,
      sortedRight,
      rightLength,
      elSize,
      sortFn
    );
  }
}

void* mergeSort(
  void* arr,
  size_t length,
  size_t elSize,
  SortFn sortFn,
  bool inPlace
) {
  void* tmp1;
  if (inPlace) {
    tmp1 = arr;
  } else {
    tmp1 = malloc(length * elSize);
    memcpy(tmp1, arr, length * elSize);
  }
  void* tmp2 = malloc(length * elSize);

  void* result = _mergeSort(
    tmp1,
    tmp2,
    length,
    elSize,
    sortFn
  );

  if (!inPlace) {
    if (result == tmp1) {
      free(tmp2);
    } else {
      free(tmp1);
    }

    return result;
  } else {
    if (result != tmp1) {
      memcpy(tmp1, result, length * elSize);
    }

    free(tmp2);
    return tmp1;
  }
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

static int length = 31;
int main() {
  int* ip = malloc(length * sizeof(int));
  for (int i = 0; i < length; i++) {
    ip[i] = length - i;
  }

  int* result = mergeSort(ip, length, sizeof(int), cmp, true);

  printf("Done!\n");
  for (int i = 0; i < length; i++) {
    printf("%d\n", result[i]);
  }
}
