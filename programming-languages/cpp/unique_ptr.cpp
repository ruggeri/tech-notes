#include <iostream>
#include <memory>
#include <vector>

int main() {
  std::unique_ptr<std::vector<int>> p(new std::vector<int>({ 1, 3, 5 }));

  for (auto i : *p) {
    std::cout << i << '\n';
  }

  // std::move can be used to generate an rvalue reference to a named
  // variable, but it's now on you not to reuse the old value!!
  //
  // The unique_ptr code is the one that will reset `p`. I think that
  // `std::move` doesn't actually change `p` at all; it just casts to
  // an rvalue ref so that this can be used in the move constructor,
  // which actually makes the changes.
  //
  // We can remove the assignment to see if the segfault still
  // happens! It doesn't!
  std::unique_ptr<std::vector<int>> p2 = std::move(p);

  // Will normally segfault.
  for (auto i : *p) {
    std::cout << i << '\n';
  }

  return 0;
}
