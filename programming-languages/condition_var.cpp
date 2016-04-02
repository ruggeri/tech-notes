#include <cstdlib>
#include <iostream>
#include <mutex>
#include <thread>
#include <unistd.h>
#include <vector>

template<typename T>
struct MyBuffer {
  T* buffer;
  int capacity;

  int head_idx;
  int length;

  std::mutex mutex;
  std::condition_variable not_full;
  std::condition_variable not_empty;

  MyBuffer(int capacity) : capacity(capacity), head_idx(0), length(0) {
    buffer = new T[capacity];
  }

  ~MyBuffer() {
    delete[] buffer;
  }

  void push(T item) {
    std::unique_lock<std::mutex> l(mutex);

    not_full.wait(l, [this]() { return length != capacity; });

    buffer[(head_idx + length) % capacity] = item;
    length++;

    not_empty.notify_one();
  }

  T shift() {
    // unique_lock is just a beefed up lock_guard. It can be locked
    // and unlocked. It still default unlocks when destructed. I think
    // the intent is that you use lock_guard to communicate intent
    // that you won't use this extra functionality?
    std::unique_lock<std::mutex> l(mutex);

    // Tests the condition. If it doesn't hold, unlocks and starts
    // waiting on the mutex. When notified (even spuriously) it
    // reacquires the lock and tests the condition. If the condition
    // does hold, we proceed.
    not_empty.wait(l, [this]() { return length != 0; });

    T t = buffer[head_idx];
    head_idx = (head_idx + 1) % capacity;
    length--;

    not_full.notify_one();

    // unique_lock is released when destructed.
    return t;
  }
};

const int MICROSECONDS_IN_SECOND = 1000000;

int main() {
  MyBuffer<int> buf(10);

  std::vector<std::thread> threads;

  threads.push_back(std::thread([&buf] {
        int i = 0;
        while (true) {
          buf.push(i);
          i++;

          usleep(MICROSECONDS_IN_SECOND * (float) rand() / RAND_MAX);
        }
      })
    );

  threads.push_back(std::thread([&buf] {
        while (true) {
          int i = buf.shift();
          std::cout << i << "\n";

          usleep(MICROSECONDS_IN_SECOND * (float) rand() / RAND_MAX);
        }
      })
    );

  for (auto& thread : threads) {
    thread.join();
  }

  return 0;
}
