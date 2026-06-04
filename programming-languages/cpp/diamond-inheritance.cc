#include <iostream>

struct GrandParent {
  std::string msg;

  GrandParent(std::string msg) : msg(msg) {}
};

struct B1 : GrandParent {
  B1() : GrandParent("METHOD1") {}

  virtual void f() {
    std::cout << this->msg << "\n";
  }

  virtual ~B1() {}
};

struct B2 : GrandParent {
  B2() : GrandParent("METHOD2") {}

  virtual void f() {
    std::cout << this->msg << "\n";
  }

  virtual ~B2() {}
};

struct Child1 : B1, B2 {
  virtual ~Child1() {}
};

struct Child2 : B1, B2 {
  // Resolve ambiguity through override.
  virtual void f() {
    return B1::f();
  }

  virtual ~Child2() {}
};

struct CollatzBase {
  virtual int collatzOdd(int num, int steps) = 0;
  virtual int collatzEven(int num, int steps) = 0;

  virtual ~CollatzBase() {};
};

struct CollatzOdd : public virtual CollatzBase {
  int collatzOdd(int num, int steps) {
    return (num == 1) ?
      steps : this->collatzEven((3 * num) + 1, steps + 1);
  }
};

struct CollatzEven : public virtual CollatzBase {
  int collatzEven(int num, int steps) {
    num = num / 2;
    steps += 1;

    return (num % 2 == 0) ?
      this->collatzEven(num, steps) : this->collatzOdd(num, steps);
  }
};

struct Collatz : public CollatzOdd, public CollatzEven {
  int collatz(int num) {
    return (num % 2 == 0) ?
      this->collatzEven(num, 0) : this->collatzOdd(num, 0);
  }
};

int main() {
  Child1* c1 = new Child1();

  c1->B1::f();
  c1->B2::f();

  Child2* c2 = new Child2();
  c2->f();

  // cast through B1!
  GrandParent* gp = ((B1*) c1);
  gp->msg = "method1";

  gp = ((B2*) c1);
  gp->msg = "method2";

  c1->B1::f();
  c1->B2::f();

  Collatz* cz = new Collatz();
  std::cout << cz->collatz(27) << "\n";

  return 0;
}
