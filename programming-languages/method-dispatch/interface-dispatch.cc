#include <iostream>

struct B1 {
  virtual void method1() {
    std::cout << "METHOD1\n";
  }

  virtual ~B1() {}
};

struct B2 {
  virtual void method2() {
    std::cout << "METHOD2\n";
  }

  virtual ~B2() {}
};

struct Child : B1, B2 {
  virtual ~Child() {}
};

int main() {
  Child* c = new Child();

  c->method1();
  c->method2();

  std::cout << c << "\n";
  std::cout << ((B1*) c) << "\n";
  // Note the fixup!
  std::cout << ((B2*) c) << "\n";

  B2* b2 = c;
  b2->method2();
  std::cout << b2 << "\n";
  B1* b1 = dynamic_cast<B1*>(b2);
  b1->method1();
  // Note the unfixup!
  std::cout << b1 << "\n";

  b2 = c;
  b1 = reinterpret_cast<B1*>(b2);
  // Method two gets called!
  b1->method1();

  b2 = static_cast<B2*>(c);
  c = static_cast<Child*>(b2);
  c->method1();

  return 0;
}
