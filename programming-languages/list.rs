// http://cglab.ca/~abeinges/blah/too-many-lists/book/

// Even though the items are heap allocated now, they are owned by a
// single List. Prepending still requires copying the entire list, it
// just doesn't invalidate the old one, which can live separately.
// Maybe it would be nice to refcount the next?

#[derive(Clone)]
enum  List<T: Clone> {
  Cons { val: T, next: Box<List<T>> },
  Nil
}

impl <T: Clone> List<T> {
  fn new(val: T, next: &List<T>) -> List<T> {
    List::Cons {
      val: val,
      next: Box::new((*next).clone())
    }
  }

//  fn prepend(self, val: T) -> List<T> {
//    List::Cons {
//      val: val,
//      next: Box::new(self)
//    }
//  }
}

fn main() {
  let v = List::new(32, &List::Nil);
  let v2 = List::new(64, &v);
  let v3 = List::new(128, &v2);

  if let List::Cons { val: my_val, .. } = v {
    println!("{}", my_val);
  }

  let mut x = v3;
  loop {
    match x {
      List::Cons { val, next } => {
        println!("Val: {}", val);
        x = *next;
      },
      List::Nil => { break }
    }
  }
}
