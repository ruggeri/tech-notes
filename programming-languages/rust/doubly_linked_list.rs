use std::collections::VecDeque;

#[derive(Copy, Clone)]
pub struct NodeId {
  idx: usize,
  generation: usize,
}

struct Node<T> {
  value: Option<T>,
  generation: usize,
  prev_id: Option<NodeId>,
  next_id: Option<NodeId>,
}

// pub struct MutCursor<T> {
//   list: &mut DoublyLinkedList<T>,
//   node_id: NodeId,
// }

pub struct DoublyLinkedList<T> {
  nodes: Vec<Node<T>>,
  free_ids: VecDeque<NodeId>,
  front_id: Option<NodeId>,
  back_id: Option<NodeId>,
}

impl<T> DoublyLinkedList<T> {
  pub fn new() -> Self {
    DoublyLinkedList{
      nodes: Vec::new(),
      free_ids: VecDeque::new(),
      front_id: None,
      back_id: None,
    }
  }

  fn allocate_node(&mut self, value: T) -> NodeId {
    match self.free_ids.pop_front() {
      None => {
        self.nodes.push(Node{
          value: Some(value),
          generation: 0,
          prev_id: None,
          next_id: None,
        });

        NodeId{
          idx: self.nodes.len() - 1,
          generation: 0,
        }
      },

      Some(node_id) => {
        let node = &mut self.nodes[node_id.idx];
        node.value = Some(value);
        node.generation += 1;

        NodeId{
          idx: node_id.idx,
          generation: node.generation,
        }
      }
    }
  }

  fn try_deallocate_node(&mut self, node_id: NodeId) -> Option<(&mut Node<T>, T)> {
    let node = &mut self.nodes[node_id.idx];

    if node.generation != node_id.generation {
      // this isn't even the correct generation!
      return None
    }

    match node.value.take() {
      None => {
        // must have been previously freed
        None
      },
      Some(value) => {
        // we are actually freeing a value!
        self.free_ids.push_back(node_id);
        Some((node, value))
      },
    }
  }

  fn deallocate_node(&mut self, node_id: NodeId) -> (&mut Node<T>, T) {
    self.try_deallocate_node(node_id).expect("expected node at node_id but found none")
  }

  fn try_get_node(&self, node_id: NodeId) -> Option<&Node<T>> {
    let node = &self.nodes[node_id.idx];

    if node.generation == node_id.generation {
      Some(node)
    } else {
      None
    }
  }

  fn get_node(&self, node_id: NodeId) -> &Node<T> {
    self.try_get_node(node_id).expect("expected node at node_id but found none")
  }

  fn try_get_node_mut(&mut self, node_id: NodeId) -> Option<&mut Node<T>> {
    let node = &mut self.nodes[node_id.idx];

    if node.generation == node_id.generation {
      Some(node)
    } else {
      None
    }
  }

  fn get_node_mut(&mut self, node_id: NodeId) -> &mut Node<T> {
    self.try_get_node_mut(node_id).expect("expected node at node_id but found none")
  }

  pub fn push_front(&mut self, obj: T) {
    let node_id = self.allocate_node(obj);

    match self.front_id {
      None => {
        self.front_id = Some(node_id);
        self.back_id = Some(node_id);
      },
      Some(old_front_id) => {
        self.front_id = Some(node_id);
        self.get_node_mut(node_id).next_id = Some(old_front_id);
        self.get_node_mut(old_front_id).prev_id = Some(node_id);
      }
    }
  }

  pub fn pop_front(&mut self) -> Option<T> {
    let front_id = self.front_id?;
    let (node, old_value) = self.deallocate_node(front_id);

    self.front_id = node.next_id;
    match self.front_id {
      None => {
        self.back_id = None;
      },
      Some(front_id) => {
        self.get_node_mut(front_id).prev_id = None;
      }
    }

    Some(old_value)
  }

  pub fn push_back(&mut self, obj: T) {
    let node_id = self.allocate_node(obj);

    match self.back_id {
      None => {
        self.front_id = Some(node_id);
        self.back_id = Some(node_id);
      },
      Some(old_back_id) => {
        self.back_id = Some(node_id);
        self.get_node_mut(node_id).prev_id = Some(old_back_id);
        self.get_node_mut(old_back_id).next_id = Some(node_id);
      }
    }
  }

  pub fn pop_back(&mut self) -> Option<T> {
    let back_id = self.back_id?;
    let (node, old_value) = self.deallocate_node(back_id);

    self.back_id = node.prev_id;
    match self.back_id {
      None => {
        self.front_id = None;
      },
      Some(back_id) => {
        self.get_node_mut(back_id).next_id = None;
      }
    }

    Some(old_value)
  }

  pub fn len(&self) -> usize {
    self.nodes.len() - self.free_ids.len()
  }

  // pub fn mut_cursor_at(&mut self, nodeId: NodeId) -> MutCursor<T> {
  //   MutCursor{
  //     list: self,
  //     nodeId,
  //   }
  // }
}

// impl<T> MutCursor<T> {
//   pub fn value() -> &mut T {
//     self.list.
//   }
// }

pub fn main() {
  println!("Hello world!");
}
