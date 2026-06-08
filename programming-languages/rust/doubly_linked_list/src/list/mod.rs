mod accessors;
mod iteration;
mod mutations;

use crate::arena::Arena;
use crate::node::NodeId;

pub struct DoublyLinkedList<T> {
    arena: Arena<T>,
    front_id: Option<NodeId>,
    back_id: Option<NodeId>,
}

impl<T> DoublyLinkedList<T> {
    pub fn new() -> Self {
        DoublyLinkedList {
            arena: Arena::new(),
            front_id: None,
            back_id: None,
        }
    }

    pub fn len(&self) -> usize {
        self.arena.num_live_nodes()
    }
}
