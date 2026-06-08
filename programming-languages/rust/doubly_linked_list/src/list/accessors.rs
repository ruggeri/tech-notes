use super::DoublyLinkedList;
use crate::node::{Node, NodeId};

impl<T> DoublyLinkedList<T> {
    // accessors by id
    pub fn try_get_val(&self, id: NodeId) -> Option<&T> {
        let node = self.try_get_node(id)?;
        node.value.as_ref()
    }

    pub fn get_val(&self, id: NodeId) -> &T {
        // unwrap because `get_node` will only return a live node, which
        // must have a value.
        self.get_node(id).value.as_ref().unwrap()
    }

    pub fn try_get_mut_val(&mut self, id: NodeId) -> Option<&mut T> {
        let node = self.try_get_node_mut(id)?;
        node.value.as_mut()
    }

    pub fn get_mut_val(&mut self, id: NodeId) -> &mut T {
        // unwrap because `get_node` will only return a live node, which
        // must have a value.
        self.get_node_mut(id).value.as_mut().unwrap()
    }

    // front/back accessors

    pub fn get_front_id(&self) -> Option<NodeId> {
        self.front_id
    }

    pub fn try_get_front_val(&self) -> Option<&T> {
        match self.front_id {
            None => None,
            Some(front_id) => Some(self.get_val(front_id)),
        }
    }

    pub fn get_front_val(&self) -> &T {
        self.try_get_front_val()
            .expect("can't get front of empty list")
    }

    pub fn get_back_id(&self) -> Option<NodeId> {
        self.back_id
    }

    pub fn try_get_back_val(&self) -> Option<&T> {
        match self.back_id {
            None => None,
            Some(back_id) => Some(self.get_val(back_id)),
        }
    }

    pub fn get_back_val(&self) -> &T {
        self.try_get_back_val()
            .expect("can't get back of empty list")
    }

    // these getters only get *live* nodes from the arena.

    pub(crate) fn try_get_node(&self, id: NodeId) -> Option<&Node<T>> {
        if self.arena.has_live_node_for_id(id) {
            Some(self.arena.get_node(id))
        } else {
            None
        }
    }

    pub(crate) fn get_node(&self, id: NodeId) -> &Node<T> {
        match self.try_get_node(id) {
            None => panic!("no live node for id:\n{:#?}", id),
            Some(node) => node,
        }
    }

    pub(crate) fn try_get_node_mut(&mut self, id: NodeId) -> Option<&mut Node<T>> {
        if self.arena.has_live_node_for_id(id) {
            Some(self.arena.get_node_mut(id))
        } else {
            None
        }
    }

    pub(crate) fn get_node_mut(&mut self, id: NodeId) -> &mut Node<T> {
        match self.try_get_node_mut(id) {
            None => panic!("no live node for id:\n{:#?}", id),
            Some(node) => node,
        }
    }
}
