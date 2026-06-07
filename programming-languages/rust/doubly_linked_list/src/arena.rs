use crate::node::{Node, NodeId};
use std::collections::VecDeque;

/// Holds `Node<T>` for potential reuse.
pub(crate) struct Arena<T> {
    nodes: Vec<Node<T>>,
    free_ids: VecDeque<NodeId>,
}

impl<T> Arena<T> {
    pub(crate) fn new() -> Self {
        Arena {
            nodes: Vec::new(),
            free_ids: VecDeque::new(),
        }
    }

    /// Allocates a `Node<T>` (reuses one or creates a new one) and
    /// stores `value` there.
    pub(crate) fn allocate_node(&mut self, value: T) -> NodeId {
        match self.free_ids.pop_front() {
            None => {
                let id = NodeId {
                    idx: self.nodes.len(),
                    generation: 0,
                };

                self.nodes.push(Node {
                    value: Some(value),
                    id,
                    prev_id: None,
                    next_id: None,
                });

                id
            }

            Some(id) => {
                self.nodes[id.idx] = Node {
                    value: Some(value),
                    id,
                    prev_id: None,
                    next_id: None,
                };

                id
            }
        }
    }

    /// Marks the node as free and returns the stored value.
    pub(crate) fn deallocate_node(&mut self, id: NodeId) -> T {
        debug_assert!(self.has_live_node_for_id(id));

        let (free_id, old_value) = {
            let node: &mut Node<T> = self.get_node_mut(id);
            node.id.generation += 1;
            (node.id, node.value.take().unwrap())
        };

        self.free_ids.push_back(free_id);

        old_value
    }

    pub(crate) fn get_node(&self, id: NodeId) -> &Node<T> {
        if !self.has_node_for_id(id) {
            panic!("no node for id\n:{:#?}", id);
        }

        &self.nodes[id.idx]
    }

    pub(crate) fn get_node_mut(&mut self, id: NodeId) -> &mut Node<T> {
        if !self.has_node_for_id(id) {
            panic!("no node for id\n:{:#?}", id);
        }

        &mut self.nodes[id.idx]
    }

    pub(crate) fn has_node_for_id(&self, id: NodeId) -> bool {
        if id.idx >= self.nodes.len() {
            return false;
        }

        let node = &self.nodes[id.idx];
        node.id.generation == id.generation
    }

    pub(crate) fn has_live_node_for_id(&self, id: NodeId) -> bool {
        self.has_node_for_id(id) && self.get_node(id).value.is_some()
    }

    pub(crate) fn num_live_nodes(&self) -> usize {
        self.nodes.len() - self.free_ids.len()
    }
}
