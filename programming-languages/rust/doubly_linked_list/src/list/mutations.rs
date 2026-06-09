use super::DoublyLinkedList;
use crate::node::NodeId;

pub struct RemoveResult<T> {
    pub val: T,
    pub prev_id: Option<NodeId>,
    pub next_id: Option<NodeId>,
}

impl<T> DoublyLinkedList<T> {
    // front/back operations
    pub fn push_front(&mut self, value: T) -> NodeId {
        let Some(old_front_id) = self.front_id else {
            let id = self.arena.allocate_node(value);
            self.front_id = Some(id);
            self.back_id = Some(id);

            return id;
        };

        self.insert_before(old_front_id, value)
    }

    pub fn pop_front(&mut self) -> Option<T> {
        let front_id = self.front_id?;
        Some(self.remove(front_id).val)
    }

    pub fn push_back(&mut self, value: T) -> NodeId {
        let Some(old_back_id) = self.back_id else {
            let id = self.arena.allocate_node(value);
            self.front_id = Some(id);
            self.back_id = Some(id);

            return id;
        };

        self.insert_after(old_back_id, value)
    }

    pub fn pop_back(&mut self) -> Option<T> {
        let back_id = self.back_id?;
        Some(self.remove(back_id).val)
    }

    // insertion operations
    pub fn try_insert_before(&mut self, target_id: NodeId, value: T) -> Result<NodeId, T> {
        if !self.arena.has_live_node_for_id(target_id) {
            // Occurs primarily if target_id points to a node that
            // has been removed.
            return Err(value);
        }

        let node_id = self.arena.allocate_node(value);
        self.place_detached_node_before(node_id, target_id);
        Ok(node_id)
    }

    pub fn insert_before(&mut self, target_id: NodeId, value: T) -> NodeId {
        match self.try_insert_before(target_id, value) {
            Ok(id) => id,
            Err(_) => panic!("could not insert before id:\n{:#?}", target_id),
        }
    }

    pub fn try_insert_after(&mut self, target_id: NodeId, value: T) -> Result<NodeId, T> {
        if !self.arena.has_live_node_for_id(target_id) {
            // Occurs primarily if curr_node_id points to a node that
            // has been removed.
            return Err(value);
        }

        let node_id = self.arena.allocate_node(value);
        self.place_detached_node_after(node_id, target_id);
        Ok(node_id)
    }

    pub fn insert_after(&mut self, target_id: NodeId, value: T) -> NodeId {
        match self.try_insert_after(target_id, value) {
            Ok(id) => id,
            Err(_) => panic!("could not insert after id:\n{:#?}", target_id),
        }
    }

    // node removal operations
    pub fn try_remove(&mut self, target_id: NodeId) -> Option<RemoveResult<T>> {
        if !self.arena.has_live_node_for_id(target_id) {
            return None;
        }

        let (prev_id, next_id) = self.detach(target_id);
        let val = self.arena.deallocate_node(target_id);

        Some(RemoveResult {
            val,
            prev_id,
            next_id,
        })
    }

    pub fn remove(&mut self, target_id: NodeId) -> RemoveResult<T> {
        match self.try_remove(target_id) {
            None => panic!("could not remove id:\n{:#?}", target_id),
            Some(result) => result,
        }
    }

    // value set operations
    pub fn try_set_node_value(&mut self, target_id: NodeId, value: T) -> Result<T, T> {
        if !self.arena.has_live_node_for_id(target_id) {
            return Err(value);
        }

        let old_value = self.get_node_mut(target_id).value.replace(value);
        Ok(old_value.unwrap())
    }

    pub fn set_node_value(&mut self, target_id: NodeId, value: T) -> T {
        match self.try_set_node_value(target_id, value) {
            Ok(old_value) => old_value,
            Err(_) => panic!("could not set value for id:\n{:#?}", target_id),
        }
    }

    pub fn try_move_node_before(&mut self, id: NodeId, target_id: NodeId) -> Result<(), NodeId> {
        if id == target_id {
            panic!("cannot move a node before itself!\n{:#?}", id);
        }

        if !self.arena.has_live_node_for_id(id) {
            return Err(id);
        }
        if !self.arena.has_live_node_for_id(target_id) {
            return Err(target_id);
        }

        self.detach(id);
        self.place_detached_node_before(id, target_id);
        Ok(())
    }

    pub fn move_node_before(&mut self, id: NodeId, target_id: NodeId) {
        match self.try_move_node_before(id, target_id) {
            Err(node_id) => {
                panic!("node was not alive for move:\n{:#?}", node_id);
            }
            Ok(()) => {}
        }
    }

    pub fn try_move_node_after(&mut self, id: NodeId, target_id: NodeId) -> Result<(), NodeId> {
        if id == target_id {
            panic!("cannot move a node after of itself!\n{:#?}", id);
        }

        if !self.arena.has_live_node_for_id(id) {
            return Err(id);
        }
        if !self.arena.has_live_node_for_id(target_id) {
            return Err(target_id);
        }

        self.detach(id);
        self.place_detached_node_after(id, target_id);
        Ok(())
    }

    pub fn move_node_after(&mut self, id: NodeId, target_id: NodeId) {
        match self.try_move_node_after(id, target_id) {
            Err(node_id) => {
                panic!("node was not alive for move:\n{:#?}", node_id);
            }
            Ok(()) => {}
        }
    }

    // internal methods

    // intended for debug only...
    pub(crate) fn is_detached(&self, id: NodeId) -> bool {
        debug_assert!(self.arena.has_node_for_id(id));

        let node = self.arena.get_node(id);
        if node.prev_id.is_some() || node.next_id.is_some() {
            return false;
        }

        if self.front_id == Some(id) {
            return false;
        }

        true
    }

    /// Removes a node from the linked list, but does *not* ask the
    /// arena to deallocate it. Node ought not already be detached.
    pub(crate) fn detach(&mut self, id: NodeId) -> (Option<NodeId>, Option<NodeId>) {
        debug_assert!(self.arena.has_live_node_for_id(id));
        debug_assert!(!self.is_detached(id));

        let (prev_id, next_id) = {
            let node = self.get_node_mut(id);
            (node.prev_id.take(), node.next_id.take())
        };

        if let Some(prev_id) = prev_id {
            let prev_node = self.get_node_mut(prev_id);
            debug_assert!(prev_node.next_id == Some(id));
            prev_node.next_id = next_id;
        } else {
            debug_assert!(self.front_id == Some(id));
            self.front_id = next_id;
        }

        if let Some(next_id) = next_id {
            let next_node = self.get_node_mut(next_id);
            debug_assert!(next_node.prev_id == Some(id));
            next_node.prev_id = prev_id;
        } else {
            debug_assert!(self.back_id == Some(id));
            self.back_id = prev_id;
        };

        (prev_id, next_id)
    }

    /// `id` must be live but detached. `target_id` must be live and
    /// attached.
    pub(crate) fn place_detached_node_before(&mut self, id: NodeId, target_id: NodeId) {
        debug_assert!(id != target_id);
        debug_assert!(self.arena.has_live_node_for_id(id));
        debug_assert!(self.is_detached(id));
        debug_assert!(self.arena.has_live_node_for_id(target_id));
        debug_assert!(!self.is_detached(target_id));

        if let Some(prev_id) = self.get_node(target_id).prev_id {
            let prev_node = self.get_node_mut(prev_id);
            debug_assert!(prev_node.next_id == Some(target_id));

            prev_node.next_id = Some(id);
            self.get_node_mut(id).prev_id = Some(prev_id);
        } else {
            debug_assert!(self.front_id == Some(target_id));
            self.front_id = Some(id);
        }

        self.get_node_mut(id).next_id = Some(target_id);
        self.get_node_mut(target_id).prev_id = Some(id);
    }

    /// `id` must be live but detached. `target_id` must be live and
    /// attached.
    pub(crate) fn place_detached_node_after(&mut self, id: NodeId, target_id: NodeId) {
        debug_assert!(id != target_id);
        debug_assert!(self.arena.has_live_node_for_id(id));
        debug_assert!(self.is_detached(id));
        debug_assert!(self.arena.has_live_node_for_id(target_id));
        debug_assert!(!self.is_detached(target_id));

        if let Some(next_id) = self.get_node(target_id).next_id {
            let next_node = self.get_node_mut(next_id);
            debug_assert!(next_node.prev_id == Some(target_id));

            next_node.prev_id = Some(id);
            self.get_node_mut(id).next_id = Some(next_id);
        } else {
            debug_assert!(self.back_id == Some(target_id));
            self.back_id = Some(id);
        }

        self.get_node_mut(id).prev_id = Some(target_id);
        self.get_node_mut(target_id).next_id = Some(id);
    }
}
