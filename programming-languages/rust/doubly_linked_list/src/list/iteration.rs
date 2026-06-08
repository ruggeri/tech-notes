use crate::DoublyLinkedList;
use crate::node::{Node, NodeId};

impl<'a, T> DoublyLinkedList<T> {
    pub fn try_next_id(&self, id: NodeId) -> Result<Option<NodeId>, ()> {
        let Some(node) = self.try_get_node(id) else {
            return Err(());
        };

        Ok(node.next_id)
    }

    pub fn next_id(&self, id: NodeId) -> Option<NodeId> {
        self.try_next_id(id).expect("node id is not present")
    }

    pub fn try_prev_id(&self, id: NodeId) -> Result<Option<NodeId>, ()> {
        let Some(node) = self.try_get_node(id) else {
            return Err(());
        };

        Ok(node.prev_id)
    }

    pub fn prev_id(&self, id: NodeId) -> Option<NodeId> {
        self.try_prev_id(id).expect("node id is not present")
    }

    pub fn cursor_at(&'a self, id: NodeId) -> Cursor<'a, T> {
        if !self.arena.has_live_node_for_id(id) {
            panic!("trying to get cursor at id which is not live");
        }

        Cursor::new(self, CursorPos::At(id))
    }

    pub fn cursor_at_front(&'a self) -> Cursor<'a, T> {
        Cursor::new(self, CursorPos::BeforeFront)
    }

    pub fn cursor_at_back(&'a self) -> Cursor<'a, T> {
        Cursor::new(self, CursorPos::AfterBack)
    }

    pub fn id_iter_at_front(&'a self) -> IdIter<'a, T> {
        IdIter(self.cursor_at_front())
    }

    pub fn val_iter_at_front(&'a self) -> ValIter<'a, T> {
        ValIter(self.cursor_at_front())
    }
}

enum CursorPos {
    BeforeFront,
    At(NodeId),
    AfterBack,
}

pub struct Cursor<'a, T> {
    list: &'a DoublyLinkedList<T>,
    pos: CursorPos,
}

impl<'a, T> Cursor<'a, T> {
    fn new(list: &'a DoublyLinkedList<T>, pos: CursorPos) -> Cursor<'a, T> {
        Cursor { list, pos }
    }

    pub(crate) fn current_node(&self) -> Option<&'a Node<T>> {
        match self.pos {
            CursorPos::BeforeFront => None,
            CursorPos::At(id) => Some(self.list.get_node(id)),
            CursorPos::AfterBack => None,
        }
    }

    pub fn current_id(&self) -> Option<NodeId> {
        let node = self.current_node()?;
        Some(node.id)
    }

    pub fn current_val(&self) -> Option<&'a T> {
        let node = self.current_node()?;
        let val = node.value.as_ref().unwrap();
        Some(val)
    }

    pub fn move_next(&mut self) -> Option<(NodeId, &'a T)> {
        let next_id = match self.pos {
            CursorPos::BeforeFront => self.list.front_id,
            CursorPos::At(curr_id) => self.list.next_id(curr_id),
            CursorPos::AfterBack => return None,
        };

        let Some(next_id) = next_id else {
            self.pos = CursorPos::AfterBack;
            return None;
        };

        self.pos = CursorPos::At(next_id);

        Some((next_id, self.list.get_val(next_id)))
    }

    pub fn move_prev(&mut self) -> Option<(NodeId, &T)> {
        let prev_id = match self.pos {
            CursorPos::BeforeFront => return None,
            CursorPos::At(curr_id) => self.list.prev_id(curr_id),
            CursorPos::AfterBack => self.list.back_id,
        };

        let Some(prev_id) = prev_id else {
            self.pos = CursorPos::BeforeFront;
            return None;
        };

        self.pos = CursorPos::At(prev_id);

        Some((prev_id, self.list.get_val(prev_id)))
    }

    // pub fn is_null(&self) -> bool;
}

pub struct IdIter<'a, T>(Cursor<'a, T>);

impl<'a, T> Iterator for IdIter<'a, T> {
    type Item = NodeId;

    fn next(&mut self) -> Option<Self::Item> {
        let (id, _) = self.0.move_next()?;
        Some(id)
    }
}

pub struct ValIter<'a, T>(Cursor<'a, T>);

impl<'a, T> Iterator for ValIter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        let (_, val) = self.0.move_next()?;
        Some(val)
    }
}
