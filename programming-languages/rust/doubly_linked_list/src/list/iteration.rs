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

    pub fn mut_cursor_at_front(&'a mut self) -> MutCursor<'a, T> {
        MutCursor::new(self, CursorPos::BeforeFront)
    }

    pub fn cursor_at_back(&'a self) -> Cursor<'a, T> {
        Cursor::new(self, CursorPos::AfterBack)
    }

    pub fn mut_cursor_at_back(&'a mut self) -> MutCursor<'a, T> {
        MutCursor::new(self, CursorPos::AfterBack)
    }

    pub fn id_iter_at_front(&'a self) -> IdIter<'a, T> {
        IdIter(self.cursor_at_front())
    }

    pub fn val_iter_at_front(&'a self) -> ValIter<'a, T> {
        ValIter(self.cursor_at_front())
    }
}

#[derive(Clone, Copy)]
enum CursorPos {
    BeforeFront,
    At(NodeId),
    AfterBack,
}

impl CursorPos {
    pub(crate) fn current_id(self) -> Option<NodeId> {
        match self {
            CursorPos::BeforeFront => None,
            CursorPos::At(id) => Some(id),
            CursorPos::AfterBack => None,
        }
    }

    pub(crate) fn current_node<'a, T>(self, list: &'a DoublyLinkedList<T>) -> Option<&'a Node<T>> {
        Some(list.get_node(self.current_id()?))
    }

    pub(crate) fn current_val<'a, T>(self, list: &'a DoublyLinkedList<T>) -> Option<&'a T> {
        self.current_node(list)?.value.as_ref()
    }

    pub fn next<'a, T>(
        self,
        list: &'a DoublyLinkedList<T>,
    ) -> (CursorPos, Option<(NodeId, &'a T)>) {
        let next_id: Option<NodeId> = match self {
            CursorPos::BeforeFront => list.front_id,
            CursorPos::At(curr_id) => list.next_id(curr_id),
            CursorPos::AfterBack => return (self, None),
        };

        let Some(next_id) = next_id else {
            return (CursorPos::AfterBack, None);
        };

        (
            CursorPos::At(next_id),
            Some((next_id, list.get_val(next_id))),
        )
    }

    pub fn prev<'a, T>(
        self,
        list: &'a DoublyLinkedList<T>,
    ) -> (CursorPos, Option<(NodeId, &'a T)>) {
        let prev_id: Option<NodeId> = match self {
            CursorPos::BeforeFront => return (self, None),
            CursorPos::At(curr_id) => list.prev_id(curr_id),
            CursorPos::AfterBack => list.back_id,
        };

        let Some(prev_id) = prev_id else {
            return (CursorPos::BeforeFront, None);
        };

        (
            CursorPos::At(prev_id),
            Some((prev_id, list.get_val(prev_id))),
        )
    }

    pub fn remove<'a, T>(self, list: &'a mut DoublyLinkedList<T>) -> (CursorPos, (NodeId, T)) {
        let curr_id = match self {
            CursorPos::BeforeFront => panic!("Cannot make cut at BeforeFront"),
            CursorPos::At(curr_id) => curr_id,
            CursorPos::AfterBack => panic!("Cannot make cut at AfterBack"),
        };

        let result = list.remove(curr_id);

        let prev_pos = match result.prev_id {
            None => CursorPos::BeforeFront,
            Some(prev_id) => CursorPos::At(prev_id),
        };

        (prev_pos, (curr_id, result.val))
    }
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
        self.pos.current_node(self.list)
    }

    pub fn current_id(&self) -> Option<NodeId> {
        self.pos.current_id()
    }

    pub fn current_val(&self) -> Option<&'a T> {
        self.pos.current_val(self.list)
    }

    pub fn move_next(&mut self) -> Option<(NodeId, &'a T)> {
        let (next_pos, next_val) = self.pos.next(self.list);

        self.pos = next_pos;
        next_val
    }

    pub fn move_prev(&mut self) -> Option<(NodeId, &'a T)> {
        let (prev_pos, prev_val) = self.pos.prev(self.list);
        self.pos = prev_pos;
        prev_val
    }
}

pub struct MutCursor<'a, T> {
    list: &'a mut DoublyLinkedList<T>,
    pos: CursorPos,
}

impl<'a, T> MutCursor<'a, T> {
    fn new(list: &'a mut DoublyLinkedList<T>, pos: CursorPos) -> MutCursor<'a, T> {
        MutCursor { list, pos }
    }

    pub(crate) fn current_node(&'a self) -> Option<&'a Node<T>> {
        self.pos.current_node(self.list)
    }

    pub fn current_id(&self) -> Option<NodeId> {
        self.pos.current_id()
    }

    pub fn current_val(&self) -> Option<&T> {
        self.pos.current_val(self.list)
    }

    pub fn move_next(&mut self) -> Option<(NodeId, &T)> {
        let (next_pos, next_val) = self.pos.next(self.list);

        self.pos = next_pos;
        next_val
    }

    pub fn move_prev(&mut self) -> Option<(NodeId, &T)> {
        let (prev_pos, prev_val) = self.pos.prev(self.list);
        self.pos = prev_pos;
        prev_val
    }

    pub fn remove(&mut self) -> (NodeId, T) {
        let (pos, result) = self.pos.remove(self.list);
        self.pos = pos;
        result
    }
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
