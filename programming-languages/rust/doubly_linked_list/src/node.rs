/// A handle used by end-users to identify a `Node` in a
/// `DoublyLinkedList`.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct NodeId {
    pub(crate) idx: usize,
    pub(crate) generation: usize,
}

/// An internal, reusable structure to hold a link in the list.
pub(crate) struct Node<T> {
    pub(crate) value: Option<T>,
    pub(crate) id: NodeId,
    pub(crate) prev_id: Option<NodeId>,
    pub(crate) next_id: Option<NodeId>,
}
