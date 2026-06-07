mod arena;
mod list;
mod node;

pub use list::DoublyLinkedList;
pub use node::NodeId;

#[cfg(test)]
mod tests {
    use super::list::DoublyLinkedList;

    #[test]
    fn push_pop_front_and_back() {
        let mut list: DoublyLinkedList<i64> = DoublyLinkedList::<i64>::new();

        list.push_front(0);
        list.push_back(999);

        assert_eq!(list.len(), 2);
        assert_eq!(list.pop_front(), Some(0));
        assert_eq!(list.pop_back(), Some(999));
        assert_eq!(list.len(), 0);
    }

    #[test]
    fn getters_and_setters() {
        let mut list: DoublyLinkedList<i64> = DoublyLinkedList::<i64>::new();

        let id = list.push_back(111);
        let id2 = list.push_back(222);

        assert_eq!(*list.get(id), 111);
        assert_eq!(*list.get_front(), 111);

        *list.get_mut(id2) = 999;
        assert_eq!(*list.get(id2), 999);
        assert_eq!(*list.get_back(), 999);
    }

    #[test]
    fn insert_before_and_after() {
        let mut list: DoublyLinkedList<i64> = DoublyLinkedList::<i64>::new();

        let id = list.push_back(555);
        list.insert_before(id, 111);
        list.insert_after(id, 999);

        assert_eq!(list.len(), 3);
        assert_eq!(list.pop_front(), Some(111));
        assert_eq!(list.pop_front(), Some(555));
        assert_eq!(list.pop_front(), Some(999));
    }

    #[test]
    fn remove_node() {
        let mut list: DoublyLinkedList<i64> = DoublyLinkedList::<i64>::new();

        list.push_back(111);
        let id = list.push_back(555);
        list.push_back(999);

        assert_eq!(list.remove_node(id), 555);

        assert_eq!(*list.get_front(), 111);
        assert_eq!(*list.get_back(), 999);
    }

    #[test]
    fn set_node_value() {
        let mut list: DoublyLinkedList<i64> = DoublyLinkedList::<i64>::new();

        let id = list.push_back(111);
        list.set_node_value(id, 999);
        assert_eq!(*list.get(id), 999);
    }

    #[test]
    fn move_node_before() {
        let mut list: DoublyLinkedList<i64> = DoublyLinkedList::<i64>::new();
        let id1 = list.push_back(111);
        let id2 = list.push_back(222);
        let id3 = list.push_back(333);

        list.move_node_before(id1, id3);

        // no nodes should be deallocated
        assert_eq!(list.len(), 3);

        // values at nodes should not be modified
        assert_eq!(*list.get(id1), 111);
        assert_eq!(*list.get(id2), 222);
        assert_eq!(*list.get(id3), 333);

        // but 111 should be moved to the middle...
        assert_eq!(*list.get_front(), 222);
        assert_eq!(*list.get_back(), 333);

        // now try moving 111 back to front
        list.move_node_before(id1, id2);
        assert_eq!(*list.get_front(), 111);
        assert_eq!(*list.get_back(), 333);

        // last, try moving 333 to front
        list.move_node_before(id3, id1);
        assert_eq!(*list.get_front(), 333);
        assert_eq!(*list.get_back(), 222);
    }

    #[test]
    fn move_node_after() {
        let mut list: DoublyLinkedList<i64> = DoublyLinkedList::<i64>::new();
        let id1 = list.push_back(111);
        let id2 = list.push_back(222);
        let id3 = list.push_back(333);

        list.move_node_after(id1, id2);

        // no nodes should be deallocated
        assert_eq!(list.len(), 3);

        // values at nodes should not be modified
        assert_eq!(*list.get(id1), 111);
        assert_eq!(*list.get(id2), 222);
        assert_eq!(*list.get(id3), 333);

        // but 111 should be moved to the middle...
        assert_eq!(*list.get_front(), 222);
        assert_eq!(*list.get_back(), 333);

        // now try moving 111 back to front
        list.move_node_after(id2, id1);
        assert_eq!(*list.get_front(), 111);
        assert_eq!(*list.get_back(), 333);

        // last, try moving 333 to middle
        list.move_node_after(id3, id1);
        assert_eq!(*list.get_front(), 111);
        assert_eq!(*list.get_back(), 222);
    }
}
