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

        assert_eq!(*list.get_val(id), 111);
        assert_eq!(*list.get_front_val(), 111);

        *list.get_mut_val(id2) = 999;
        assert_eq!(*list.get_val(id2), 999);
        assert_eq!(*list.get_back_val(), 999);
    }

    #[test]
    fn get_next_and_prev() {
        let mut list: DoublyLinkedList<i64> = DoublyLinkedList::<i64>::new();

        let id1 = list.push_back(111);
        let id2 = list.push_back(222);

        assert_eq!(list.next_id(id1), Some(id2));
        assert_eq!(list.prev_id(id2), Some(id1));
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

        assert_eq!(list.remove(id).val, 555);

        assert_eq!(*list.get_front_val(), 111);
        assert_eq!(*list.get_back_val(), 999);
    }

    #[test]
    fn set_node_value() {
        let mut list: DoublyLinkedList<i64> = DoublyLinkedList::<i64>::new();

        let id = list.push_back(111);
        list.set_node_value(id, 999);
        assert_eq!(*list.get_val(id), 999);
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
        assert_eq!(*list.get_val(id1), 111);
        assert_eq!(*list.get_val(id2), 222);
        assert_eq!(*list.get_val(id3), 333);

        // but 111 should be moved to the middle...
        assert_eq!(*list.get_front_val(), 222);
        assert_eq!(*list.get_back_val(), 333);

        // now try moving 111 back to front
        list.move_node_before(id1, id2);
        assert_eq!(*list.get_front_val(), 111);
        assert_eq!(*list.get_back_val(), 333);

        // last, try moving 333 to front
        list.move_node_before(id3, id1);
        assert_eq!(*list.get_front_val(), 333);
        assert_eq!(*list.get_back_val(), 222);
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
        assert_eq!(*list.get_val(id1), 111);
        assert_eq!(*list.get_val(id2), 222);
        assert_eq!(*list.get_val(id3), 333);

        // but 111 should be moved to the middle...
        assert_eq!(*list.get_front_val(), 222);
        assert_eq!(*list.get_back_val(), 333);

        // now try moving 111 back to front
        list.move_node_after(id2, id1);
        assert_eq!(*list.get_front_val(), 111);
        assert_eq!(*list.get_back_val(), 333);

        // last, try moving 333 to middle
        list.move_node_after(id3, id1);
        assert_eq!(*list.get_front_val(), 111);
        assert_eq!(*list.get_back_val(), 222);
    }

    #[test]
    fn val_iterator() {
        let mut list: DoublyLinkedList<i64> = DoublyLinkedList::<i64>::new();
        list.push_back(111);
        list.push_back(222);
        list.push_back(333);

        let mut iter = list.val_iter_at_front();

        assert_eq!(*iter.next().unwrap(), 111);
        assert_eq!(*iter.next().unwrap(), 222);
        assert_eq!(*iter.next().unwrap(), 333);
    }

    #[test]
    fn id_iterator() {
        let mut list: DoublyLinkedList<i64> = DoublyLinkedList::<i64>::new();
        let id1 = list.push_back(111);
        let id2 = list.push_back(222);
        let id3 = list.push_back(333);

        let mut iter = list.id_iter_at_front();

        assert_eq!(iter.next().unwrap(), id1);
        assert_eq!(iter.next().unwrap(), id2);
        assert_eq!(iter.next().unwrap(), id3);
    }

    #[test]
    fn mut_cursor_remove() {
        let mut list: DoublyLinkedList<i64> = DoublyLinkedList::<i64>::new();
        let id1 = list.push_back(111);
        let id2 = list.push_back(222);
        let id3 = list.push_back(333);

        let mut cursor = list.mut_cursor_at_front();
        assert_eq!(cursor.move_next().unwrap().0, id1);
        cursor.move_next();
        assert_eq!(cursor.remove().0, id2);
        assert_eq!(cursor.move_next().unwrap().0, id3);
        assert_eq!(cursor.move_next(), None);
        assert_eq!(cursor.move_prev().unwrap().0, id3);
        assert_eq!(cursor.move_prev().unwrap().0, id1);
        assert_eq!(cursor.move_prev(), None);
    }
}
