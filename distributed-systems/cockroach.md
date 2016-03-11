* Row-level locking.
* To edit a row, you put in place a switch with references the
  transaction ID.
* The transaction ID is itself stored in the DB, especially whether it
  commit.
* All switches are "flipped" when the transaction ID is commited.

Therefore it is "atomic"; everything goes through or does
not.

However, non-repeatable reads would be a problem. Presumably this
could be resolved through some kind of MVCC situation. Reads are also
slow in that they require a 2nd disk hit to look at whether a
transaction is committed.
