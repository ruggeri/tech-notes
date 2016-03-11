* Atomicity
    * Transactions can be commited or rolled back.
    * Rolled back commits have none of the changes made.
    * Commited transactions have 100% of changes made.
    * All changes become visible simultaneously.
* Consistency
    * Database constraints are enforced and will not be violated.
* Isolation
    * Goal: transactions processed in a way compatible with a linear,
      sequential processing.
    * Another way: throughout a transaction, you should see all the
      changes of a prior transaction, or none of them.
    * Many different isolation levels.
* Durability
    * A commited transaction is never lost.
