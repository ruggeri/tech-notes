## Serializability

* Serial schedules are simple, correct, but no parallelization.
* Serializable transaction processing schedules are what we desire.
* They talk about *conflict-serializability*. That means that you can
  swap the order of any two *nonconflicting* reads/writes (those that
  don't read/write same value), and the result is *still*
  serializable. Conflict-serializability says you can do as many of
  these swaps as you want.
    * This is stronger than serializable. It's like a stability
      property.
    * I think they bring this up because there is an easy graph-based
      test for conflict-serializability.
    * And of course conflict-serializable implies serializable.

## Locks

* Two-phase locking. Acquire all the locks needed, then unlock them
  all after commit.
* Of course, deadlock can happen.
* They talk about read/write locks.
* Upgrading locks from read to write.
    * You don't want to grab a write lock you don't need.
    * But if you allow someone else to get a shared read lock, but
      then you need to upgrade to a write, what do you do?
    * You could abort their lock, but that wastes cycles.
    * You could wait, but now you have a new possibility for deadlock.
        * For instance, what if you both grab a read lock on X, then
          both try to get a write lock on it?
* They suggest an *update lock*. Readers don't block updaters. But
  updaters do block other updaters. Only an update lock can be
  upgraded.
    * That basically means that read locks are only used for those
      transactions where provably we will not modify this record.
* They talk about *increment locks*.
    * These are for commutative actions.
    * Increment locks allow other increment locks, but not read or
      write locks.
* They mention that to prevent phantoms, we must lock the entire table
  on insert/delete. That's not true, if we can lock an index block!

## Lock Scheduling

* They discuss lock scheduling (at a very high level), but this isn't
  that interesting to me.

## Tree Protocol

* Basically, we're asking ourself: how do we do locking on a B-tree in
  a way that allows concurrency.
* Here's our approach:
    * To lock a node, you must have locked its parent first
      (exception: root).
    * You can release locks at any time.
    * You cannot reacquired locks you've released.
* I think what they really mean is, acquire locks on your way
  down. Release immediately those you know you won't use again.
* The idea here is that you only really need to lock a *subtree*. So
  you can just release those subtrees you won't touch anymore.
* I guess the only thing I don't like is that you don't necessarily
  know who rotations are going to effect, if you do inserts/deletes.

## Timestamps

*My notes are elsewhere*.
