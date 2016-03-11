## Failure Types

* Media Failure
    * Maintain parity blocks for corruption.
    * But also need to worry about drive failure (head crash)
    * Use RAID replication.
    * Keep old archives.
    * Keep redundant online copies of the DB (master-slave, for
      instance).
* Catastrophic Failure
    * Fire in datacenter.
* System failure during transaction
    * Transaction is being processed, but system loses power.
    * E.g. if transaction adds one to every counter. Where were we in
      the middle of the process?

## Undo Logs

* We keep a log of how to undo any transactions that might not have
  been completed at the time we lost power.
* The log will record the start of a transaction, how to undo each
  step, and when the transaction was committed/aborted.
    * How to undo a step is an "UPDATE" record.
* Note that some committed transactions might have been committed but
  were not written due to write buffering. But that's why we'll be
  sure to write the log record.
* Basically, we'll flush an update log record before any new value for
  a record is written to disk.
    * Likewise, we'll flush a commit log record as soon as possible,
      after we have actually written out all the work of that
      transaction to disk.
    * And the commit log record must be flushed only *after* all the
      modifications have been written to disk.
* Moreover, you can batch up your log writes.
    * Basically, you do all you DB record work in memory. You build up
      your updates, and the log.
    * You flush the undo log.
    * You flush the updates.
    * You commit.
    * You flush the record saying you committed.
    * You return that the transaction was committed.
* So everything is being buffered. However, you do require a flush
  before you can commit.
* To recover:
    * Find all uncommitted transactions.
    * Revert the values.
    * Not everyone may need to be reverted, but it can't hurt.
    * Have to travel backward in time in the log.
* Note that if we crash during recover, we're fine! Everything is
  idempotent!
* To avoid scanning the whole log, we *checkpoint*. That means
  stopping taking new transactions, let the current ones complete, and
  then checkpoint the log (maybe deleting the old).
* Nonquiescent checkpointing allows us to checkpoint in parallel with
  transaction processing. Here we record that we are starting to
  checkpoint, with the list of running transactions. When those have
  completed, we write that we've ended the checkpoint. Everything
  previous to the start of the checkpoint may be deleted.

## Redo Logging

* Undo logging requires we actualy flush the updates before
  comitting. Can we just require that the *log* be flushed?
* We can, if we use a redo log!
* Basically, write all updates to the log, followed by commit,
  followed by flush. You can later flush the actual DB updates
  whenever you want.
* If any failure occurs, we can always just replay all the committed
  writes in the log. This will complete anything that was interrupted.
* Checkpointing is, again, important!
    * When you checkpoint, you can start a checkpoint. You need to
      make sure all transactions which were *commited* at this time
      are finally *written to the DB* by the time you complete the
      checkpoint. Now you won't have to "redo" these, and can delete
      those transactions complete at the start of the checkpoint.
* OTOH, you have to keep track all modifications to the records in
  buffers until the transaction commits. *Nothing* can be written
  until commit time.

## Undo and Redo Logging Summary

* Undo logging requires us to flush *all* the DB modifications before
  flushing the final commit log message.
    * That's because the commit log message tells us not to undo the
      half-completed transaction.
    * So we have to have actually *finished* the transaction.
    * Also, we need to flush any undo record before doing the write.
    * So the most parallel thing to do is:
        * Work in memory entirely. Build up undo log.
        * When commit hits, flush log (excluding commit log entry).
        * Flush updates.
        * Flush commit log entry.
    * That's a lot of IO! And hardly any can be parallelized!
* Redo logging requires us to flush the commit log entry *before* a
  write begins.
    * That's because this will tell us how to *finish* an interrupted
      transaction.
    * The most parallel thing to do is:
        * Work in memory entirely, build up redo log.
        * When commit hits, flush the log (including commit log entry).
        * Flush out the memory whenever you feel like.
    * One downside:
        * You can't write out *any* modifications until the commit
          hits!
        * That's because there's no way to undo, and you don't know
          whether to undo.
    * Another downside:
        * For checkpointing, you need to not only track who is
          committed, but who has has all their *writes* committed.
        * We cannot delete a log entry for a record that has not been
          entirely flushed.
        * That takes some additional bookkeeping.
* Last, both undo and redo logging can have contradictory requirements
  on a block, *at the time of a checkpoint*.
    * Consider if a committed transaction has modified a block, and an
      uncommitted transaction has *also* modified that block.
    * With redo logging, we are required to flush out the block before
      we can complete the checkpoint. But we can't, because the
      transaction is not committed.

## Undo/Redo Logging

* So we might keep an Undo/Redo log. Here, we our records tell us we
  updated a record from value X to value Y.
* Before writing any record to disk, we must flush the log.
    * So that in case it fails we can undo.
* But we can also flush a commit record for a transaction even
  *before* we've written all the modifications to disk.
    * Though naturally this must happen before we commit for the user.
    * Unless we're okay with *delayed commitment*, which allows us to
      lose completed, confirmed writes.
    * There's no corruption problem with delayed commitment, of
      course.
* To recover, redo all committed transactions (in order), and undo all
  uncommitted ones (in reverse order).
* To checkpoint:
    * We start a checkpoint; there are a number of active transactions.
    * Flush the log, and all dirty blocks. Unlike redo logging, we can
      flush *all* buffers; we'll always be able to undo.
    * Write an end to the checkpoint.

## Archiving

* If we could shutdown the DB, we could just copy the the DB to
  another disk. We want to do this in a nonquiescent way.
    * Copying the log isn't quite enough, since the log can grow much
      faster than the DB. That happens if updates hit a small hot set.
* To start a dump, start copying DB files over, while they are being
  modified online.
    * At some point, we'll have copied over all the files. Note that
      this includes a *mixture* of files copied at different times.
    * But we started the dump at some point in time. So we can just
      replay the log from that point to fixup the archive.
* In fact, we want one last thing. We want to do a checkpoint right
  after we start the dump (before copying files).
    * We replay the log up to the end of the checkpoint.
    * The archive reflects the DB up to the checkpoint, and no log
      information from before the (previous) checkpoint is needed.
    * Without the checkpoint, we could recover up to the point of the
      dump start, but we could lose transactions that were running at
      the dump start time.
