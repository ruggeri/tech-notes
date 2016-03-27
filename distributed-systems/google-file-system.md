* One master.
    * This maintains file metadata. Most important, it keeps file
      system paths, and mapping from a path to the chunks in that
      file.
        * Changes to this are stored in a replicated operation log.
        * They do some buffering when they can of writes to the
          log. They also do checkpointing.
    * Master does not maintain information about what servers have
      what chunks. On master startup, that information is received
      through polling the servers.
    * Presumably that reduces the load on the master
* Chunkservers have chunks of files. These are partitions.
    * Typically a chunkserver serves many chunks, will make it easier
      to further partition files when machines are added.
* Client contact master to find where chunks are located.
    * But this information can be cached, so the load is not too
      great.
* The master picks a "primary" for each chunk.
    * All writes go through this chunkserver. The primary orders all
      writes.
    * Typically primary lease is 60sec, but as long as the primary is
      in contact with the master this can be extended arbitrarily.
    * Client will ask master who the primary is. Master will reply
      with primary and the other replicas.
    * Client sends data to all replicas. They buffer this.
    * Then client sends write request to primary. It will apply this
      write in a serial order.
    * The primary forwards this write to the secondaries. Secondary
      applies write in the same serial order.
    * The secondaries reply that they succeeded, at which point
      primary replies.
* Typical mode is just to do appends.
    * Only guarantee is that you will write the record in its entirety
      at least once in the chunk.
    * You can write it many times, or write it in a corrupted way.
    * It's on the client application to deal with that. Typically they
      checksum their blocks.
    * The application can have "checkpoints" in the file (which is a
      log). It can ignore inconsistent parts of the log.
