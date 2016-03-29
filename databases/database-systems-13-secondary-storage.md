# Composition of HDD

* Consists of several *platters*.
* Each platter consists of *tracks*, which are fixed radius from
  center.
* All tracks at a given radius across the platters are called a
  *cylinder*.
* The actuator can simultaneously pull data off a cylinder.
* The track consists of *sector*, the indivisible unit in which bytes
  are read/written by the disk. To write to a byte in a sector, you
  have to read the block, modify it in memory to change that byte,
  then write it back.
* Just as there are memory pages, there are *disk blocks*, which is
  the unit in which the OS works with the disk. It is a multiple of
  the size of a sector.

# Costs

* Positioning head (seek time)
* Spinning disk (rotation time)
* Actually reading each block (transfer time)

Of course, if you swamp the disk, your request can get queued,
too. That's another source of delay.

# Improving Disk Performance

* Locate co-accessed data on the same cylinder to eliminate seek
  latency.
* Use more disks to increase IO bandwidth.
    * You can *stripe*, which means putting one block on one disk, the
      next on a second, etc.
    * This means if you need to read several consecutive blocks, you
      can get the full advantage of the increased IO bandwidth.
    * Even if you don't do this, if there are many simultaneous
      requests for data, you get the benefit of more IO.
    * You may also use RAID.
* Try to improve disk scheduling algorithm to minimize seek latency.
    * Typically the disk control implements this.
    * Could use *elevator algorithm* which just sweeps from inside to
      out and back.
* Cache data in anticipation of future use.

# Error Handling

* Store checksums to be able to detect errors.
    * Protects against intermitent read errors.
* When writing a sector, check to make sure this was successful.
    * Re-read the sector and compare.
    * Or just check the parity, which is simpler.
    * Protects against intermitent write errors.
* Also pair sectors. Always write first to one of the pair's
  sectors. Even if a write fails and garbles that sector's data, you
  can recover from the other.
    * Protects against media decay.
    * Protects against problem where a write fails, but then we can't
      read back old contents. That can happen if we start writing then
      power fails. Then we lose both the old and new values.
* HDD can still fuckup from disk/head crash.
    * Solution is RAID. Many kinds.
    * RAID0 uses `n` disks to store the blocks. Doesn't do any
      replication, so any failure loses blocks. Random reading/writing
      is faster.
    * RAID1 is to store each block on each of `n` drives. You can
      tolerate `n-1` disk failures. Read throughput scales linearly,
      but write throughput is constant. Also you lose a bunch of disk
      space...
    * RAID2, RAID3 are not used in practice.
    * RAID4 stores blocks on `n-1` disks. The last disk stores
      _parity_ information: the first parity block is the total parity
      of the first block on each of the `n-1` drives. A failure of any
      one of the `n-1` drives can be recovered (and a failure of the
      parity drive can likewise be rebuilt). At the same time we get
      increased read and write throughput. One problem: the parity
      drive is a bottleneck for writes...
    * RAID5 is like RAID4, except instead of placing all parity info
      on one disk, you use `1/n`th of each disk for parity for the
      other `n-1` disks. That means you can tolerate one disk loss,
      but you have much better write performance.
    * RAID6 can tolerate multiple failures. This means you'll need at
      least two parity blocks. You can do this using Reed-Solomon
      coding, for instance.
        * RAID6 is basically RAID5 with a little more checking.
        * Apparently people are worried about RAID5 now?
    * There are "nested" RAID levels, of which RAID10 is most important.
        * This basically is a RAID0 array consisting of RAID1 arrays.
        * If you organize `k_1` drives into RAID1 arrays, then combine
          `k_0` of these into a single RAID0 array, you get the
          following perf.
        * `k_0/n` space efficiency. Basically, your space efficiency
          is inversely proportional to the number of drives in RAID1.
        * `n`x read performance. That's because data is distributed
          across RAID1 arrays, where it's replicated for more read
          copies.
        * `n/k_1`x write performance. You have to write a copy to each
          drive in the RAID1 array.
        * You can tolerate up to `k_1-1` failures in each of the RAID1
          arrays.

# Record Representation

* Just structs written to disk, with maybe some boring
  header/metadata.
* Store multiple records in a disk block. Again, with a header.
* There are often DB addresses which need to be translated to physical
  disk addresses. This allows some flexibility in moving DB records
  around. Probably useful for garbage collection, for instance. You'll
  need a map associating the two.
* A common alternative hybrid is to have DB addresses store the disk
  block, but also the record id. Then there's a lookup table in the
  block header.
* Once a record is loaded into memory, you want to be able to access
  it directly. One way to do this is to keep a table of DB addresses
  to memory addresses.
* Then you swizzle the pointers. Every time a record is loaded, you
  look at its out-references to other objects. If they're listed in
  the table, you replace them with direct memory references. You'll
  need a bit to keep track of what's been swizzled and not.
* This is auto-swizzling, you can also do it on demand.
* This discussion applies more to object databases, but may come up
  with indexes??
* Somewhat annoying to *unswizzle* when you want to remove that
  memory. You need to keep a table of forward and back
  references. Ugh.
* Still, the alternative is to always go through the translation map.
* When packing variable-length fields, you can keep pointers to each
  of the variable length fields (don't need the first), storing them
  after all fixed length fields.
* You can also store variable length parts in other blocks, but this
  adds indirection. OTOH, keeping variable length portions *inside*
  the record makes it harder to iterate through them, and to move them
  around. One possibility is to give yourself a certain amount of
  space for the most common sizes, and then allow overflow into
  another block if you need more.
* Some DB store by column, as this allows better compression, which
  better utilizes IO. But that isn't helpful if you want to read
  random records!

## Record Modification

* Insertion is easy if we can randomly locate records. If we have to
  keep them sorted by primary key within a block, we need to slide
  around records. We may even have to create an "overflow" block if it
  doesn't fit.
    * Note, the ability to "slide around" records inside a block is a
      great reason to keep an index at the top of the block. This keys
      primary key to offset within the block.
* Deletion is again easy if we can slide around records, to
  consolidate space.
    * We have to be careful to put a tombstone in case people point
      here? We can do that in the index by storing a null offset.
* Updates can force us to slide around for variable length records.
