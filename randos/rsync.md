# Which Files To Send

- First, it compares file size and modification time to see if a file
  needs to be re-sent. This does not involve actually reading the files,
  and it almost always works. Edits which preserve size and modification
  time are very unusual.
- However, you can use `--checksum` to force a hash like MD5 on the
  contents. Barring hash collision, you will identify files that
  changed.

# Determining Changed parts

The presumption is going to be that the sender and recipient share a
largely similar file. We are going to try to save transmission bytes.

The recipient starts by splitting their file into chunks of size `K`
(blocksize). They calculate the MD5 hash and a simpler, rolling checksum
for each chunk. These are sent to the sender. If the file has `N` bytes,
and the checksum blocksize is `K`, the recipient sends ~`N/K` MD5 hashes
and checksums.

The sender will start scanning through the file byte-by-byte. They will
calculate the checksum, but they will generate ~`N-K` rolling checksums
(rather than ~`N/K`), because they will calculate the checksum at every
byte location.

When the sender calculates a checksum that was sent by the recipient in
the initial phase, the sender computes the MD5 to double check. Assuming
they match, the sender is going to send an instruction like "translocate
chunk XYZ here".

In between translocation instructions, the sender must transmit
instructions to insert.

You only have to send those chunks _within which_ there was either an
insertion or deletion. Ideally you only have insertions at one end,
requiring sending just those insertions. If you must have deletions,
hopefully they happen in just one block.

If you have either random writes, random deletes, or a mix of both, if
you have enough of them, then possibly every block will get changed, and
no translocations will be found.

You have a tradeoff: you can make smaller blocks (more overhead, but
more chance of finding translocations), or larger blocks (less overhead,
but less chance of finding translocations). By default, rsync chooses a
block size proportional to sqrt of filesize (with a min/max block size).

## Signature Searching

How do you search quickly to see if you have a hash match? Well, he uses
a table indexed by the first 16 bits of the rolling hash. That's
relatively small. You search the leading bits of hash to find a bucket,
and then linearly traverse to see if you have a true match. Almost
always, you don't have a match.

## Resources

https://www.samba.org/~tridge/phd_thesis.pdf
