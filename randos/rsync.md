Designed for efficient transfer. Rsync first compares using a checksum
to see what files to send. (Actually, for efficiency, it by default
just uses file size and modification time; you can tell it to checksum
the file).

So say a file has changed, we want to send those parts which have been
modified. The receipient splits their file into chunks. They calculate
the MD5 and a simpler, rolling checksum for each chunk. These are sent
to the sender.

The sender does the rolling checksum for each chunk. If the checksum
differs, he knows he has to send it. If they're the same, he checks
with MD5. The idea, presumably, is to avoid the expensive hashing if
possible.

But that's not the whole story. Because the checksum is rolling, even
if the blocksize is `K`, the user can generate more than `N/K`
checksums. Instead, they generate `N/k` checksums, where `k` could be
as small as 1 byte.

When the sender is deciding what to send, they calculate the rolling
checksum. If they find that checksum somewhere, they just say "This
block moved from here to there." This helps a lot when code moves
around: for instance, if you remove one sentence from a file.

The key is the rolling checksum. If not for that, everything would
have to be aligned at block boundaries.

Because the rolling hash isn't very strong, we do want to double check
with MD5. The idea here is that we chose one hash for its rolling
property, and a second for its strength.

## Signature Searching

Okay, now you have a bunch of rolling hashes. You need to search
inside it. He uses an index of the first 16 bits of the rolling
hash. That's small. These indexes jump into a sorted array of hashes,
and he does a linear search therein.

## Resources

https://www.samba.org/~tridge/phd_thesis.pdf
