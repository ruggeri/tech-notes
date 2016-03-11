## Basics

In BitTorrent, someone starts seeding the file. This is the
**tracker**. By downloading the torrent file, downloaders can find the
tracker. They then download pieces of the file, which are
cryptographically signed.

They download random pieces. The master also directs them to talk with
other downloaders, who can share the pieces they have.

BitTorrent is decentralized in the sense that anyone can make a
tracker file and share it on the internet. There are torrent indexes,
but these are trivial to start.

## Trackerless torrents

What if the originator stops torrenting the file? Can you shut down
the swarm by targetting one machine? Even this master can be
eliminated through a distributed-hash table.

Take a DHT implementation like Chord. This is a ring of nodes, each of
which contains `log(n)` links of `2**i` length further in the ring.
Navigation to a node requires `log(n)` communications. A node in the
ring is responsible for a range of hashes.

Take a cryptohash of the torrent name, now store an array of nodes
seeding that file in the DHT. Joining the torrent just involves
looking up who the peers are.

BitTorrent clients presumably have some means of finding the DHT, like
root DNS servers are distributed. The DHT is like a search engine for
people seeding material.

## DHT Details

* Adding a node:
    * Enter yourself somewhere.
    * Inform all nodes that should point to you about your presence.
      Update `~log(n)` nodes' routing tables, `log(n)` to find these.
    * You can just go back to the first node that would point to the
      new node. You update this, and walk backwards, visiting each
      previous node, possibly updating that, until the finger entry
      points before the predecessor of the new node.
    * Get necessary data from the successor node.
* Removals:
    * Keep track of successors of successors. Then you can skip over a
      successor who leaves the table.
    * Can also store stuff from adjacent nodes for replication
      purposes.
* Stabilization:
    * When many nodes add at same time, may not be effective or correct
      to aggressively update finger tables.
    * Need to update finger tables when removing.
    * Not super interested in this detail ATM, though.

## Enforcement

Peers may engage in tit-for-tat, where they only give data to peers
that give data back to them. However, a pure tit-for-tat would mean no
sharing, so a portion of bandwidth may be reserved for purposes of
finding new friendly peers.
