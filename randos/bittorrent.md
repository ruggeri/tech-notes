# BitTorrent Peer Protocol Basics

Typically you start with a metadata ".torrent" file. This lists the
files included in the torrent, which are then split into smaller
"pieces", each of which has a cryptographic hash recorded. Within this
torrent file may be listed the IP address of a "tracker", which will
identify peers where you can start downloading the file from. We'll
return to "tracker" soon. You can find the .torrent file on search
engines; when you've got it, you can start your search for peers to
download file pieces from.

A BitTorrent client connects to peers. It can make RPCs like:

- `keep-alive` is a ping to let connected peer know you're still around.
- `request`/`piece` are RPCs to request a piece of a file, or to send
  it.
  - A peer will request pieces in a randomish order, for best fan-out of
    pieces.
  - In fact, it tends to request _rarest_ pieces amongst the peers it
    has contact with, so as to increase fan-out at fastest rate! Not
    essential, but a nice optimization.
- `choke`/`unchoke`: this tells a peer that you are willing or unwilling
  to send them pieces.
  - By telling peers that you are choking them, they won't bother you
    making `request`s until you unchoke them.
  - The decision to choke/unchoke is at each peer's discretion.
    Typically, you unchoke peers who are giving you the best return
    bandwidth.
  - Plus you unchoke one peer randomly to discover new peers that will
    give you good bandwidth.
- `bitfield`/`have`: `bitfield` tells a newly connected peer what pieces
  you have (a bitfield, one bit per piece). `have` tells them whenever
  you complete a piece.
- `interested`/`not interested`
  - Since unchoked slots are typically limited, it is nice to tell a
    peer when you are `not interested` in any pieces that they have.
    That way, even if you are uploading quickly to them, they don't hold
    an unchoked slot for you.
  - You might become `interested` if you learn they acquire a piece that
    you need.

The idea is to allow non-sequential download of pieces of files, where
the peers may have some but not all of the pieces. This allows for
better use of the total bandwidth available, allowing for a kind of
fan-out. Works best when pieces are requested/downloaded in a random
order; indeed in "rarest amongst peers" order.

Since anyone can join the swarm, including malicious actors, we need to
consider some security.

- Generally a bit hard to overwhelm with abusive traffic, because if you
  start sending a lot of requests to peers they will just block you.
  There are many peers, so you'd need to knock out a lot of machines.
- Protected against malicious upload of garbage data because of the
  cryptographic hash.
- Not anonymous: if you are trading some illegal file, your IP address
  will be seen by untrusted peers who might be recording your IP address
  for later de-anonymization.
  - Late 2000s/early 2010s, rights holders would monitor swarms, noting
    IPs. They would notify ISPs, who would then warn users. This was a
    cooperation between industry like RIAA and ISPs like Comcast.
  - ISPs cooperated because this insulated them from liability for
    actions of their users.
  - This was called "Copyright Warning System." But it wasn't effective
    in deterrence. So eventually it was wound-down. Rightsholders then
    just focused on lawsuits against big uploaders.
  - They would file a John Doe lawsuit, then subpoena ISP records to
    match IP address assignment with a person. But this can be defeated
    by VPNs. And biggest seeders can be in jurisdictions which don't
    facilitate lawsuits (Eastern Europe).
- Choke/unchoke is generally enough to get peers to help distribute the
  file.
  - Non-seeding peers (haven't completed file) prefer and unchoke peers
    who give them good upload rates reciprocally. This is basically
    tit-for-tat. It is a quite good incentive to cooperate.
  - Seeders, by default, will upload to whoever can download the fastest
    from them. This means that non-cooperators can leech off seeders and
    use up all their bandwidth.
  - However, if there are many more cooperators than seeders, it is
    better to cooperate, since seeder bandwidth is a limited and scare
    resource, even if it is given nondiscriminatingly to cooperators and
    non-cooperators alike.
  - Basically, by default, seeders prefer maximum upload rate over
    attempt to ensure fairness.
- Once a torrent has been fully downloaded, there is not really any
  incentive in the base protocol to keep seeding. Most people disconnect
  on reaching 100%.
- No centralized point of failure in peer network: everyone equally has
  pieces they can share. If you don't take all peers down, the file will
  continue to trade.
  - Torrent search engines are very easy to start; you can attack them,
    but it is whack-a-mole. Bandwidth requirement is low because it's
    just an index, doesn't serve files. Not illegal in all
    jurisdictions.
  - In fact, often could be many competing search engines or places to
    share torrent files, making it very difficult to stop trading.

## Peer Discovery: Tracker

You need to find the peers in the first place. This historically is the
job of the "tracker". It tracks the peers. The .torrent file lists a
tracker or trackers. To join, you register yourself and ask tracker for
peers. You poll every once in a while for more/new peers.

Tracker is a single point of failure. If tracker is taken down, the
.torrent file can be re-uploaded to search engines, but with the tracker
information updated. This is still the "same" torrent, because the
tracker isn't part of the "infohash" (hash of piece/file info) which
identifies the torrent.

It's not obviously very profitable to run a tracker. You can't exactly
show ads. And, if you are doing nothing to stop people uploading
copyrighted material, you may have legal risk. It appears to be mostly
run by volunteers, and sponsored by donations? Trackers are less
important today because (1) centralization is a weakness, (2) not very
strong incentive to run one, and (3) alternatives exist (DHT and PEX).

**Private Trackers**

"Private" trackers are invite-only and only let in peers who
authenticate with them. Normally this is done by embedding a user token
in the tracker URL in the .torrent file. When connecting, the tracker
looks for this token. Of course, if you share the .torrent file, you'll
let other people connect as "you".

Private trackers often enforce upload/download ratio requirements. They
do this is a very simple way: BitTorrent client will send "announce"
messages to tracker every so often giving amount of bytes
downloaded/uploaded. The tracker will trust these messages. If you fall
below a ratio requirement, the tracker will stop giving you peers.

Tracker can check whether your claims are plausible based on other
user's announce messages about how much they've downloaded. You can
start to look suspicious, at which point you're banned.

You absolutely can spoof the announce messages, but that basically
requires modifying the source of your BitTorrent client. Private
trackers can require a whitelisted client; malicious clients can pretend
to be a whitelisted client in their `User-Agent`, and this is
whack-a-mole, but somewhat effective.

Basically, there is no fancy cryptography or distributed
monitoring/enforcement. But it gets to be more trouble to lie and
pretend to upload than simply upload in the first place!

## Peer Discovery: PEX

This was added as an extension to BitTorrent: a BitTorrent Enhancement
Proposal (BEP).

Basically, when you connect to a peer, they can ask you to periodically
send a list of users you have connected to recently, and that you have
disconnected from. This allows peers to learn about other peers
organically outside the tracker.

Note you only send who _you_ have connected to (and disconnected from).
This prevents abuse from propagating, or hijacking the swarm to do a
DDOS.

PEX can reduce load on tracker by offloading peer discovery to peers
themselves. Tracker normally won't give you a "full" list of peers,
because that would use a lot of bandwidth. Tracker wants you to
"announce" only every 30min, and typically gives only 50 peers in
response. PEX messages are about one per min, and list at most 50 peers
added. More of the peers will be live from the PEX messages (because
they have more recently connected).

You might even be able find peers who have joined via other tracker.
However, that is more likely to happen via Tracker Exchange (another
extension).

Private trackers often expect you to disable PEX so that people can't
infiltrate the swarm by bypassing the tracker.

## Peer Discovery: Trackerless Torrents and DHT

Clearly trackers are a single point of failure. The tracker could be
shut down or censored. It is clearly a higher value target. But we can
eliminate its role through a distributed hash table.

Our key is going to be the "infohash" of a torrent; the hash of the most
important metadata (file names, pieces, and piece hashes, basically).
The value for the hash will be a list of peers participating in the
swarm.

A peer who wants to join will look up the infohash in the DHT. It will
find the peers, and it will connect to them. It will add itself. The DHT
will expire a peer if it doesn't hear from it for a while.

To enter the DHT, you still need some DHT nodes. These can come from the
.torrent file itself, can be set in the client software (basically
"root" nodes, though nothing makes them special in the DHT), saved from
previous DHT interactions. It's hard to take apart the DHT because there
is no central node playing a special role.

BitTorrent software will generally start running a DHT node. So DHT
nodes are not special servers, but run by regular users. As a DHT node
operator, you provide peer lookup for content you have no idea about.

The replication factor is approximately 5: on joining, you announce
yourself to the five "nearest" nodes to the infohash key. These nodes
are only expected to keep ~6k peer IDs per infohash. They don't need to
track _everyone_ because they can rely on PEX. Even if nodes are lost,
peers will eventually re-announce themselves to the DHT. Because of
sloppy routing, key/values can exist on more than five nodes.

It is possible for people to claim a key range if they maliciously
choose node ID non randomly. Doing so allows them to censor a
torrenthash. There is a proposal that node ID should have some bits
which are a hash of IP, so that malicious selection of node ID is
limited.

Use of DHT allows use of "Magnet Links", which are basically a link
which says "Open with BitTorrent client, query this infohash in DHT".
You don't need to separately download a torrent file and load it in your
client.

## DHT Details

Take a DHT implementation like Chord. This is a ring of nodes, each of
which contains `log(n)` links of `2**i` length further in the ring.
Navigation to a node requires `log(n)` communications. A node in the
ring is responsible for a range of hashes.

- Adding a node:
  - Enter yourself somewhere.
  - Inform all nodes that should point to you about your presence.
    Update `~log(n)` nodes' routing tables, `log(n)` to find these.
  - You can just go back to the first node that would point to the
    new node. You update this, and walk backwards, visiting each
    previous node, possibly updating that, until the finger entry
    points before the predecessor of the new node.
  - Get necessary data from the successor node.
- Removals:
  - Keep track of successors of successors. Then you can skip over a
    successor who leaves the table.
  - Can also store stuff from adjacent nodes for replication
    purposes.
- Stabilization:
  - When many nodes add at same time, may not be effective or correct
    to aggressively update finger tables.
  - Need to update finger tables when removing.
  - Not super interested in this detail ATM, though.

# Source

- This was very substantially updated in 2026-04-XX with help from
  ChatGPT.
