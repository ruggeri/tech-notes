This is the Castro and Liskov paper. It gives an algorithm that
requires `3f+1` machines, but can work in an asynchronous network.

They are clear that synchrony is not needed to ensure safety of the
system, but it *is* required for liveness. They note that this is
required, of course, since you can't have both because FLP.

They show that if there is bounded delay in the system (even if
unknown), they will eventually make progress. They note that their
algorithm is "optimal" in the sense that you can't solve even the
simpler synchronous problem with `<3f+1` machines.

They also note: a machine must be able to proceed after talking to at
least `n-f` machines, because `f` faulty machines may just not talk to
it. But of the `n-f` it talks to, `f` of those might be faulty (if the
`f` who didn't talk merely had their packets dropped). So it must be
able to proceed with `n - 2f > f`, since the non-faulty must outnumber
the faulty. (Kinda an informal note that shows again in async model
what we already knew from sync model and original lamport paper).

Game plan:

* Client sends request to what it thinks is primary.
* Primary multicasts operation.
* Replicas execute operation, send response to client.
* Client waits for `f+1` confirms with the same answer and then knows
  it was saved.

**Client Interaction With Service**

It sends signed `(operation, client_timestamp, client_id)` message to
who it thinks primary is. `client_timestamp` is needed to ensure
exactly once semantics. It should always be incrementing.

Replicas are going to eventually send signed `(view_number,
client_timestamp, client_id, replica_number, result)` to client. The
`client_id` is needed because two clients may use the same
timestamp. The replica number is needed because otherwise client won't
know how to verify the message (I guess?).

When the client has `f+1` signed messages with same result, it knows
the operation was completed.

If the client doesn't get these messages, it may be because the
"primary" it sent the message to is either (1) misbehaving, or (2)
disconnected. So the client multicasts their message to the entire
service. There is no harm, because machines will notice if they
already processed this operation.

If a replica *hasn't* seen the client's message before, they forward
it to the current primary. If they never get further instruction from
the primary to proceed, they now suspect the primary has failed, and
the view should advance.

**Pre-prepare phase**

The goal is a replicated state machine, which really means a
replicated append-only log.

Primary is going to assign a sequence number to the operation. It will
then multicast signed `(view_number, operation_sequence_number,
operation_message_digest)` AND `operation_message`. This is the
**pre-prepare** message.

The `operation_message` is already signed by the client; it doesn't
need to be resigned; it is faster to just sign the message
digest. OTOH, you could just fold in `operation_message` instead of
`operation_message_digest` if speed doesn't matter.

They talk about how you could even use a different "transport" for
`operation_message`: one suited for larger messages.

The primary multicasts the pre-prepare to everyone, including
itself. It also acts as a "replica" for the rest of the protocol.

A replica will accept the pre-prepare message if:

* Signatures match.
* The replica is in the correct view.
* If it hasn't already accepted a conflicting preprepare message for
  this `view_number, sequence_number`.

If anything is wrong, the pre-prepare message is ignored.

**Prepare phase**

If a replica accepts the pre-prepare, it will send a **prepare**
message: `(view_number, sequence_number, digest,
replica_number)`. Again, the replica number is presumably only to
identify who is sending; presumably the signature already establishes
this...

Anyway, a replica that accepts the pre-prepare from the leader will
now multicast the prepare message to all replicas.

A replica will accept a prepare messages if:

* Signatures are correct.
* The replica is in the same view.
    * If the replica has advanced in view number, it ignores the
      prepare message.

The replica logs accepted prepare messages: it is waiting to receive
`2f` identical such messages, plus its own prepare message.

At this point, we note: if replica1 gets to `2f+1` prepares, then no
other replica2 can get to `2f+1` prepares for this `(view_number,
sequence_number)`, because at most `f` machines can be faulty, and a
non-faulty machine never sends conflicting prepares.

**Commit Message**

When a replica hits `2f+1` prepares, it is time to send a signed
**commit** message to everyone:

    (view_number, sequence_number, operation_message_digest, replica#)

We accept the commit message if:

* Signatures are correct.
* Replica is still in the correct view.

We log the commit message: we don't perform the operation yet. We now
wait for `2f+1` commits. When any machine has hit `2f+1` commit
messages, it knows that `f+1` non-faulty machines have voted to
commit.

When we hit `2f+1` commit messages, we can almost perform the
operation. However, we must wait until all operations with lower
sequence number are performed first. I guess there will never be any
gap in sequence numbers??

When the operation is performed, the replica can give the client the
response.

**Garbage Collection/Checkpoints**

Every 100 messages or so, we will send a checkpoint message. It is:
`(sequence_number, state_digest)`. This is a digest of the *state*,
the state of the data with the operations applied up to
`sequence_number`.

We wait until we receive `2f+1` of these checkpoint messages for a
given `sequence_number`; everyone should have the same state digest.

At this point, we can discard all messages before `sequence_number`.

We'll keep a copy of the checkpointed state, plus the checkpoint
messages. With this information, we can prove to any machine how to
update its state without forcing it to replay all messages (or us
storing them).

**View Changes**

When a client has received a request, but not executed it within T
seconds, the client will trigger a view change.

The trigger view change message is:

    (view_number + 1,
     last_stable_checkpoint_sequence_num,
     the set of 2f+1 signed checkpoint messages,
     for each request that entered prepare:
       pre-prepare message sent from leader
       2f+1 prepare messages)

Basically, the view change message contains proof that all the
contained messages did actually prepare.

The primary will collect up `2f+1` (including its own) view change
messages. At this point, it can declare the new view started. The new
view message is:

    (new_view_number,
     set of votes to change the view,
     regenerated pre-prepare messages with current view number)

What pre-prepare messages? The leader looks at the slowest replica's
checkpoint number, and the fastest replica's checkpoint number. These
represent finalized sequence numbers that not `2f+1` people have
performed (they must have committed, else no one would have advanced
their checkpoint to this point). The new view leader wants to "finish
the job" of these sequence numbers.

Some sequence numbers might have been skipped. We replace these with a
null operation. The point is that no operation could be "lost" since
to advance the view we need `2f+1` participants, and at least one of
them must have every accepted message.

The replicas will "redo" the protocol for the messages that have
prepared but may not have been performed by `2f+1` machines. If a
replica *has* performed the op, it can participate in the redo of the
protocol without performing the op again.

Some people may not even have the lowest numbered checkpoint. They can
request the state of this checkpoint from a peer.

## Optimizations

For large replies (like read a 1MB file), the client can designate a
machine to send the reply. The other replicas simply send a digest of
the reply. That reduces network traffic.

**Reducing Rounds of Communication**

They do an optimization to reduce latency. Normally operation is not
executed until commit messages have accumulated. However, you can
"tentatively" execute an operation as soon as you detect that it has
committed. You can send the client the result of the operation.

The client waits until they have `2f+1` of these tentative commits. At
that point, the client knows that `2f+1` replicas have executed the
command.

Before you get to `2f+1`, it is possible the view will change, and no
one else had prepared that operation, and the replica which
tentatively commit isn't available. In that case, the sequence number
will have a no op performed. So we have to be able to undo later.

This reduces the number of needed rounds by one, but obviously adds
quite a bit of complexity.

**Faster Reads**

When there is low contention on written records, we can optimize a
read. The client sends a read-only operation to every replica. They
use their current tentative state. They wait until all operations up
to the tentative state have been committed (to prevent reading
uncommitted data).

They send back the result. YOu need `2f+1` replies, and all the same
value. The values returned might differ if there were concurrent
write operations for the read data.

If so, the client must retry (maybe as a normal read-write
operation). Hopefully there is low contention and retrying is not
typically necessary. The advantage is maybe saving going through the
entire protocol just to read.

**MACs vs Digital Signatures**

A message authentication code (MAC) is a *private-key* approach to
authenticating messages. A digital signature is the public key
version. MACs are three orders of magnitude faster to apply. It took
them 43ms just to do a 1024bit RSA signature (on a 200mhz machine...).

Every pair of replicas (and also the client) shares a secret key with
everyone else. The key is 16bytes. You do a MAC by appending the
16byte secret key to the message, and taking the MD5.

Instead of using a signature (which can be verified by anyone), you
compute ALL the MACs for the other replicas, and send this as a
vector. This is called the "authenticator." Anyone can check the
message by using their entry in the authenticator.

Obviously, length of authenticator is linear in number of
replicas. But number of replicas is small, and generation of
authenticator vector is so fast compared to digital signature.

## Resources

http://pmg.csail.mit.edu/papers/osdi99.pdf
