With Lamport clocks, events are sent to a node with (a) the message, and
(b) the client's most recently seen clock.

When a node receives a message, it sets its clock to the maximum of (a)
it's current clock and (b) the client's submitted clock. It then
increments. It replies with this value to the client.

Now we know that events submitted to the system will be assigned Lamport
timestamps which respect causal order.

## Totally Ordered Broadcast (Unreliable)

But how can we use this? They talk about a way to do totally ordered
broadcast:

* A client submits a message to a node.
* The node assigns that a Lamport timestamp and then broadcasts it to
  every node (including itself). The event has not yet been processed.
* On receipt of event, place it in a queue.

* Every time there is a new event at the head of the queue, send an
  acknowledgement that you have it *to everyone*.
* When you have received acknowledgements that everyone has the message
  at the head of their queue, you may remove and process it.
  * Why wait? Because only when you receive ACKs from every other
    process do you know that there will be no further, lower-ordered
    events.

Note: this relies on reliable message delivery! Otherwise you won't know
that your ACK has been received by everyone before you ACK the next
message! You can solve this using TCP I suppose.

Of course, this can't handle failure of even one node.

## Get/Set Requests

I believe that using the above TOB algorithm, you should be able to
write get/set algorithms. Set commands can be accepted right away. But
get (or, for that matter, CAS) commands will have to block. We won't
know the value to return.

If it is acceptable for different clients to view different histories of
values, so long as all observed histories are compatible with causality,
then you can return a response to a get right away.

But note that a CAS must still be synchronous. You cannot say whether it
succeeded until you know all prior numbered events.

## Vector Clocks

Note a problem: you wait to process event X after processing all lower
Lamport timestamp events Y. You know that if Y caused X, then Y will be
processed before X. But lots of events Y *won't* have caused X. You're
waiting unnecessarily.

You can be a little more intelligent. If you keep vector clocks, then
you can wait to process an event when you've processed all the message
ids recorded in the vector clock.

Now, this means that you will *no longer* necessarily process messages
in the same total order. Concurrent messages may be reordered.

They give an example of a distributed messaging board. You want to
display a message posted to the board only if all the messages that it
*replies to* have also been received and processed.

That means if someone requests the message thread, they may not see
concurrently submitted messages. But they will see a message iff all
(causally) prior messages willalso be displayed.

## Sources

* www.cs.princeton.edu/courses/archive/fall16/cos418/docs/L4-time.pptx
