TODO3: What's virtual circuits vs packet switching?

Routing algorithms: RIP is how routers talk to each other to transmit
the IPs they can reach, and the distance. It sounds like they run some
distributed Bellman-Ford type thing. They transmit updates
periodically. The distributed version converges over time to the true
best routing info, but when network changes, can temporarily create
routes that are cycles, or otherwise inefficient. Apparently some
modifications to RIP are used to mitigate these problems.

Congestion control asks nodes to rate limit packets. This applies
back-pressure, eventually all the way to the source. Otherwise, a lot
of packets will be dropped when routers run out of queue memory. This
necessitates re-transmits, consuming bandwidth unnecessarily.

Many "classes" of IPs used to exist; used a certain number of bits for
the allocation (8, 16, 24). The remainder were to be internally
allocated. Now they use CIDR, which allocates ranges of class c
addresses for more granularity.

IP spoofing involves forging the sender to insert packets into a
stream. You can guard against this by:

* Doing Diffie-Hellman exchange to establish a session-key
* You can avoid MITM attacks by having the server sign its choice of
  DH data.
* In fact, you could just have the client use the public key to
  establish a session key. Downside: if the server's private key is
  later compromised, we can go back to previously recorded
  transmissions and decrypt the stream.
* Jonathan says using DH exchange enables perfect forward secrecy,
  because even the decrypted traffic won't allow you to know the
  session key agreed by the parties.
* Cool!

NAT. It's a thing. TODO3: STUN. I think this just works with a relay
server.

UDP: unidirectional, out of order, no guarantee of delivery. Very low
overhead. TCP: bidirectional, sequenced. Packets are buferred if one
is missing. That means we must acknowledge package receipt. Every once
in a while, the recepient sends back the highest packet number
received (before any gaps). It also gives a window, which is the
amount the sender should send before the next ack. **This is different
from congestion window**. The window size is about receivers ability
to process incoming data, not about congestion in the network. If a
frame has not been acknowledged within a timeout period, it is resent;
only upon receipt can TCP remove it from the sender's output buffer.

TODO3: Multicast? Anycast?
