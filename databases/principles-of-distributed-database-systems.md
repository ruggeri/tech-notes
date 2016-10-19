## Ch1: Introduction

* DDBMS: Multiple logically interlated databases over a network.
* Differences between DDBMS and parallel DBMS is vague. Parallel DBMS
  in a shared-nothing architecture is very similar to DDBMS. Main
  difference is assumption of homogeneity. Parallel DBMS is also
  covered in this book!
* Promises:
    * Transparency; should act like one database.
        * Network: shouldn't have to think of network.
        * Replication: Shouldn't have to think of keeping replicas
          up-to-date, or which one to use.
        * Fragmentation: shouldn't have to do the sharding
          ourselves. Queries should be broken up to shards for us.
    * Increased reliability.
        * Should be resilient to failures.
        * Maybe failures take down part of the system, but the rest
          can operate.
    * Performance
        * More sites means pooling of CPU and IO power.
        * Closer sites means better latency.
    * Easier scaling.
        * Should just be able to add new sites.
* Complications:
    * Replication: how to choose where to store, how to receive, how
      to keep all copies up to date.
    * How to deal with failures.
    * How to keep things synchronized across sites when there may be
      high latency between them. In particular, how not to degrade
      performance too much.
* Design Considerations:
    * Distributed Query Processing.
    * Distributed Concurrency Control.
    * Distributed Deadlock Management.
    * Reliability.
    * Replication.

## Ch2: Background

* Types of network:
    * Bus: everyone shares one channel. Senders need to listen to hear
      if anyone else tries to send simultaneously. Choose a random
      backoff.
        * This approach is called CSMA/CD: carrier sense multiple
          access with collision detection.
        * Actually, it looks like we don't really need this now sense
          everything is connected via switches.
        * Hubs create shared collision domains, but no one uses those
          anymore. Their only advantage was price over switching
          technology.
    * Star: people speak directly to a central node that forwards the
      message to the recipient.
    * Ring: token passes around.
    * Mesh: everyone has a point-to-point interconnection.
* Unicast networks have packets sent to recipient via
  switching/routing.
* Broadcast networks communicate over channel utilized by all nodes.
    * Some networks support multicasting where message is sent to a
      subset of nodes.
* Broadcast is mostly radio or satellite. Mentions HughesNet as an
  example!
* Measures:
    * Bandwidth: bits per second.
    * Data transfer rate: after you account for overhead of
      encoding/decoding, error correction, appending headers/trailers,
      etc. Basically: the usable bandwidth.
* Circuit vs packet switching
    * Circuit exists for entire connection. Presumably a path is
      dedicated for the line. In electro-mechanical systems, I assume
      this is done by an operator patching you in. Indeed, operators
      were the original switches!
    * Packet switching is where each packet is addressed and routed
      independently. They may even take different routes or arrive
      out-of-order.
    * Packet switching is especially useful because we don't need to
      dedicate bandwidth to a circuit; we can better utilize overall
      bandwidth. This is important because traffic is bursty: a
      circuit would need to be provisioned at maximum capacity, even
      if it typically uses much less capacity than this.
    * Circuit switching can be better when packet switching introduces
      to much extra delay or delay variance. This affects multimedia
      applications.
* Talks about protocol stack:
    * Application message.
    * Then TCP/UDP layer.
        * TCP: requires setup. In-order, reliable. Flow-control to not
          overwhelm receiver with data. Congestion control to backoff
          when network is saturated.
        * UDP is connectionless. No such guarantees; preferred if
          application wants to choose what guarantees it needs.
    * Then IP layer.
    * Then whatever is needed for the physical network.
