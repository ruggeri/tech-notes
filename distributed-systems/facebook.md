## Goal

Let's think of how to build Facebook in a scalable way.

## Load Balancing

First, you can list multiple A records for the web proxy servers. When
you do this, the DNS server will permute the order of the A
records. The client will try the first one, but will use others if
they can't connect. This is called Round Robin DNS. Sometimes this is
enhanced via NS records: the root nameserver tries to recursively
resolve the domain with your nameserver. Your nameserver is also an
entrypoint for your application, so it just replies with its own
IP. The advantage is that if this server goes down, the nameserver
will be down, so this will not be handed to users anymore, and they'll
go elsewhere. I'm not sure how many records like this you can make,
though.

There may also be some way of doing "client-side" load balancing,
where the client randomly chooses different subdomains to make the
request to.

You can then have load balancer servers. In fact, there are hardware
solutions for this.

Okay, I believe you can load balance!

## Application Servers

This is, in my opinion, the easy part. All the app servers just run
the same code.

Assets can be stored on a CDN. Implementing a CDN should be simple, I
think, it only pulls data into the CDN, writing once, and never
modifying. This can be scaled easily.

## Database

Obviously not all the data can fit in one SQL database. This would
have too much load. You're going to need to distribute this. You need
high throughput writing, and random reads.

You can use something like HBase. HBase is a key-value store, without
transactions. Since it shards out the data, it can handle very heavy
write load. When reading/writing, there is no coordination at
different sites (well, except replicas...).

It looks like Cassandra is now the most popular wide column store.

So my question then is: how far can you get without transactions.

## Features

* Show your feed.
* Record likes, delete likes. List and count likes.
* Comments, nested comments. Deleting comments.
* Record friendships. Record pending friendships. Delete
  friendships. List friendships.
* Show list of everything one person has done.
* Publish posts. Tag people in posts.
* Messaging
    * Most recent conversations
    * Message history of a conversation
* Push events:
    * Likes
    * New messages
* List upcoming events.
* Photo upload, list photos, organize photos.

## Posts, Likes, and Comments

* When you create a post, create a row for the post, with the
  information about it.
    * You also need to add this post to user's feed.
    * To do this, perhaps the user has a column called `posts`. Read
      this column, append the post, and save.
    * You can do a `checkAndPut` so that this read+write can be done
      atomically.
    * NB: in case of failure, you can keep this operation *in a
      message queue*. The operation should be *idempotent*. Only when
      both sites are mutated do we remove the item from the queue.
    * Likewise we should be able to edit/delete a post.
* When you like something, you just append your user ID to a list of
  people who like that thing. You also want to do an increment.
    * I guess you need to record this as an "event" to list in *your
      own* activity feed.
    * So do this as before, with a message queue.
    * Likewise we can delete a like, by removing the ID from the list
      and destroying the feed event.
* When you add a comment, you create a row for that, referencing the
  post, and any comment you replied to.
    * You add this comment to a list of comment ids in the post.
* Okay, so the basic strategy is:
    * Create a record.
    * Fill in linkages to this record. Be careful to use
      `checkAndPut`.
    * To prevent inconsistency, use idempotent messages in a message
      queue. Only remove the message when you have completed all the
      necessary actions.
    * There should be a brief time that something is not visible
      through a linkage, but that should be fine.

Source: http://www.infoq.com/articles/cap-twelve-years-later-how-the-rules-have-changed
