## Introduction

Redis is an in-memory datastore (i.e.: fast), typically used for
caching. It can distribute through master/slave, and it has some
persistence capability. It appears that it may even have transactions!
It can also be used for pub-sub.

One advantage over memcached is that Redis stores keys to
datatypes. The five provided are:

* STRING
* LIST
* SET
* ZSET (sorted set)
* HASH

Each of these datstructures supports some regular operations.

Commands

* All
    * GET key -> val
    * SET key val
    * DEL key
* List
    * LPUSH/RPUSH key val
    * LPOP/RPOP key -> val
    * LRANGE key low high (fetches a subrange of the list; LRANGE
      my-key 0 -1 gets everything)
    * LINDEX key idx -> val
* SET
    * SADD key val
    * SMEMBERS key -> [vals]
    * SISMEMBER key val -> boolean
    * SREM key val -> boolean (returns whether was member)
* HASH
    * HSET key k v
    * HGET key k -> v
    * HGETALL key -> the whole hash
    * HDEL key k -> boolean
* ZSET
    * ZADD key val score
    * ZRANGE key low high (fetches in some fixed order; insert order I
      think?)
    * ZRANGEBYSCORE key low high (fetches in the score order)
    * ZREM key val

**Example**

Let's build a Reddit system.

Might have a namespace `articles:` which stores a hash of article
properties. We then have a `articles-by-time` and `articles-by-score`
ZSETs; this contains just `article:123456` keys, but sorted by either
(1) creation time or (2) total score.

We'll add a `article-votes:` namespace to keep track of the votes for
article; we'll keep track of the users who have voted for this
article.

In our `upvote_article` function we'll:

1. SADD `article-votes:ARTICLE_ID` USER_ID
2. If not already included, we'll HINCRBY articles:ARTICLE_ID, votes, 1
    * Note this increments a value inside a hash map.
3. We'll ZINCRBY articles-by-score, ARTICLE_ID, 1

In our `post_article` function we'll:

0. INCR article_id to requisition an ARTICLE_ID to use
1. SADD article-votes, ARTICLE_ID, USER_ID
    * Gets one vote from the poster.
    * EXPIRE article-votes, ONE_WEEK
    * This way we won't waste memory recording votes in Redis forever.
2. HMSET articles:ARTICLE_ID { ... }
3. ZADD articles-by-score article, 1
4. ZADD articles-by-time article, now

Our `get_articles` would be like:

0. ZREVRANGE articles-by-score, 0, NUM_PER_PAGE
1. Iterate through article ids:
    * HGETALL article_id

Soon we'll want to start keeping track of articles by group:

```
for group_id in GROUP_IDS:
  SADD group:GROUP_ID, article:ARTICLE_ID
```

But the point is to get results for a group (think: subreddit). To do
this we write `get_group_articles`. We'll need to use an intersection:

    ZINTERSTORE key, [group:GROUP_ID, articles-by-score]

Since this is slow, we'll cache the result under `key =
group-articles-by-score:GROUP_ID`. We'll expire this in 60 sec. We'll
make the reader first check whether the key exists, then possibly
rebuild it.

This sounds suboptimal, because (1) latency, (2) cache stampeding.

Note that this makes sense if articles can be part of many
groups. Otherwise it would have made more sense to just do a `ZINCR
group-articles-by-score:GROUP_ID ARTICLE_ID vote`. Then we wouldn't
need to recalculate this evrey once in a while.

The problem is that if an article can belong to several groups, then
this would take time proportional to the number of groups it is in.

## Whirlwind Tour of Redis Use-Cases

Some uses of Reddit.

**Cookie Caching**

You can use signed cookies, but people think that's hard for some
reason. So instead you can use a cookie token, which is associated
with a session stored server-side. To speed up retrieval of the
session, you can use Redis.

They show an example where we keep a hash of token to user id, and
also a ZSET of user ids by how recently that user visited any page. We
also keep a list of the last 25 pages they've visited. They store this
as a ZSET to make sure it's 25 last unique pages.

We'll keep only the last X million sessions. To garbage collect, we'll
run a chron job every minute, counting the number of sessions. If
there are too many, the wokrer will take about 100 off the end of the
ZSET of recent sessions. They will then remove these keys.

I immediately thought: race condition. You pick someone to delete, but
then they log in! They say don't worry because (1) it's unlikely and
(2) it's a recoverable problem, someone just gets randomly logged
out. They'll also talk about how to avoid race conditions later.

Their claim: without Redis, we would be randomly updating rows
in-place, which is a very slow operation for a database. It's even
worse than just inserting/appending records, which is actually much
faster...

**Shopping Cart**

For `shoping-cart:USER_ID`, we store a hash of ITEM_ID to a quantity.

They mention that now we can do analysis of what people put in a
cart. I say to myself that it would have been better to store this in
a DB or an append-only logfile. But I think what Redis people would
say is: (1) this is way faster and (2) there is some persistence, even
if it isn't ironclad, and that's okay here. This isn't vital data: for
the purposes of analysis, we can lose some data. The speed gain
justifies it without comprimising a critical operation.

**Web Page Caching**

Most content changes infrequently. So we can add a middleware to cache
such content in Redis to avoid re-rendering. We can of course set long
expiration dates, like we do for assets. But some mostly static pages
may only be viewed once by a user, in which case the Cache-Control
header doesn't matter. And what if we do want to bust the cache
sometimes when things change?

We can combine techniques; maybe we should use a Cache-Control of 5min
or so in addition to the page caching by Redis.

**Database Result Caching**

BTW: Redis doesn't allow nested structures. You can just build a more
multipart key, though.

They suggest a zset of row ids called `delay-between-row-updates`,
where the delay is how frequently you'll want to update this row in
the cache. You'll also have a `rows-to-update` zset, which contains
row ids, sorted by when the next update should happen.

A worker extracts the first N rows to update. If it's not time to
update, sleep and repeat. If it's time to update this row, fetch it
from the DB and store it in Redis. Then add it back in
`rows-to-update`, but with the appropriate delay now added.

One nice thing with this approach is that the user always has a needed
row readily available. The update happens in the background, and the
user never waits.

**Analytics**

A lot of analytics tracking can happen in Redis, rather than recording
events into a DB unnecessarily. An example is keeping track of how
frequently pages are viewed.

We can even use this in real-time, where only the most popular pages
remain cached. On every pageview, we increment a counter of number of
times viewed; we keep a zset. Before caching a page, we check if this
is amongst the top N thousand pages; else we won't cache it. This is
what the ZRANK command does.
