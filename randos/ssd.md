On an SSD, a page needs to be reset before you can write it. So if you
want to update a page, you need to read it, and write it to some blank
page elsewhere, updating the logical mapping describing where that
page lives physically.

Of course, sometimes you have to erase pages. The problem is that
pages are erased in bigger chunks than they are written. So you have
to read a bunch more memory, clear it, then rewrite it back with the
one change. Note that in fact you must *move* the big erased page,
because you need to ensure that you can recover from a failure.

One annoying bit: if you are constantly fsyncing small changes to a
log, you're going to keep moving a bunch of data around the
drive. This will also wear it down substantially, which will reduce
lifespan, since each cell can only be written so many times.

But that sucks, because you may need to fsync for transactionality
purposes in your application.

To give an idea of IOPS, 15k RPM drives give around 200 random IOPS. A
good SSD nowadays gives more like 100k IOPS. It looks like for
sequential on a rotational drive, you might get ~15k IOPS.

https://en.wikipedia.org/wiki/Write_amplification
