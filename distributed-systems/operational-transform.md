Operational Transforms are exactly for colaborative editing.

Properties:

* Consistency: operations that have a happens-before relationship
  continue to be applied in that order.
* Convergence: at quiescence, everyone converges to the same contents.
* Intention Preservation: you want an operation to continue to do the
  same thing as the user expected it to do, even if you change some
  history before it when merging.

Okay, we'll again have `insert('chars', idx)` and `delete(idx, len)`.

The idea is going to be that when we perform an operation, we first
*transform* it to compensate for already-applied changes from other
users. I think you can keep a version history of edits.

Here's some of the ideas:

* If you insert op2 after an insert for op1, then you need to
  increment the start index of op2 by the length of op1.
* If op2 inserts before op1, do nothing.
* Et cetera.

Other things to watch out for:

* Cursor position.
* Undo/Redo stack.

I think the math they use behind OT is that, if you have two
concurrent actions A and B, there is a function `xform(A, B)` that
returns `(A', B')` such that `B'A = A'B`. That is, a way to fixup
both.

## OT vs CRDT

OT came first. OT is about sending operations, which are then
transformed relative to local changes on the other side. CRDT is about
sending states that get merged.

OT can be more complicated, and traditionally implementations had
mistakes that resulted where eventual consistency was not acheived.

I think OT is more general.

## Sources

* https://davidwalsh.name/collaborative-editing-javascript-intro-operational-transformation
    * Kinda useful, but maybe oversimplifies?
* https://docs.google.com/presentation/d/1soRtddFXH-zATvr8vvwTvfGvgqlDw0RaclcOKjOcxCk/present
    * Not actually super useful...
* http://www.codecommit.com/blog/java/understanding-and-applying-operational-transformation
    * Seems very comprehensive. But also kinda over my head at the
      moment.
* High-Latency, Low-Bandwidth Windowing in the Jupiter Collaboration
  System
    * Most cited paper in this area.
* https://news.ycombinator.com/item?id=7904220
    * HN thread where someone says that OT is hard to get write;
      that's what the CRDT people say.
* https://www.youtube.com/watch?v=NSTZ4mIv_wk
    * Someone said this was a useful video where OT and CRDT were
      contrasted.
