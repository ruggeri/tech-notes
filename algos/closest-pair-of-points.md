**Problem**: In a 2-dimensional Euclidean space, find the closest pair
of points.

First, sort the points in the `x` dimension. Choose the median, and
solve the problem recursively. Call the min of these solutions `r`

After solving it in both halves, you still have the possibility that
the closest pair straddle the median `x` value.

However, you know that the closest pairs have to live within `r` from
the median `x` line. Moreover, there is a max `y_max` and a min `y_min`; we
now know that the closest pair lives in the rectangle:

    (((x_median - r), (y_max)), ((x_median + r), (y_min)))

Next, note that you can divide the `y` space up into lengths of `r`:

    (((x_median - r), y_max), ((x_median + r), (y_max - r)))
    (((x_median - r), y_max - r), ((x_median + r), (y_max - 2r)))
    ...

We should also consider

    (((x_median - r), y_max - r/2), ((x_median + r), (y_max - 3r/2)))
    (((x_median - r), y_max - 3r/2), ((x_median + r), (y_max - 5r/2)))
    ...

The idea here is that if the closest pair straddles the median, it
must live in one of these boxes.

Lastly, note that we can assign each point into these boxes in `O(1)`
time by virtue of the modulo operation. Note also that geometrically,
only 6 points can fit in each box. There may be many boxes, but at
most `n` of them can have a point. If we only track boxes with a
point, there are at most `2 * n` boxes to iterate through, each with
at most 6 points. Finding the closest in each box is a constant time
operation, so we can merge our recursed results linearly!

* [Wiki](http://en.wikipedia.org/wiki/Closest_pair_of_points_problem)
