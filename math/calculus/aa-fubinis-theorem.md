Fubini's theorem asks whether you can change the order of integration.

Assume that `f(x, y)` is integrable on `AxB`. Further, assume that
`g(y) = f(x_0, y)` is integrable on `B`. Then we have that:

    \int_{AxB} f(x, y) d(x, y) = \int_A (\int_B f(x, y) dy) dx

First, let me pause and note that a function is considered Lebesgue
integrable only if `\int_X |f| d\mu < \infty`.

Anyway, choose any partitioning of `A` and any partitioning of `B`
(note, these ought to both be of countable size). The you can define
`m_{i, j} = inf_{x\in A_i, y\in B_j} f(x, y)`. Likewise for `M_{i, j}`.

Obviously `\sum_{i, j} m_{i, j} vol(a_i \cross b_j)` is less than the
integral of `f(x, y)` over `A \cross B`.

Now, note consider `\int_B f(x, y) dy`, for an `x` in `a_i`. The
consider the partitioning of `B`. Note that for any `y \in b_j`, that
`m_{i, j} <= f(x, y) <= M_{i, j}`. That's because the `inf` (or `sup`)
may have been achieved at some other `x` value.

This tells us for every `x\in s_a`, the inner integral is bounded as:

    \sum_j m_{i, j} vol(b_j) <= \int_B f(x, y) dy <= sum_j M_{i, j} vol(b_j)

But those bounds imply `inf_{x\in a_i} \int_B f(x, y) dy` is no less
than `\sum_j m_{i, j} vol(b_j)`. (And a likewise statement for `M_{i,
j}`.) But then we have that:

    \sum_i (\sum_j m_{i, j} vol(b_j)) vol(a_j) <= \int_A (\int_B f(x, y) dy) dx

And now we squeeze. This shows that the iterated integral must exist,
and it must be equal to `\int_{AxB} f(x, y) d(x, y)`.

Notice the roles of `A` and `B` could be swapped. And if both the needed
integrals exist, it shows that either order of integration can be used,
equally well.

## When Does Fubini Not Apply?

It's clear that, if the proof will work at all, I need that the double
integral exists. But I also need that `\int_B f(x, y) dy` always exists.

But I wonder: when could that *not* happen?

**TODO**: I want to find a good counter-example that I understand...
