So you want to fit a linear model, but you have too many datapoints
and not enough parameters.

Take your entire vector of target variables `y`, and view this as a
vector. We want to find the `\theta` such that `X\theta` minimizes the
squared loss.

(I'm treating the `y` as a column, and `X`'s rows are the predictors
for an example).

So you can see `y\hat` as a function of `\theta`: the function treats
the datapoints as fixed, and theta as the input. Because there are
fewer parameters than datapoints, this is an embeding of a lower
dimensionality space into a higher dimensionality space.

This is exactly the problem we've seen before. We can simply find:

    theta\hat = (X\trans X)\inv X\trans y
    y\hat = X theta\hat

In thw next section I will try to give more intuition for this
result. However, this result is very clear from linear algebra.
