So you want to fit a linear model, but you have too many datapoints
and not enough parameters.

Take your entire vector of target variabels `y`, and view this as a
vector. We want to find the `\theta` such that `X\theta` minimizes the
squared loss.

(I'm treating the `y` as a column, and `X`'s rows are the predictors
for an example).

So you can see `y\hat` as a function of `\theta`, it treats the
datapoints as fixed, and theta as the input. Because there are fewer
parameters than datapoints, this is an embeding of a lower
dimensionality space into a higher dimensionality space. We want the
`y\hat` that minimizes the squared loss, which means exactly that we
want to project `y` onto the image of `X\theta`. In fact, what we want
most of all is the corresponding `\theta_0`.

From the prior work, we know that we can solve this with `\theta_0 =
(X\trans X)\inv X\trans y`.

This follows from the work we did before, we just have to view `\theta
-> y\hat` as a linear transformation defined by `X`. But let's try to
gain *even more* intuition.

Let's say that all `x_i` were orthonormal. Then the problem would be
exactly solved by saying "the gradient in direction `x_i` is exactly
`y_i`. But we can't have the `x_i` orthogonal because there are too
many of them.

**What is X\trans X Represent?**

I made this part up myself.

A good question is what is `X\trans X`. Then `X\trans X` is the matrix
of inner products of the `x_i` across the examples with the
`x_j`. This tells us how many units of `x_j` to predict for each unit
of `x_i` that we see. This is just another meaning of the projection
operation, and is the one-dimensional version of what we're trying to
do overall.

So this "mutual-information" (speaking loosely) of `x_i` and `x_j` is
exactly what we need to back-out. A simple model that projects `y`
onto `x_i` will double-count effects that are captured by other
variables. Thus we need to remove that with this matrix `X\trans X`.

Another note. This matrix `X\trans X` is like a mixing matrix that
mixes an orthogonal basis together. By inverting this matrix we can
undo the mixing. This is exactly what needs to be performed on
`X\trans Y`.

**TODO**: I think I could make this argument stronger if I knew more
about covariance. I think `X\trans X` might be related to the
empirical covariance. I am even more sure of that when I look at the
wiki page on estimation of covariance matrix.

**TODO**: I think this section is well intended: it is good to have an
intuitive understanding of what this projection of the data onto
itself means. But this isn't a comprehensive understanding yet; I
haven't really truly internalized this, or expressed this properly. I
should revisit this in time.

Finally, Strang notes that you can fit any kind of model, not just a
line, this way, so long as it is linear in a transform of each of the
predictor variables.

