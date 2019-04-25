## Motivation

Consider a data matrix `A`, where the columns correspond to variables
and the rows correspond to examples.

Lots of techniques work best if the variables are independent. For
instance, think of training a neural network. When calculating how to
update a weight, we're assuming that the proper update is not affected
by any of the other weight updates we are making simultaneously.

When `A` has correlations, the variables are not even *linearly
independent*. So the least we can try to do is eliminate correlations.

We want to find new features that are "de-correlated." This makes
training a lot easier. We may also find that a lot of the features are
not particularly useful. We can often throw those away.

## Standardizing Variables (PCA)

SVD is a matrix decomposition. It can always be done. But in practical
applications, we may want to do a little pre-processing. First, we want
to mean center our data matrix columns. If we do that, the inner product
will be properly measuring the covariance of the variables.

Next, we probably want to normalize the variance of each column to one.
Otherwise our techniques will think that higher-variance columns have
more "information," when in reality that may just be because of the
units.

## Undoing The Correlation Matrix

If we've mean centered and normalized variance, then the correlation
matrix is exactly `A^T A`. (Correlation is covariance divided by
variances).

Since `A^T A` is symmetric, the spectral theorem says we may write:

    A^T A = V \Sigma^{1/2} \Sigma^{1/2} V^T

(Where `V` is an orthogonal matrix.)

This is basically saying that `\sqrt{\Sigma} V^T` would be a data-matrix
with an equivalent correlation matrix. Now, the way to *remove*
correlations in `A` is the same way to remove correlations in `V^T`.
Set:

    A' := A (V \Sigma^{-1/2})

Multiplying by `V` is supposed to remove the correlation. You can view
the columns of `V` as uncorrelated meta-features. However, when you
re-represent `A` as meta-features `V`, it may be that some of those
meta-features are "more important" in explaining the data in `A`.

For instance: say that the first column of `A` is truly random, whereas
all other columns are just a copy of the first plus a little noise. Then
in the new representation you don't have equal variance anymore. That's
why we divide by `\Sigma^{1/2}`.

Well, the proof is in the math. Let's make sure this works:

    A'^T A'
    = (A V \Sigma^{-1/2})^T (A V \Sigma^{-1/2})
    = (\Sigma^{-1/2} V^T A^T) (A V \Sigma^{-1/2})
    = (\Sigma^{-1/2} V^T) (A^T A) (V \Sigma^{-1/2})
    = (\Sigma^{-1/2} V^T) (V \Sigma V^T) (V \Sigma^{-1/2})
    = I

Bam! Every column has unit variance and no covariance in the new
representation!

## Singular Value Decomposition

Let's call `A'` `U` instead. Then by our own definition:

    U = A (V \Sigma^{-1/2})

Let's write the original matrix `A` in terms of our new matrices. Using
our new representation `U`, we can first expand back the columns by
multiplying by `\Sigma^{1/2}` on the right.

    U \Sigma^{1/2} = AV

Next, we can turn our scaled representations back into the original
representation by multiplying by `V^T`:

    A = U \Sigma^{1/2} V^T

This is the **singular value decomposition**.

(Note: Maybe I should have written the covariance matrix as `\Sigma^2`,
which would basically align with the notation of `\sigma^2` for
variance.)

## TODO

* Feature selection?

## Applications

* PCA, low-rank approximations. Feature compression.
* Collaborative filtering.
* Image compression.
* Matrix factorization.

* In low dimensional space:
    * Clustering and categorization.
    * Use for info retrieval. If you have a term document matrix,
      you can extract meta-features and then match your query in that
      space.
* Note: LSI is a lot like PCA but it doesn't mean normalize, which can
  turn a sparse matrix dense.
* PCA does seem to be basically just SVD (plus mean centering).
