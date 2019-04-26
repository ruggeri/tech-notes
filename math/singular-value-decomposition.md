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

This is basically saying that `\sqrt{\Sigma}^{1/2} V^T` would be a
data-matrix with an equivalent correlation matrix. (**TODO**: This
sounds weird. Clearly `\sqrt{\Sigma}^{1/2} V^T` doesn't have the same
number of examples as `A`. Is each row of `\sqrt{\Sigma}^{1/2} V^T`
supposed to be an "example" in some sense?).

Now, the way to *remove* correlations in `A` is the same way to remove
correlations in `\sqrt{\Sigma}^{1/2} V^T`. Set:

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

## Uses Of SVD/PCA

**Whitening of Data Matrix**

First, we note that PCA uses the singular value decomposition (which
applies for any matrix), but is based in a statistical theory. For
instance, that is why we mean center and unit normalize the columns of
the data matrix first. This is so that `A^T A` is properly the
correlation matrix.

First, PCA gives a "whitened" version of the data which is much easier
to learn on. This is by removing correlations, but I also expect it
helps to normalize the de-correlated features. Otherwise, if you are
doing any kind of regularization, you'll have a preference for using
those features that explain a lot of the variation in the dataset (and
are thus already scaled bigger).

**Feature Selection**

Of course, PCA is useful for doing feature selection and low-rank
approximations. Any feature that is significantly predicted by other
variables plus a small amount of normally distributed "noise" (possible
"signal") will tend to be eliminated and have that "noise" dropped.

One should be a little careful though. If the remaining "noise" is
actually the important signal, PCA isn't going to detect. For instance,
imagine a data-set where column one is some irrelevant variable
(variance 1.0), and column two is a copy of that irrelevant variable,
plus a tiny amount of noise. Then if PCA is forced to construct a single
feature, it will want the first feature.

Basically: PCA feature selection isn't being informed by your problem.

An example of feature selection is the "eigenface" approach. Note that
you can use PCA to do compression (minimizes squared loss).

**Clustering**

Say you want to do nearest-neighbor style clustering. You're doing this
with the L2 norm in Euclidean space. Then if a variable is copied a
bunch of times, it becomes more and more important for the purpose of
calculating distance.

By doing PCA and dropping those meta-features which capture little
additional variance, you are reducing the "double counting" that was
previously being performed. Note that I think it's important to
normalize meta-feature columns to unit variance (after having used
variance to drop unimportant meta-features), so that you don't retain
the bias.

An example is document classification, where you have documents and your
features are the presence of certain words. In document categories that
have lots of synonyms, you'd be over-emphasizing the importance of those
categories.

**Querying**

If you cluster documents, you can also find those which are closest to a
particularly query. For instance, if someone searches with terms X, Y,
and Z, you can transform that into the latent feature space, and then do
nearest-neighbor in that space.

**Collaborative Filtering**

You can do PCA for recommendation. Basically, your `U` is a matrix where
rows are users, and columns are "personality traits." The columns of the
matrix `V^T` describes the "ideal" set of personality traits associated
with each product.

To predict what a user `u` will think of a product `v^T` , we may take
`u v^T`. But this will perfectly reconstruct the observation in the data
matrix `A`.

*But*, if we start dropping low-value features, the prediction will no
longer be perfect. Effectively, we are suspecting that those low-value
features are noise related to simply whether or not they have
experienced that product, not whether they like the product.

I may note: there is an *alternative* technique called *non-negative
matrix factorization*. It is similar to PCA in that it searches for a
latent representation of the users and a representation of the products.
Like PCA it tries to identify good products by multiplying `u v^T`.

The difference is that NNMF constrains the matrix to use only
*non-negative entries*. The columns of `v^T` are likewise not
constrained to be orthogonal. The learning is done by gradient descent.

Basically, NNMF means that the features are only (positively) additively
combined. If you're searching for latent features for faces, eigenfaces
means you will "subtract" a face, but NNMF only lets you "add" faces. It
is hoped that NNMF creates more interpretable features.

I assume that, for the same `k`, PCA will minimize squared loss better
than NNMF, because that is what PCA is the solution for. I'm not sure
when NNMF is a better choice. NNMF should tend toward *sparse
representations* of users; a feature that is negatively correlated with
the user's original representation will simply not be used in the user's
re-representation.
