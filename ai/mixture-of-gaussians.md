Typically, *k-means* just means doing an iterative process to find
points that are the mean of the points assigned to the cluster. It's
basically doing EM for solving a mixture of gaussians, except (1) the
Gaussian is required to have a symmetric covariance matrix (i.e., its
slope is the same across all dimensions) and (2) deterministic
assignment of a point to clusters.

In mixture-models, you can have asymmetric gaussians (i.e., condition
1 does not hold). And, you can give each point a weight in each
cluster, even if its very small. This would be done proportional to
the likelihood of that point in the cluster. This is smoother than
deterministic assignment.

K-means is often used for quantization.
