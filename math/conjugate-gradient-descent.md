This is a special technique for solving systems of linear equations
when the matrix is (1) symmetric and (2) semi-definite.

A *positive-definite* matrix is a (1) symmetric matrix with:

    z^TMz > 0

What this means to me is that the matrix `M` maps every vector to a
new vector `Mz` such that when `Mz` is projected onto `z`, this
component along `z` is positive. That is: no `z` is mapped to an
orthogonal vector, nor a vector "against" the original `z`.

Semi-definite allows for mapping to orthogonal vectors.

Not really sure what symmetry is going to give us yet...
