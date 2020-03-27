Performing "half" of this elimination can be seen to build the `LU`
decomposition. Here, what we do is we start with `IA`. Then, when we
add a row, we do the row operation on `A`, but the opposite operation
on `I`. Here, we *only* try to eliminate in `A` below the
diagonal. This results in transforming `A` to an upper triangular
matrix, while `I` becomes lower triangular.

Note that `A=LU` has `L` with 1s along the diagonal, while `U` has
non-one diagonal. Sometimes we therefore factor to `LDU`. `L` is the
same as before, but rows of `U` are scaled so that diagonal is
one. The scaling is performed by `D`, which is a diagonal matrix with
just the scaling values.

Notice that because we may have needed to do pivots, in order to
decompose any matrix we may have to decompose to `PA=LU` or `PA=LDU`.
