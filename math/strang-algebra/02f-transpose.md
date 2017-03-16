**Transposes**

So let's talk about transposes. I say that `A\trans x` maps `x` to its
projection on each of the columns of `A`. That's like almost literally
the definition.

Also note:

    Ax = (x\transA\trans)\trans

This is maybe obvious.

We already showed that for any orthonormal matrix, the inverse is
equal to its transpose. For a matrix with skew, the transpose
`A\trans` does not properly invert `x`.

We say a matrix is **symmetric** if it is equal to its own transpose.

`RR\trans`

For **permutation matrices**, the transpose is also the inverse. (This
is in part because premutation matrices are rotation matrices, albeit
the rotation might have an imaginary component).

**TODO**: I have no idea what symmetry means.
