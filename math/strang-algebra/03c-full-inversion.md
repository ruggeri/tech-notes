Okay. So we saw previously how to invert a matrix. The way we did this
was like so:

0. Decompose into `LU`, but be computing `L\inv` by performing the
   inverse operations to `I`
0. Now you have want to turn `U` into `I` step by step, performing the
   opposite action on `L\inv`.
0. That is the same as trying to get to reduced row-ecehlon form.
0. If `A` was invertible, then you have built `A\inv`. But if `A` was
   singular, you weren't quite able to turn `U` into `I`. You turned
   it into something that isn't of full-rank.
0. What we can do is apply the matrix we've built up to a vector
   `y`. If the result lies in the columnspace of `R` (the row reduced
   echelon form), then this is the inverse image. If not, then `y` is
   outside the columnspace of `A`.
0. Note that since `R` makes calculation of the nullspace simple. The
   subspace of solutions has rank equal to the rank of the nullspace.
