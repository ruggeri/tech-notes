He discusses that an orthogonal matrix is its own inverse.

He notes that because orthogonal matrices don't change lengths, they
can be numerically helpful.

He describes Gram-Schmidt, which just successively subtracts out
projections onto the previously considered vectors. You of course can
collect these in a matrix R to put them back in.

The QR decomposition gives us a way to solve least squares. In that
case:

    (A\trans A)\inv = ((QR)\trans QR)\inv = (R\trans Q\trans Q R)\inv
    = (R\trans R)\inv

So:

    (A\trans A)\inv A\trans y = (R\trans R)\inv (QR)\trans) y
    = R\inv R\trans\inv R\trans Q\trans y = R\inv Q\trans y

If you already have `A` in `QR` form this should take `n**2`
time. It's `n**2` to apply `Q\trans y`, and it's easy to invert an
upper triangular matrix.
