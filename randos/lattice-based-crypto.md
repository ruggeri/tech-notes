- A lattice is a discrete subgroup of `R^n`. Typically it is a discrete
  subgroup of `Z^n`. Note that a full rank lattice may still be a proper
  subgroup. Consider the set of basis vectors `{ 2 e_i }`.
- The same lattice can have several bases. For instance consider sign
  flips of basis vectors.
- `\lambda_1(L)` is the min of the `\norm{\vv}` for `\vv \in L`.
  Equivalently, you can write that it is `\min \norm{\vv - \vw}` for any
  `\vw \ne \vw`. It is the minimum distance between two lattice vectors.
- They also write `\lambda_i(L)`. This means: take `i` linearly
  independent vectors. You want the vectors in your set to all be small.
  We'll consider the _max_; the worst length vector in the set.
  `\lambda_i(L)` is the minimum value. That is:
  - `\lambda_i(L) = \min \max \setof{ \vv_1, \ldots, \vv_i : all independent}`.
  - Note this generalizes `\lambda_1`.
- One thing to note: I believe that you can have a basis of `\Z^n` which
  is `2 e_1, \ldots, 2e_n, \parens{1, \ldots, 1}`. This is `n+1`
  non-linearly independent vectors (if you allow real weights), but none
  is an integer linear combination of the other.
- SVP: shortest vector problem. You want to find the non-zero vector
  that minimizes `\lambda_1`. There is an algo that runs in time
  `2^{O(n)}`, where `n` is the size of the latice.
  - If you are okay with finding a vector that is at most `2^n` times
    the minimum length `\lambda_1`, then there is the LLL algo that runs
    in polytime.
  - There is a statement that maybe no quantum algo has an exponential
    runtime boost? At least not known.
  - It looks like a gap of `n` will give us useful crypto applications.
- An easier problem is to decide between `\lambda_1 <= \gamma` versus
  `\lambda_1 > \gamma`. Of course, an instance of the SVP problem with
  gap `\gamma` would solve this. This is called GapSVP.

Source: Lattice Based Cryptography for Beginners

https://eprint.iacr.org/2015/938.pdf
https://cims.nyu.edu/~regev/papers/pqc.pdf
https://www.math.u-bordeaux.fr/~ybilu/algant/documents/theses/BERGAMI.pdf
