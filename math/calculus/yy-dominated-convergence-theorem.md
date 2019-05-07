Let `f_n` converge to `f` *pointwise*. Let us ask many questions.

**Must The Limit Of Integrals Converge?**

Assume that `\int f_n d\mu` exists for every `f_n`. Must `\lim \int f_n
d\mu` converge?

No. Consider `f_n` where `f_n(x) = 0` for `x > 1/n` but `f_n(x) = (-1)^n
n` for `x <= 1/n`. The integral keeps flipping from -1 to +1.

Note that `f_n` converges to exactly zero, so that `\int \lim f_n d\mu`
does exist (and equals zero). So the failure for the limit of integrals
to converge says nothing about integrating `f`.

**If The Limit Of Integrals Converges, Must The Integral Of Limit?**

Assume that `\lim \int f_n d\mu` does converge. Must `\int \lim f_n
d\mu` also converge?

Consider `f_n` where `f_n(x) = 1/x` if `|x| > 1/n`. Else, `f_n(x) = 0`.

Consider integrating on `[-1, +1]`. This always integrates out to zero.

But note that `f_n` converges pointwise to `f(x) = 1/x`. But that is
*not* integrable.

**Assume Both Integrals Exist. Are They Equal?**

Again, no. An example is `f_n(x) = n` for `x < 1/n`. Else `f_n(x) = 0`.

The clearly the integral of `f_n` is always exactly 1.0. But `f(x) = 0`,
so the integral is zero.

**Would Continuity Help?**

No. We can make all these examples continuous. These are all piecewise
continuous examples, and you can always smooth those out...

**Would Uniform Continuity Help?**

Yes. If `lim f_n` converges to `f` *uniformly*, and if `lim \int f_n
d\mu` exists, then we can show that this implies that `\int f d\mu`
exists. Moreover, that that the limit of the integrals and the integral
of the limit is the same.

(This assumes we are integrating over a finite volume area `A`.)

The basic idea is to consider that we can restrict `n` so that `f_n - \eps
<= f <= f_n + \eps`.

Then any lower sum of `f` is no more than `\eps vol(A)` less than the
corresponding lower sum of `f_n`. But then we can further restrict `f_n`
to be as close to `\lim \int_A f_n d\mu` as we like. We're showing that
the supremum of lower sums of `f` can be no less than `\lim \int_A f_n
d\mu`.

The same argument applies to upper sums, so we have squeezed ourselves
and established that `\int f d\mu` exists and is equal to `\lim \int_A
f_n d\mu`.

**Is Uniform Continuity Required?**

No. Consider `f_n(x) = 1` if `x < 1\n`. Else `f_n(x) = 0`.

This does *not* converge uniformly to `f(x) = 0`, but at least `f_n`
doesn't move away from `f` to "escape."

Which leads to the next question...

**Is Boundedness By A Constant Required?**

No.

Consider a function `f_n` where there are `n` bumps at the first `n`
positive integers. Each bump at `i` has width `1/2^{2i}` and height
`2^i`.

Then `\lim \int f_n d\mu = 2.0`. This is exactly `\int f d\mu` (which is
just `f` with all the bumps). Interesting note: the `f_n` don't converge
uniformly to the `f`. So you can be unbounded *and* not uniformly
converge!

**Is Boundedness By An Integrable Function Required?**

We are now getting toward the *dominated convergence theorem*. This
theorem basically says that if measurable functions `f_n` converge
pointwise to `f`, and if `\lim \int f_n d\mu` exists, AND that all `f_n`
are dominated by an integrable function `g`, then `\int f d\mu` exists
and is equal to the limit of integrals.

In the example above, the `f_n` are dominated by `f`, since they
converge below.

Here's an example though where the `f_n` are bounded by no integrable
function, yet everything is fine.

Let `f_n` be the function where if `x > 1/n`, then `f_n(x) = 0`. But, if
`|x| < 1/n`, we set `f_n(x) = sgn(x) n`. Note that `f_n` converges
pointwise to the zero, which has zero integral over `[-1, 1]`. Also,
`\int f_n d\mu` is always zero, too.

But note that `sup f_n(x) = \floor{1/x}` on `[0, 1]`, and the opposite
holds for `[-1, 0]`. That means any function that bounds all `f_n` must
also bound `g(x) = 1/x` on `[-1, 1]`. But no function can bound that and
be integrable!

**Proof Of DCT**

We now prove the DCT using *Fatou's lemma*. We will prove Fatou's lemma
elsewhere.

First, note that when we say `f_n` is Lebesgue integrable, we typically
mean that it has finite integral. That's a little odd, but it is
assumed.

If the `f_n` are measurable, and if they are dominated by an integrable
function `g`, it can be shown that the `f_n` are integrable. Likewise,
it can be shown that `f` must also be dominated by `g`, and that `f` is
also measurable, and thus `f` is also integrable.

Now that we know those exist, we may wonder if the limit of `\int f_n
d\mu` exists. But let's not assume that yet. Let's simply first
consider:

    |\int f_n d\mu - \int f d\mu| = |\int f_n - f d\mu|

This is by linearity of Lebesgue integration. Next, we can say:

    |\int f_n - f d\mu|
    <= |\int |f_n - f| d\mu|
    = \int |f_n - f| d\mu

This is true by monotonicity of Lebesgue integration. Note that for a
function to be considered Lebesgue integrable, it must integrate to a
finite value on both its negative and positive parts. That ensures that
`\int |f_n - f| d\mu` exists.

Now, we do not know if `\lim |\int f_n - f d\mu|` exists. But we do know
that `\limsup |\int f_n - f d\mu|` does exist (it always does).
`\limsup` means `\lim_{n \to \infty} \sup_{m >= n} ...`.

We don't know if the `\limsup` is finite, of course. In fact what we
really need is that it not merely be finite. We want that it must be
equal to zero.

Fatou's lemma gives us a way to ensure this. It says: consider a
sequence `h_n` of measurable, *non-negative* functions. Then let `h(x)`
be the `limsup` of `h_n(x). Then

    \limsup \int h_n d\mu <= \int h d\mu

Using Fatou's lemma, we may say that `h_n(x) = |f_n(x) - f(x)|`. Then
`h(x) = 0` (by definition of `f`). That means that

    \limsup \int h_n d\mu <= \int 0 d\mu = 0

We can change the `\limsup` into simply a `\lim`, because the `h_n` are
always non-negative, and the sup is zero. Which is to say:

    \lim \int |f_n - f| d\mu = 0

But since the inner integral is always greater or equal to `|\int f_n -
f d\mu|`, we have also

    \lim |\int f_n - f d\mu| = |(\lim \int f_n d\mu) - \int f d\mu| = 0

But that means, of course, that

    \lim \int f_n d\mu = \int f d\mu

And that is precisely what we want! We've proven that (a) the limit
converges and (b) it converges to the correct, desired value!
