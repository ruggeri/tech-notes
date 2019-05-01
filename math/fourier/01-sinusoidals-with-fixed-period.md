Before we dive into considering the topological closure of a basis of
sinusoidals with any arbitrary period, let's consider a much simpler
world. Let's restrict ourselves to the closure of sinusoidals with
period exactly `T = 2\pi`. That is, functions spanned by the set:

    sin(\theta + \phi) (for all \phi)

**Sine and Cosine Span**

I claim that `sin` and `cos` are a basis for this space. What happens
when you add `alpha cos` and `beta sin` together?

Note that `(alpha, beta)` can be written as `r(cos phi, sin phi)`, for
some `r` and `phi`. Let's presume that `r = 1`.

Then we are asking, what is:

  cos(phi)cos(theta) + sin(phi)sin(theta)

This is the projection of `(cos(theta), sin(theta))` onto `(cos(phi),
sin(phi))`. Basically, you're projecting the `theta` vector onto the
`phi` vector, where the `phi` vector is `e_1` rotated up `phi` radians.
Which is to say: you're computing `cos(theta - phi)`.

This shows that `sin` and `cos` is exactly a basis of the space.

That makes sense. Whenever you have two independent vectors, a weighted
sum is going to effectively be (1) some scaling, and (2) some mixing.
**And the mixing can always be seen as rotations.** (Whoa dude).

Here there is a happy match-up because the basis vectors are themselves
sinusoids. And the rotation of sinusoids is equivalent to phase shifts.

Using simply `sin, cos` as a basis is much nicer than using that
uncountable set with the `\phi`, right?
