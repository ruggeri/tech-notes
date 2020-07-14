Denote the solution as `\omega` (a basis vector).

Denote the uniform superposition `H|0>_n` as `s`.

Let's operate in the `\omega`, `s` plane. Two vectors determine a plane
so this exists.

Denote `s'` to be `s` with the `\omega` part nulled out:

    s' := s - <\omega|s> s

Remember, this is just one coordinate of `s` set to zero. It's the
coordinate that is popped in `\omega`. Of course, `s'` is perpendicular
to `\omega`.

Let's call the angle between `s` and `s'` `\theta/2`.

Okay, let's run a Grover step. We're starting with `s`, so flipping the
`\omega` part really just means reflecting `s` over `s'`. That is a
rotation of `\theta` degrees.

Now, we go to the next part. We're supposed to use `H` to blow
apart/recombine `s - 2\omega`, and then flip the first coordinate here,
and then recombine again using `H`. There's a final flip again. Anyway,
you can look at the explanation in 09-grovers-algorithm.

The point is, `s - 2\omega` can be broken into two parts: the part
parallel to `s` and the part that is perpendicular.

The part that is parallel to `s` basically goes through a no-op. It's
"recombined" back to `|1>`, flipped, then sent back to `-s`, then
flipped again back to `s`. Great.

The part that is perpendicular has *no* `s` component. Therefore
applying `H` yields no `|1>` component. Therefore flipping does nothing.
So we simply apply `H` again (a no-op). And here's the final step, we
now *do* negate.

So we negate the perpendicular component, but keep the parallel
component. That's a reflection. What we've just done is flip over `s -
2\omega` over `s`.

What has that done? We've rotated `\theta` degrees toward `\omega`!

**Step 2**

Every step of Grover's does a rotation by `\theta`. Think why. If the
current state is at an angle of `\alpha` above `s'`, then it will be
reflected over to an angle of `\alpha` below `s'`.

Now it's time to reflect over `s`. What's our angle with `s`? It's
`\alpha + \theta/2`. So we're going to go `\alpha + \theta/2` above `s`.

But what's our final angle? It's `\alpha + \theta/2` plus the angle of
`s` to `s'`, which is `\theta/2`. For a total of `\alpha + \theta`.

So we're seeing each step does a constant rotation.

**How many rotations?**

Each time we rotate by `\theta` (starting at `s` which is at an angle of
`\theta/2` to `s'`). Our ideal is when

    (1/2 + r)\theta = \pi/4
    r = \pi/(2\theta) - 1/2

So what is `\theta`? Well, recall that `\theta/2` is the angle between
`s` and `s'`. The height of that triangle is `1/sqrt(2^n)`. Therefore we
have:

    sin \theta/2 = 1/sqrt(2^n)

Provided that `\theta/2` is small enough, we may approximate `sin
\theta/2 = \theta/2`. In which case we have:

    \theta = 2/sqrt(2^n)

Which gives us, finally,

    r = (\pi/4) (sqrt(2^n)) - 1/2

Again, this shows that you can go too far! Too many rotations will start
decreasing the probability you get the correct answer!

Which is to say that `r = O(2^{n/2})`. And that is where we get our
quadratic speedup from!
