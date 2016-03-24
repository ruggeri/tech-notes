# Inner Product Space

You can prove that given a vector space, and an inner product where:

```
<au,v>=a<u,v> (this may follow from the 2nd property?)
<u1+u2,v>=<u1,v>+<u2,v>
<u,v>=<v,u>\conj
```

And a basis where `<ei,ej>=0` and `<ei,ei>=1` that

```
\Sum <u,ei>ei=u
```

Basically this is just saying that an inner product allows us to
rewrite a vector in terms of a basis, provided:

0. We have a special kind of function,
1. We have a special kind of basis that respects that function.

Note that if we choose a basis `{e_i}`, then if we set `<e_i,e_i>=1`
and `<e_j,e_i>=0`, this defines the rest of the inner product. In a
finite dimensional vector space, this is exactly the traditional dot
product.

You can prove in fact that any inner product is the dot product for
*some* set of vectors! To do this, orthonormalize a basis. The inner
product is the dotproduct wrt this basis. Bam!

It's up to us to choose our basis vectors and *declare* them
orthogonal, this is arbitrary. Once we have them, the inner product is
fully specified. So it's *not* special in any way!

In primitive cave times, before we ever dreamed of the inner product,
we could just pick an arbitrary basis. If you gave me a vector, I
could "invert" it, telling you what linear combination of the basis
this was. I did this by plucking out the appropriate coordinates. I
assume that you gave me the vector in coordinates of my basis, or else
I wouldn't know what vector you're talking about!

All the inner product does is start from our old notion of "plucking
out coordiantes", and extends it. Now we can project vectors onto
other, non-basis vectors! Most of the time this is uninteresting. But
given *another* orthonormal basis, we can use the inner product to
easily "invert" a vector specified in the *original* basis in terms of
the new basis!

Note that this is a very privileged basis! What kinds of bases are so
special like this?? Why have we chosen to extend the original "pluck
the coordinates" concept in a *linear* way? Could there have been
other generalizations of the inner product that would privilege other
kinds of bases?

## Even More!

Many bases will share an inner product. If vectors are given to us in
terms of coordinates of `{e_i}`, we can find the inner product `<u,v>`
easily as the dotproduct of these coordinate vectors.

If a second basis `{e'_i}` is given to us in terms of `{e_i}`, then we
know that `v` can be written in terms of `{e'_i}` by calculating
`<v,e'_i>`. This could theoretically be a pain to calculate, if the
`e'_i` are not orthonormal; that is, if the `e'_i` don't generate the
same inner product. But if they are, this is simple to do in the basis
`{e_i}`, and gives the same calculation, by the above.

So now I must truly ask: what bases share the same inner product? Why
have we chosen to extend the inner product this way?
