Bayesian inference is when you assume there is a *prior distribution*
that generates the parameters of your system, which in turn is used to
generate the observed data.

So, given this prior distribution, we can calculate a *posterior
distribution* over the parameters. The way we do so is using *Bayes'
Rule*, which is why we call this *Bayesian Inference*.

The rule is this. Assume the hyperparameters are encoded as
`\alpha`. The parameter is `\theta`. Then the posterior distribution
is:

    Pr(\theta | Data and \alpha)

Given the posterior distribution, we have many choices. We could pick
the `\theta` with the highest density: this is called *maximum a
posteriori* or MAP estimation. This is roughly analogous to a maximum
likelihood estimate, and is exactly equal if the prior is uniform.

Alternatively, we could choose the mean value. Or compute anything
else we desire.

How to compute this distribution? Well, let's use Bayes' rule. This
says the posterior distribution is equal to:

    Pr(\theta | \alpha) * Pr(Data | \theta) / Pr(Data | \alpha)

Note that the denominator is just a normalizing constant. What's
really happening here is that we're re-weighting the prior for
`\theta` based on its probability of producing the data.

## Conjugate Prior

It is common to choose the prior distribution from a family of
distributions where the posterior is another member of that family.

A particular case is the Beta distribution, which is conjugate to the
binomial distribution. Let's find what is conjugate to the binomial
distribution. Let us call this `f` for a moment. We'll calculate the
posterior now. Let's start with the numerator:

    f(p ; \alpha) * (n choose k) p**k (1-p)**(n-k)

Okay. Let's examine the denominator:

    \Int_p (n choose k) p**k (1-p)**(n-k) * f(p ; \alpha)

Let us assume that we can choose `\alpha` such that the prior
distribution is uniform. We won't always choose that, but it kinda
makes sense to desire that the uniform distribution is in the
family. In that case, we have as the denominator:

    \Int_p p**k (1-p)**(n-k)

This function first the form of the *Beta function*, which is defined
as:

    Beta(x, y) = \Int_p p**(x-1) (1-p)**(y-1)

Therefore, the denominator appears to be `Beta(k+1, n-k+1)`.

Let's continue. Note that the `(n choose k)` cancel from numerator and
denominator. Then, we have:

    p**k (1-p)**(n-k) / Beta(k+1, n-k+1)

This is called the *Beta distribution*. Confusingly, we call the
hyperparameters of the Beta distribution `\alpha, \beta`. We see that
the uniform distribution is the Beta distribution with
`\alpha=\beta=1`.

To update our prior to a posterior, given some data, we simply
transition to the Beta distribution parameterized by `\alpha +
num_pos` and `\beta + num_neg`.

One way to interpret the Beta distribution for `\alpha, \beta` is to
see this as having a uniform prior, and then observing `\alpha-1`
positives, `\beta-1` negatives, and then your data. The larger
`\alpha` and `\beta` are, the more "biased" your prior is, making it
harder for your data to pull away from `p=\alpha/(\alpha + \beta)`.

`\alpha` and `\beta` are sometimes called *pseudocounts*, because
they're like positive/negative observations, but they didn't really
happen. Note that you can set the pseudocounts to `<1`, so long as
they stay `>0`. This would bias you *away* from 0.5 and toward
extremes. I'm not really sure what the intuition would be there.

## Multinomial Distribution and Dirichlet Prior

The multinomial generalizes the binomial distribution. It simply
allows for more classes.

This also has a conjugate prior. This is likewise a generalization of
the Beta distribution. Basically, it is:

    \Prod p_i ** \alpha_i / B(\alpha)

What is this? The parameter for a multinomial is a vector `p` where
the `p_i` are in the range `[0,1]` and they sum to `1`.

The Dirichlet is hyperparameterized by `\alpha`. This is a vector of
pseudocounts. The Beta function needs to be generalized to vectors,
which is sorta easy:

    \Int_p \Prod p_i ** \alpha_i

Note that the space `p` is a little weird. Luckily, I don't have to
think about how to calculate this thing.

Again, we can think of the hyperparameter as a vector of pseudocounts.
