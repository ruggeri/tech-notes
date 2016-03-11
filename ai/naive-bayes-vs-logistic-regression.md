## Naive Bayes

You are given the conditional probabilities `P(y|x=0)` and
`P(y|x=1)`. Then, given a single feature, we have (by Bayes' Theorem):

    P(x=1|y=k)/P(x=0|y=k)=(P(y=k|x=1)P(x=1))/(P(y=k|x=0)P(x=0))

This is equal to:

    P(y=k|x=1)/P(y=k|x=0) * P(x=1)/P(x=0)

Now imagine that `y` is a list of features, all conditionally
independent given the value of `x`. Then this is:

    (P(x=1)/P(x=0)) * \Prod P(y_i=k_i|x=1)/P(y_i=k_i|x=0)

Now, if we take the log of this, we have

    log(P(x=1)/P(x=0)) + \Sum log(P(y_i=k_i|x=1)/P(y_i=k_i|x=0))

This calculates the log odds of `x=1` given `y=k`. The decision
boundary for `x` wrt to this model is `0`.

## Logistic Regression

Logistic regression likewise tries to find `w_i` such that `w_0 + wy >
0` when `x=1`.

So our NB model is a candidate model when performing Logistic
Regression.

## Difference

So what is the difference between LR and NB?

Consider perfectly correlated features. This violates the assumption
of NB. NB would find that `w_i` with identical log odds. This makes
sense given the assumptions of NB.

LR, however, can knock down the weights of the `w_i` (by a factor of
`sqrt(2)`).

If you want to predict the class `X` given the features, LR is more
powerful, because it does not assume that the feature variables are
independent.

We can see the problem solved by LR as *discriminative*, while NB is
*generative*.

Let's consider this. Given a NB model, and observing the class, I can
predict the probability that a feature will be expressed. Likewise,
given a *subset* of the features, I can give an odds ratio for the
class.

On the other hand, given an LR model, the `w_i` *do not* tell me the
probability that a feature will be expressed. Similarly, observing
only a subset of the features, I *cannot* use the LR model to predict
the probability of the class.

That is because the LR model was trained to estimate
`P(x=1|y=k)`. This is a discriminative task. Perhaps one way to think
about this is like so. LR can understand some interactions between the
presence of feature variables, but it still returns `O(n)`
weights. There is necessarily information lost about the correlations
between features, because there are `O(n**2)` pairwise interactions
(to say nothing of even three-way interactions). But LR does not try
to model the interactions; it is only trying to model the underlying
class.
