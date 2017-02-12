## The Story of Linear Regression

We know by now that linear regression is premised on an assumption
that the output is linear plus some Gaussian noise of mean zero. In
that case, solving the linear regression with the squared loss
function means that you end up with the MLE of the parameters.

Note: it doesn't matter what the variance of the noise is. The MLE is
the same regardless.

This setup describes how samples from `Y` are generated given a sample
of `X`.

I believe that Hinton has already described how if you do L2
regularization, this is like putting a Gaussian prior on the weights
of the system. In that case, linear regression solves for the MAP
estimate of these parameters.

## The Story of Logistic Regression

Here we go. We want to describe a model by which samples from `Y` are
generated from samples of `X`.

Given `X`, there is some probability that `Y` is a one or is a
zero. This is a Bernoulli trial. Therefore, it suffices to find the
probability of success, of equivalently, the odds ratio. So the
question is how is `X` linked to the probability of success in the
Bernoulli trial.

Note that with this setup, since we use `X` to compute a parameter for
a Bernoulli trial, we don't need any notion of noise, per se.

The model we'll use assumes that the odds ratio is linear in `X`. This
is equivalent of saying that the probability is a logistic transform
of a linear function of `X`. I show this like so:

    log(p/1-p) = z
    p/1-p = e^z
    p = e^z - pe^z
    (1+e^z)p = e^z
    p = e^z/(1+e^z)
    p = 1 / (e^-z + 1) << logistic function

We see here that each feature then adds to the log odds ratio. This is
a lot like Naive Bayes.

Now, we can ask what error function is linked to the maximum
likelihood estimate. So the likelihood is:

    \Prod_{x, y} y\sigma(Ax) + (1 - y)(1 - \sigma(Ax))
    (via log transform) =>
    \Sum_{x, y} log(y\sigma(Ax) + (1 - y)(1 - \sigma(Ax)))

So this looks ugly to take the log of, but note that `y` is either 0
or 1. Therefore, this is equal to:

    -ylog(\sigma(Ax)) + -(1 - y)log(1 - \sigma(Ax))

Note that I multiplied by -1, so this is now a *minimization* problem.

Look at that! This is the typical cross-entropy error function. So, if
you minimize the cross-entropy error, this is the same as maximizing
the likelihood.

This basically makes sense. If the `\sigma(Ax)` is your best guess at
the probability of `Y`, then the best encoding has you use
`log(\sigma(Ax))` bits for a positive, and `log(1 - \sigma(Ax))` for a
negative.

So the maximum likelihood estimate is exactly the parameter setting
that would have minimized the description length of the data.
