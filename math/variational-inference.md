So let us say you have a model you want to perform inference in. That
means you know some variables, and want to predict others.

If the variables you observe are always the same, you would probably
want to train a model which is a function approximator for `f(Y|X)`.

However, in the context of medical diagnosis, you may not have run all
possible tests and want to be able to perform inference using
different sets of test results.

This is a common scenario for PGMs.

The problem is that inference can be intractable. For instance, if you
have a Bayesian network and tabular CPDs, then this is a factorization
of a joint probability distribution. It is often fast to generate a
sample; you start at the root, sample these values, and continue. So
long as no variable has a large CPD table (that is; hopefully each
variable depends on few preceding variables), this is tractable.

In the easy cases, the typical approach is to do *belief
propagation*. I won't really explain this algorithm.

But that generates a sample of the unconditional joint distribution:
not the distribution conditioned on the evidence. Now, you can do
*rejection sampling*, and throw away samples that don't fit the
evidence, but that can be very inefficient, especially if the evidence
observed has low probability.

Another approach is MCMC. The big picture is that we generate a series
of a samples, each one generated on the previous sample. It is
supposed to be simple to sample from this distribution. Even though
the samples are not independent, if we design things correctly, the
empirical probabilities of the MCMC samples should estimate the
distribution we are targeting. This is called Markov Chain Monte Carlo
because it is an approximation algorithm (Monte Carlo), and since each
next sample depends on the previous sample, it is a Markovian process.

Let us consider Gibbs sampling, which is a special case of the more
general Metropolis-Hastings method. First, initialize the
variables. This is the first sample. Now: how do we produce the next
sample?

We will produce a new sample `(x_0, ..., x_n)`. The way to do this is
to sample each coordinate individually, in series. First you sample
`x'_0` conditioned on all the prior `(x_1, ..., x_n)`. Then you sample
`x'_1` conditioned on `(x'_0, x_2, ..., x_n)`.

It does not matter in what order you sample the coordinates. In the
beginning, the first sample will be highly dependent on the
initialization value. However, as you continue the process, the Markov
chain will **mix**, and approach a stationary distribution. That
stationary distribution *is* the true distribution. So it is
acceptable to treat these samples as samples of the true distribution
if you take long sequences of them.

Note that the samples have an extremely high degree of
*autocorrelation*, but that is okay for our purposes. It is common to
*thin* the samples, to remove autocorrelated samples. However, that
shouldn't be strictly necessary. Likewise, it is common to *burn in*
the chain so that it can mix. Basically: the worry is that you start
in a very low probability-density part of the sample space. However,
this again is not strictly necessary.

*Variational inference* is yet another approach. Basically, we will
try to approximate our difficult distribution with a simpler
distribution selected from a family. We will perform an iterative
algorithm to choose the "best" distribution from this family.

TODO: I actually going to leave this incomplete as I don't have time
for this right now and am not terribly interested in PGM at this moment.
