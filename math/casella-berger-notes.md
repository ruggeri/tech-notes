* Axiomatic definition of probability vs interpretations
* Discrete and Continuous variables
* Probability density function, cumulative distribution function
    * NB: not all distributions can be represented by pdfs if single
      values have non-zero mass.
* Conditional probability, mutual independence
* Bayes rule
* Binomial Distribution
* Expected value
    * Doesn't always exist: see Cauchy random variable. Not necessary
      to say?
    * Minimizes the expected value of `(X-E[X])**2`. Why do I care?
* Variance
    * Expected square error in estimating `X` with `E[X]`.
    * Square root is the standard deviation.
    * Called the "second moment". Does that matter?
* Moment generating functions: are these important?
    * I doubt it. I don't understand what these are.
    * They seem to characterize a distribution, but not in a way that
      I understand to be useful...
* Normal Distribution
* Joint distribution, marginal distribution
* Covariance and Correlation
* Random Sample
    * Expectation of the sample mean is the population mean.
    * The variance in the sample mean is the population variance
      divided by the size of the sample.
    * The expected sample variance is equal to the population variance.
        * Note the definition of the sample variance, however, is
          divided by `n-1`.
    * These are *unbiased* estimators.
* Weak and strong law of large numbers
* Central Limit Theorem
* Sufficient statistics
* Estimators
    * MLE
        * Problems with numerical stability from an iterative
          procedure to calculate.
        * Small changes in the data can effect big changes in the MLE,
          which make its use suspect.
    * Bayes Estimator
        * Prior vs Posterior.
        * Binomial example.
* Expectation Maximization in presence of missing data or unobserved
  variables.
* Evaluating Estimators
    * Typically want to minimize MSE of our estimate of the parameter
      theta.
    * It turns out that MSE of the parameter theta is equal to the
      squared bias of the estimate plus the variance in the estimator.
        * Both the bias and variance are defined for a specific theta;
          it depends on the value of theta.
        * Therefore, there may be no "best" estimator; for instance,
          the estimator `17` is best exactly when theta is equal to
          17.
    * Thus, we generally focus on *unbiased* estimators. This is a
      much more specific class. In this case, MSE tells us to prefer
      the estimator with smaller variance, because in this case MSE is
      equal to the variance!
    * The best unbiased estimator (wrt MSE) is called the *uniform
      minimum variance unbiased estimator*, or UMVU estimator.
* Sample mean and sample variance are UMVUs for normal distribution.
    * I don't see where they show that!
    * You can improve on the MSE by using the biased sample variance,
      BTW, but that isn't unbiased anymore (duh).
    * Note that sample mean is not always UMVU; in a uniform
      distribution of unknown upper and lower bounds, UMVU for mean is
      the mid-range (average of sample max and min).
* Loss function
    * We're using MSE, which is nice, but why?
    * We need to define a loss function, like absolute or squared
      loss.
        * This reflects that the action when we know theta will be
          wrong, and the more wrong theta is, the more wrong our
          action will be, and the higher the real world penalty.
    * Also: it's not clear that taking the mean would be the right
      choice either.
    * The *risk function* is a function of theta, the chosen loss
      function, and a proposed esitmator; it is the expectation of the
      loss encurred by using the estimator. So it would be MSE for the
      given theta.
    * They say that when we do decision theory, we would probably not
      limit ourselves to unbiased estimators. We don't care about
      bias, if our loss would be lower.
    * But then, as we've said before, there typically isn't a dominant
      estimator.
* Bayes risk is *not* a function of theta, since it integrates over
  all theta, weighting the risk of an estimator. Whereas the risk
  measures the expected loss for a given theta, the Bayes risk is the
  expected loss without knowing what the parameter is (but of course
  knowing a distribution of parameter values).
    * It turns out that this implies the best decision rule is that
      which minimizes the expected loss *relative to the posterior*.
    * That means that the rule only depends on the posterior; it
      doesn't matter what theta *really* is; it only matters our
      perception of `\pi(theta|x)`.
    * You can work out easily that the estimator that minimizes the
      Bayes risk for the squared loss function is the posterior mean.
    * Likewise, for the absolute loss function we want the posterior
      median.
* Assumptions:
    * UMVU says that you really want something unbiased, and that you
      want to minimize the variance. It's not clear why you would
      require unbiasedness. And variance is only one measure of
      dispersion (though all else equal this seems desirable to
      minimize).
    * Your choice of loss function is clearly a decision.
    * But then the risk function decides that what you really hate is
      the expectation of the loss function; that isn't necessarily
      true.
    * Risk doesn't typically gives us a single best estimator anyway.
    * With Bayes risk, we double-down and say that we want to minimize
      the expected risk. That seems appropriate if we've already
      decided we care about risk anyway.
* Hypothesis testing
    * Likelihood ratio test; compare the max likelihood of the null
      hypothesis to the max likelihood over all of Theta (the
      parameter space). If this is `<c` (where `c` is a constant
      between zero and one), then reject the null hypothesis.
    * If you have a prior, you can use the posterior to evaluate the
      probability of both hypotheses; you can accept the one with the
      higher probability. Alternatively, if you want to be safe, you
      can reject the null hypothesis only when the probability of the
      alternative is higher than some threshold.
    * To evaluate, the power function `\beta(\theta)` is the
      probability when the parameter is `\theta` that the null
      hypothesis will be rejected.
        * Ideally this is zero when `\theta` is in the null hypothesis
          region, and one elsewhere.
    * The power function will be used to measure the performance of a
      hypothesis test.
        * You may wish to put a min false positive probability, and a
          max false negative probability. This would need to be true
          for any setting of `\theta`.
        * This may not be possible for a given sample size.
    * A *level \alpha test* means that the probability of incorrectly
      rejecting the null hypothesis is always less than `\alpha`. This
      means that for every `\theta \in \Theta_0` (i.e., for every null
      hypothesis theta), the probability of incorrectly rejecting the
      null hypothesis is less than `\alpha`.
        * This shows a focus on not falsely rejecting the null
          hypothesis.
        * BTW, `\alpha` is your "signifcance level".
    * A test is called *unbiased* if for every `\theta` in the
      alternative hypothesis, `\beta(\theta)` is greater than any
      `\beta(\theta')` for `\theta'` in the null hypothesis.
        * That is, it's stupid to say you're avoiding false positives
          if don't even detect true positives!
    * A *uniformly most powerful test* is one which has greater
      `\beta(\theta)` for all `\theta` in the alternative hypothesis,
      when compared to other tests in the class.
        * That is: the test always has the highest true positive rate
          in the class, no matter the parameter.
    * A UMP test amongst the class of level `\alpha` tests is best in
      class for tests with that false positive rate.
        * For many problems, a UMP does not exist.
    * The Neyman-Pearson lemma says that for *simple* hypotheses
      (where both null and alternative hypotheses consist of one
      `\theta` each), the LRT with an appropriate cutoff is a UMP test
      of level `\alpha`.
    * Karlin-Rubin shows one extension. If for any pair of `\theta_1 >
      \theta_2`, the likelihood ratio of `\theta_1` versus `\theta_2`
      is non-decreasing as the observation `x` increases, then
      Karlin-Rubin gives a way to construct a UMP test of level
      `\alpha`.
        * This basically says that, for one-sided hypotheses, we can
          construct a simple UMP level.
    * However, for many hypotheses no UMP test will exist.
* p-value: A p-value is any statistic where for any `\theta` in the null
  hypothesis, and any `\alpha`, `Pr_\theta(p(x) <= \alpha) <= \alpha`.
    * Given such a p-value it is easy to give a level `\alpha` test by
      rejecting the null hypothesis iff `p(x) <= \alpha`.
    * Reporting a p-value imparts more than setting a threshold
      `\alpha`, running a level `\alpha` test, and reporting
      acceptance or rejction.
    * The p-value lets the reader bring their own conception of what
      `\alpha` should be, and answer whether the hypothesis would have
      been accepted using the level `\alpha` test.
    * You get to pick what "more extreme" means.
* Careful!
    * A low p-value does not suggest anything about the probability
      that the null hypothesis is true. That makes no sense in
      frequentist statistics. It only means that the observation was
      unlikely for any \theta in the null hypothesis.
    * In fact, if you really do have prior beliefs on the \theta, it
      may still be that the null hypothesis is quite likely, either
      because the alternative hypothesis has very low probability to
      begin with, or because the observation is about equally unlikely
      under both hypotheses.
* Decision Theory for Hypotheses
    * Zero-One loss is common. In that case the risk for `\theta` in
      the null hypothesis is `\beta(\theta)`; likewise for `\theta` in
      the alternative hypothesis is `1-\beta(\theta)`.
    * You can generalize zero-one loss by scaling these.
    * Can also have loss functions which assign more or less penalty
      depending on the value of `\theta`. For instance, consider a
      hypothesis that cancer risk is > x%. If you reject incorrectly,
      the higher the probability of cancer the worse!
* Interval Estimation
    * An interval estimator takes a sample and returns a range of
      theta values.
    * The theta values should be those for which the result is not too
      unlikely. Theta outside that range are rejected.
    * The *coverage probability* for a given theta and interval
      estimator is the probability that theta will lie in the interval.
        * We want this to be high, since otherwise, for this theta, we
          will tend to inaccurately reject it.
    * The *confidence coefficient* for an estimator is the infimum of
      coverage probabilities.
        * The higher this is, the lower the worst false rejection rate
          is.
    * This corresponds to hypothesis testing.
        * Take a level alpha test. Consider a sample `x`. Define the
          interval estimator to be the region in which the test would
          not reject the null hypothesis.
        * Consider a theta. Consider any sample from theta. Because we
          are using a level alpha test, we should reject they
          hypothesis with probability at most alpha.
        * Therefore, the probability that theta is in the confidence
          interval is 1-alpha.
    * You can of course do a Bayesian version, which are called
      *credible sets*. This says the parameter has a 90% chance of
      being in the set. That is of course quite different from what
      the frequentist says.
    * There is a measure of "false coverage", which is the the
      probability for a false parameter `theta'` to be in the
      confidence interval when the true parameter is `theta`.
        * This is a function of theta and `theta'`
        * A uniformly most accurate confidence interval minimizes this
          for every pair.
        * This corresponds to the UMP idea.
        * Likewise, there is a corresponding idea of a confidence
          interval being *unbiased*.
        * A 1-\alpha confidence interval is unbiased if, when the true
          parameter is theta, the probability that `\theta'` would be
          in the confidence interval is less than `1-\alpha`.
            * That is, the probability of false coverage is never more
              than the minimum probability of *true* coverage.
            * This follows since `Pr_\theta(\theta \in
              C(X))>=1-\alpha`.
    * There is a theorem that a UMA unbiased confidence interval will
      always be the shortest, under certain conditions.
    * They mention that finding the smallest confidence set in the
      bayes situation is simple; you simply take all points where the
      posterior density is `>z`, some constant, such that the
      probability of the parameter outside the interval is
      appropriately low.
    * Decision Theory time:
        * Loss function is a function of `\theta`. First needs to
          account for probability that theta is not in the interval.
        * Also needs to account for volume of the interval.
        * We can weight these and average them. The risk is the
          expected value.
        * Now you can use the risk function to choose the confidence
          interval that minimizes risk even for the worst theta.
        * Often this amounts to twiddling your requirement of the
          confidence coefficient of the interval.
        * It all goes back to hypothesis testing, it seems to me...
* Asymptotics
    * Consistent estimator converges to the right answer, for every
      theta.
    * MLE is a consistent estimator; it will converge.
    * Fisher Information
        * Cramer-Rao says that inverse of Fisher information is a
          lower bound on the variance of an unbiased estimator.
        * If bound is obtained, this is of course a UMVUE. But
          sometimes no estimator achieves this bound. This may occur
          even when a UMVUE exists!
        * An estimator achieving this bound is called *efficient*.
        * An estimator that approaches this bound is asymptotically
          efficient.
    * TODO: WHAT THE FUCK IS FISHER INFORMATION?!
    * MLE is asymptotically efficient.
    * Robustness
        * Generally: what if our model is slightly wrong? We still
          want to do okay. We'll probably give up some optimality if
          we've got everything right for our model.
        * Sometimes you actually have a delta-contamination: you have
          a `1-\delta` chance of drawing from your model distribution,
          and a `\delta` chance of some other distribution. For small
          `\delta` you might be okay, if your estimator is robust.
        * Another example: what if one datapoint is really wrong? For
          instance, what if you increase one sample by 100x?
    * A more robust estimator typically trades off efficiency. The
      amount of the tradeoff in the limit is the *asymptotic relative
      efficiency*; this is the ratio of the efficiencies (which is
      just the inverse of the variance).
    * The median is very robust to outliers. One solution is to mix a
      loss function that is quadratic up to a point, then switches
      over to linear. A minimizer of this estimator is called a Huber
      estimator.
    * I think this book offers just a limited discussion on
      robustness...
    * They talk about how to do a LRT test with a distribution that is
      hard to analyze.
        * How do you find a threshold to acheive a level alpha test?
        * They say you can appeal to asymptotics.
    * They show how, if you show that the error of an estimator (they
      suggest a MLE) in the limit is normally distributed, then you
      can use that to calculate the error.
        * In the limit, you might expect rejection error to go to zero.

Source: Casella-Berger
