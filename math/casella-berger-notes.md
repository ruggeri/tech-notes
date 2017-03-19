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
    * These are *unbiased* estimators. That means the mean value of
      the sample statistic is equal to the true statistic.
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
    * We propose a null hypothesis, which we will try to reject. We
      also have a complementary alternative hypothesis.
        * For instance, the null hypothesis may be that a parameter
          `\theta` is in one set, while the alternative is that it is
          in the complementary set.
    * A hypothesis testing procedure is a procedure whereby you either
      choose to reject the null hypothesis, or decide that the data
      does not allow you to reject the null hypothesis.
        * The sample space where the null hypothesis is *rejected* is
          called the *rejection region*, while the space where the
          null hypothesis is *not rejected* is called the *acceptance*
          region.
    * We would like to judge the quality of a hypothesis testing
      procedure.
    * Let us consider the two types of errors:
        * First, assume that `\theta` in fact is in the null
          hypothesis. Then our worry is that we falsely reject
          `\theta`: that is, we worry about the probability that
          `\theta` generates a sample in the rejection region. This is
          called "Type I Error".
        * Next, assume that `\theta` is *not* in the null
          hypothesis. Then our worry is that the sample is *not* in
          the rejection area. That is, we worry about the probability
          that `\theta` does not generate a sample in the rejection
          area.
    * Let us now define `\beta_T(\theta)`, which is the probability
      that given the parameter is truly `\theta`, that it generates a
      sample that the test rejects.
        * We *want* `\beta(\theta)` to be exactly zero for all
          `\theta` in the null hypothesis. That means we never falsely
          reject the null hypothesis.
        * Alternatively, we want `\beta(\theta)` to be exactly one for
          all `\theta` in the alternative hypothesis. That means we
          would never fail to reject the null hypothesis when the null
          hypothesis is in fact false.
    * Notice that the test is telling us whether or not to reject the
      hypothesis. Therefore, the test needs to be parameterized by the
      hypothesis. As we said, a good test for a hypothesis is likely
      to reject when the parameter is not in the null hypothesis, and
      likely to accept otherwise.
    * Now, we define a *level \alpha* test to be one where
      `\beta(\theta) <= \alpha` for all `\theta` in the null
      hypothesis.
        * This means that a level `\alpha` test has a ceiling on the
          probability of making a type I (false rejection) error.
        * It says nothing about type II error (failure to reject).
    * A simple test would say that `\beta(\theta) = 0` for all
      `\theta`. That would be a level `\alpha` test for any `\alpha`.
        * But it is clearly stupid because this would never reject the
          hypothesis when it isn't true.
        * That's the point, we want `\beta(\theta)` to be large for
          anything outside the null hypothesis.
    * We define an *unbiased* test. This says that `\beta` on the null
      hypothesis set is always greater or equal to `\beta` on the
      alternative hypothesis set.
        * This means that the test is at least as likely to reject the
          hypothesis if `\theta` is in the alternative hypothesis, as
          opposed to when `\theta` is in the null hypothesis.
        * This would be even dumber than just having
          `\beta(\theta)=0`. For a biased test, there are null
          hypothesis `\theta` less likely to be rejected than
          alternative hypothesis `\theta`. That is perverse!
    * We want to start considering tests that have high `\beta` for
      members of the alternative hypothesis. Given a class of tests,
      `C`, we say that a test `T` is *uniformly most powerful* if for
      any other test `T'`, then for all `\theta` in the alternative
      hypothesis, `\beta(\theta) >= \beta'(\theta)`.
        * That is, a uniformly most powerful test is one which is
          least likely to fail to reject the null hypothesis when the
          parameter truly lies in the alternative hypothesis.
    * A UMP test amongst the class of level `\alpha` tests represents
      a "best in class" test for that given false-rejection rate
      `\alpha`.
        * Many problems don't have a UMP level `\alpha` test. That
          means that some level `\alpha` tests work better on parts of
          the alternative hypothesis than other tests, and vice versa.
    * The Neyman-Pearson lemma says taht for certain simple hypotheses
      (where both null and alternative hypotheses consist of a single
      value of `\theta`), then the LRT with an appropriate cutoff is a
      UMP test of level `\alpha`.
    * What is the Likelihood Ratio Test? Basically, we compare the
      supremum of the likelihood of the sample over the null
      hypothesis space, and take its ratio with the supremum of the
      likelihood over the entire space. If the ratio is `<c`, then we
      reject the null hypothesis.
        * The logic of this test is clear, but is it any good?
        * The Neyman-Pearson lemma from right above says that for the
          appropriate `c` that gives a desired `\alpha`, this is a UMP
          test, if both hypotheses consist of a single parameter. So
          in this sense, the test is good, in fact: optimal.
    * Another result, by Karlin-Rubin, applies in certain cases for
      *one-sided* tests.
        * The null hypothesis is `\theta <= \theta_0` and the
          alternative hypothesis is `\theta > \theta_0`.
        * Next, the theorem requires that for any two
          `\theta_1>\theta_2`, that the likelihood ratio
          `f_1(x)/f_0(x)` is monotonically non-decreasing in `x`.
        * What that means is that the larger `x` is, the more likely
          `theta_1` is over `theta_0`.
        * In that case we can choose a cutoff `x_0`, where the
          probability that `\theta_0` would generate an `x>x_0` is
          exactly the desired `\alpha`.
        * That means that `\beta(\theta_0)=\alpha`, and we know that
          for any `\theta<\theta_0`, `\beta(\theta)` can only be
          smaller. That means this is a level `\alpha` test.
        * Now we need to show that this is a uniformly most powerful
          test. I'm lazy and don't want to prove that right now.
* Bayesian Hypothesis Testing
    * If you have a prior, you can use the posterior to evaluate the
      probability of both hypotheses; you can accept the one with the
      higher probability. Alternatively, if you want to be safe, you
      can reject the null hypothesis only when the probability of the
      alternative is higher than some threshold.
* First, let's start by mapping all samples into the range
  `[0, 1]`. This lets us talk about an example being "more extreme"
  than another.
    * Obviously, any monotonic transformation of the space would
      preserve the notion of extremity.
    * So we want something better. We want that `p(x_0)` is defined
      such that to `sup_\theta P_\theta(p(x) <= p(x_0)) = p(x_0)`.
    * That means exactly this: that `p(x)` is the probability of
      obtaining a result at least as extreme as this one (for the
      worst theta).
    * From this, we can immediately get a level `\alpha` test. In
      particular, we reject exactly when `p(x) <= \alpha`.
    * Thus, the p-value is useful in allowing the reader to interpret
      the "strength" of the evidence. That is, we can set the false
      rejection rate as low as `p(x)`, and this evidence would still
      indicate rejection of the null hypothesis.
* It is common that the p-value is monotone in `x`, which yields a
  very understandable rejection region.
    * In particular, for any function `W`, you can define `p(x)` to be
      `sup_\theta Pr_\theta(W(X) >= W(x))`.
    * A one-sided value may have `W(x)` be montone in `x`, while a
      two-sided value may have `W(x)` be proportional to the distance
      from some statistic like the mean.
* Hypothesis Testing Summary/Pitfalls:
    * A level alpha test only means that the probability of
      false-rejection given theta is in fact in the null hypothesis is
      less than or equal to alpha.
    * Nothing here speaks to the "probability that the null hypothesis
      is false" or "is true".
    * Note that a sample does not just suggest rejection of the null
      hypothesis merely because it is unlikely under the null
      hypothesis. It only makes sense to take as evidence against the
      null hypothesis if this sample is *even more* unlikely under the
      alternative hypothesis.
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
    * We can design an interval estimator from an acceptance region of
      a level `\alpha` point hypothesis test.
        * Take a `\theta_0`. Let `A(\theta_0)` be the acceptance
          region of a level `\alpha` test for `H_0=\theta_0`. That is,
          let this be the region where we wouldn't reject `\theta_0`.
        * Now, let the confidence interval of `x` be every `\theta`
          where `x \in A(\theta)`. That means, where `x` would not
          cause the rejection of `\theta`.
        * So consider any `\theta`. It generates an `x`. What is the
          probability that `\theta` is in the confidence interval?
          Well, the probability that `\theta` was rejected by the test
          is at most `\alpha`. That means that the probability that
          `x` lies in the acceptance region is at least `1-\alpha`. By
          definition, `\theta` is in the confidence interval when `x`
          is in the acceptance region. And since we just said that `x`
          is in the acceptance region with probability at least
          `1-\alpha`, that means that `\theta` is in the confidence
          interval with that same probability.
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

## Regression

The model is that `E(Y|x)` is linear in `x`. The note that if the
joint distribution is bivariate normal, then this assumption is true.

But then they start discussing residual sum of squares, and they do
the calculus to find those parameters that minimize this.

Then they talk about in what world this is a good estimate. They say,
assume the `x_i` are known and fixed by the experimenter. Assume the
`Y_i` are measured, but that there is some error because of
uncontrolled variables unknown to the experimenter.

Now, at first they don't even assume that the errors are normal! Then,
they show that in a restricted set of estimators, the line that
minimizes RSS actually is the best linear unbiased estimator
(BLUE). But I didn't follow that proof, and I don't know why I care
about BLUE anyway. I believe this theorem is the Gauss-Markov theorem.

Next, they introduce the *conditional normal model*, which is what I
know. The errors are distributed normally. The other model considered
is the *bivariate normal* model, where it is allowed that the
predictors may have error, too.

They first analyze the conditional normal model. They show that the
maximum likelihood estimate is the least squares solution.

**TODO**: The discussion of bivariate normal was just too much math
  for me. And I didn't read their errors in variables stuff.

Source: Casella-Berger
