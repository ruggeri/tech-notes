## Week 1: Gradient Descent

* Supervised Learning
* Regression
* Classification
* Unsupervised Learning
    * Clustering
    * Separation of voices. Can do this with basically just SVD!
* Mean Squared Error.
* Gradient Descent
* Finding derivative is easy for linear functions.
* Training data is a matrix, rows of data, with features, followed by
  outputs.
* With MSE, wrt to each parameter, the error is a quadratic
  function. Also quad fn is positive.
* So if the derivative is 0, then the parameter has been set optimally.
* Gradient descent is very sensitive to start location. Random
  restarts might be useful.
* With gradient descent; alpha is the learning rate.
* You can have divergence, because as the slope gets more and more
  whack, you will take bigger and bigger steps.
* You don't need to decrease the learning rate over time, but you
  might want to in case you are stepping to fast and diverging.
* Derivative of MSE is linear: `sum (h(x_i) - y(x_i))x_i`, which makes
  sense:
    * A datapoint's importance in total error is proportional to the
      point's error.
    * The magnitude of `x_i` means you get a bigger reduction in error
      for the same tweak of the parameter.
* Claim: linear regression MSE is always convex? Is that true? Of
  course, convex function can't fall into local optima.
* Batch Gradient Descent: you look at all the points. But you could do
  it example by example.
* "Normal Equations Method": that's solving exactly. Pretty sure it
  involves inversion of the matrix.
* Linear algebra roundup:
    * Matrix is a linear transformation.
    * Vector is mapped to a point by a matrix.
    * Orthogonal matrix: preserves orthogonality.
    * Orthonormal matrix: preserves orthogonality and length of
      vectors. I think normality implies orthogonal.
        * If you think of "normal" only wrt basis vectors, you can
          have normality without orthogonality. But that sounds silly.
    * Not all matrices will have an inverse; for instance, if they
      collapse a dimension.
* Vectors are always columns, so that they appear on the right of
  matrices during multiplication: `Ax`.
    * To perform `A` followed by `B`, multiply the matrices together:
      `BA`.
    * This makes sense, because whatever `A` maps a unit vector to
      will then be run through `B`.

## Week 2: Multivariate Linear Regression

* Linear model is: `\theta^Tx`.
* Gradient descent can run slow if the features have very different
  scales. In that case, a "small" step in one dimension might be a
  huge step in the other dimension. That can lead to slow divergence.
    * Thus, you might try to put the features on the same scale.
    * OTOH, isn't this also affected by the sensitivity of the y
      variable to the input variable? How do you know two numbers are
      on the right "scale". Should you use feet, yards, miles?
    * Basically, you want your contours to be circles.
    * You might normalize so that all features are in a fixed range;
      say -1..+1.
* You can also normalize so that features have a mean of zero. This is
  *mean normalization*.
    * In particular, you can normalize by `x_1 - \mu_1 / \sigma_1`;
      you're basically normalizing mean to zero and standard deviation
      to 1.
    * TODO2: **Why?** He doesn't describe why.
    * The reason why feature scaling might help is not explained.
* How do you know how many iterations for gradient descent? Keep doing
  until it stops improving the metric.
* You can also check if the learning rate is too great if the cost
  function is increasing.
* In linear regression, you can re-featurize your features. You can
  apply transforms to a single variable, or alternatively you can
  combine features. For instance, if you have a feature for width and
  another for length, you can multiply them to produce an area
  feature. Your model can be linear in area now.
* Gradient descent can also simply solve polynomial regression
  problems.
* You can also turn polynomial fitting into linear fitting by
  introducing new features representing second-order terms.
    * I don't think that should be entirely necessary for gradient
      descent, though. Though it might make the problem convex?
    * In fact, polynomial regression is typically seen as a form of
      linear regression with re-featurization.
* Didn't discuss how you might choose the features.
* Normal Equation.
    * Matrix of data is called the *design matrix*. It consists of
      rows of the data observations. We call that `X`, while the
      response variable is a vector `y`.
* It sounds like you can find global optimum by setting `\theta =
  (X'X)^-1X'y`. Why the fuck so? Ng doesn't prove that at all. This is
  a joke.
* **TODO2**: What is the intuition behind this?
* I figured out the intuition, which I have in my linear algebra notes.
* Note that `(X'X)^-1` doesn't exist if:
    * The columns of `X` are not independent.
    * That happens if some features are redundant.
    * Which can specifically happen if there are more features than
      response variables.

## Week 3

* Can use linear regression and predict class 1 if `>0.5`.
    * He doesn't say so, but would work for multiple classes if you
      have a different basis vector for each output class.
* Linear regression tries to put a line through the data, which isn't
  actually what you want.
    * E.g., if for all `x>10`, `y=1`, adding a lot of data points
      `x>>10` will tend to suppress the slope of the line of best fit,
      overestimating the decision boundary.
* Another weirdness: you can end up with values `<0` or `>1`.
* Logistic regression has hypothesis: `h_\theta(x) =
  1/(1+e^(\theta\trans x))`.
    * This fn is the *sigmoid function*, or *logistic function*.
    * We'll expect that `h_\theta(x)` estimates
      `P(Y=1|X=x)`. **TODO2**: Not proven!
* Logistic regression is kind of placing the positive and negative
  datapoints at `-\inf` and `+\inf`. That's why we the decision
  boundary is `\theta' x = 0`.
* Note that adding extreme points minimally changes the *decision
  boundary*.
    * That's because the sigmoid function quickly becomes `~1`.
    * The decision boundary is defined by the nullspace of the
      `\theta' x`.
    * Effectively logistic regression is placing this boundary.
    * Of course can have non-linear boundary if you introduce higher
      order terms.
* If you use squared loss with logistic regression, then the cost is
  not convex in `theta`, so gradient descent will get stuck in local
  optima.
    * The typical cost function will be the negative log of
      `h_\theta(x)` if `y=1` (since high values of `h_\theta(x)` are
      ~1, and the `-log(1)=0`).
    * OTOH, take the negative log of `1 - h_\theta(x)` if `y=0`.
    * Note that the cost approaches infinity if our model is very sure
      of a wrong answer.
* Since the problem is convex, we can of course use gradient descent.
* Derivative of cost function wrt theta turns out the same as in
  linear regression: `(y - h_\theta(x))'X`.
    * NB: Gradient has partials as its columns. That makes sense: if
      there are three inputs and 1 output dimension, then the gradient
      is a mapping of `3` to `1` dimensions.
* NB: Andrew Ng says he used conjugate gradient descent for years
  without knowing how it worked??
* Mentions that conjugate gradient descent or BFGS/L-BFGS are more
  complex approaches to gradient descent.
* Can train multiple-class problem using separate one-vs-all training
  of each class against all the others.
    * To classify a new point, we just run each classifier, picking
      that class which maximizes the observation.
* Models with few features can underfit; we say they have "high
  bias". That's because even as we see training examples to the
  contrary, the model rigidly fights the data.
    * As you add more terms, you can reduce error, but at the risk of
      overfitting, with little predictive value.
    * We say this has "high variance".
* Ways to fight overfitting:
    * Feature selection: throw out irrelevant features. This can be
      done manually, or via "model selection", which is discussed
      later in the course.
    * We could keep all the features, but try to suppresss the
      parameters from getting too wacky.
* Regularization: penalize for large parameters
    * Basically you add `\lambda theta'*theta` to the cost
      function. You don't know lambda, so you have to learn that too.
    * Changes the gradient descent update function simply: subtract
      `lambda theta` from the derivative.
    * **TODO2**: Work out normal equation for closed form solution.
    * He says the problem of inverting `(X'X)` in normal equation is
      gone, as this becomes `(X'X + lambdaD)`. Why does that happen?

## Week 4: Neural Networks

* With lots of features, too many features to train if you throw in
  quadratic terms.
* Basically, you're trying to get non-linearity. Is there another way
  to acheive that?
* Idea that there is one fundamental learning algorithm in the brain
  is advanced by evidence that the brain can be rewired. There don't
  seem to be specific parts of the brain that can do *only* a given
  task.
* Neurons: we'll have a number of inputs, and the output will be the
  sigmoid fn applied to a linear transformation of the inputs.
    * We could theoretically explore other *activiation functions*.
    * This effectively binarizes the output, while having a linear
      decision boundary.
    * This seems like one of the simplest ways to get non-linearity.
* Input layer is the first layer. There is also an "output layer", the
  top layer.
* Hidden layer is any layer inbetween.
* Each layer has a matrix that describes the mixture of a previous
  layer to the next layer. We transform the activations of the previous
  layer by this matrix, then apply the logistic function to binarize.
* So `z2 = Theta*a1` and `a2=g(z2)`, where `g` is the logistic
  function.
    * But we also want to use the bias neuron in the next layer.
    * To do this, we cheat and augment `Theta` so that the first row
      of `Theta` is `(1, 0, ...)`. That way each layer holds onto the
      bias unit.
    * This is called *forward propogation*.
* You can program perceptrons to compute `OR` and `AND` and `NOT` easily.
    * With an extra layer you can combine these to learn `XOR`.
* Of course, you can have multiple output classes if you just have
  multiple neurons in the output layer.

## Week 5: Training NN

* Basically, you generalize cost function:
    * Multiclass:
        * `J = sum((y .* log(prediction)) + (1-y).*(log(1-prediction)))`
        * Note that `y` is a *matrix*, where the columns are basis
          vectors representing one of `K` classes.
        * Then we add a regularization term:
        * `J += lambda sum(Theta_l .* Theta_l)` for each of `l` levels.
        * That's ever so slightly wrong, since we shouldn't count the
          weight given to the bias unit.
* Basically, you "backpropogate errors". That means:
    * Take an example and feed-forward through the network.
    * You find the error in the output layer.
    * Then you take this difference, and you project onto the previous
      layer's `Theta`, remembering to multiply by `g'(z)` by the chain
      rule.
    * This shows how a change to one of the previous neuron weights
      would effect the ultimate error.
    * You do this for each example, summing the deltas.
    * Now you have your gradient. And of course you can add
      regularization too.
* Gradient checking
    * Just to make sure we calculate the gradient correctly, also
      calculate it using `+`/`-` a small epsilon.
    * Note that you could use this to calculate your gradient, but
      it's very slow. That's because you have to feed forward.
* Random initialization is important.
    * Say you set everyone to zero.
    * Then in 1st iteration everyone has a weight of 0.5.
    * Then all edges into an output vertex will have the same delta.
    * Likewise, every edge into a topmost hidden layer vertex will
      have the same delta.
    * In fact, all edges will have the same delta, because every node
      in this topmost layer has the same activation and delta.
    * Etc.
    * All the way down to the bottom-most layer, where activations are
      different (because they're observed inputs).
    * But then all the edges out of an input vertex have the same delta.
    * Thus we randomize weights in the beginning. This lets them
      diverge.
* Architecture choice:
    * Hard to say. But a lot of the time you use a single hidden layer.
    * Typically use approximately the same number of hidden features
      as regular features. Maybe a couple times more.
* Whoa: this is *not a convex problem*. But he just throws out there
  that this isn't usually a problem.

## Week 6: Bias/Variance

* Often use a holdout set for picking a degree of polynomial.
    * High bias would occur when you have a high error on both your
      train and test data.
    * High variance when you have low error on train but high error on
      test.
* Can also use another holdout set (usually separate) for picking the
  amount of regularization to provide.
    * High bias when you have high error on both train and test.
    * High variance when you have low error on train but high error on
      test.
    * I guess that's the same as before, basically.
* Definitely need to use a different set to report your expected
  generalization performance.
* You can also examine how much extra data is helping you:
    * Plot training vs validation errors *as a function of data used
      to train*.
    * Training error should be increasing, validation decreasing.
    * But if the validation flattens out, then it can't benefit from
      more data.
    * This happen typically when the bias is too high. We're not
      effectively using our data.
    * If there is high variance, than there should be a gap. More data
      might help us close that. (Of course, we should probably use
      regularization too).
* Attacking High Variance
    * Get more data
    * Restrict features
    * Increase lambda
* Attacking High Bias
    * Get additional features
    * Add polynomial features
    * Decrease lambda
* Neural nets:
    * Large does tend to be better.
    * But slower, of course.
    * And more prone to overfitting. So you regularize.
    * How many layers? One hidden layer is a good default, but you can
      test!
* Spam Classification
    * Use word vectors. Primarily use top occuring words.
    * Strip out punctuation. Stem. Fix misspellings.
    * Typical approach is to look at the misclassified items in the
      holdout set.
    * Helps to have a single value metric to be optimizing.
* Skewed Classes
    * Sometimes the positive class is much less common than the
      negative.
    * In that case, we could check precision and recall.
    * Precision is #true positive predictions / #positive predictions.
    * Recall is #true positive predictions / #positives.
    * Meh, these are *two* numbers.
    * You could trade precision for recall in logistic regression by
      upping the threshold from `0.5` to something larger.
    * A way to combine is the `F1` score, `2PR/(P+R)`.
        * I think this would say that for a given `P+R`, it's best
          when `P=R`.
* "Unreasonable effectiveness of data"
    * One simple study shows that 10x more data gives better perf than
      switching to the best algorithm from the worst with the same
      amount of data.
    * For some questions, it's clear that features are
      insufficient. E.g., no human could ever predict price of a house
      by just square footage.
    * Another thought: you want to fit a complex function. But that
      means lots of parameters to train. That means you need to avoid
      overfitting. Which means you need a lot of data.
    * Also, more data won't help much if we have a really stupid
      hypothesis type. We need those extra parameters to use the more
      data. In particular, if your hypothesis underfits the current
      data, more data isn't likely to help...

## Week 7: SVM

* Uses a different cost function
    * Still uses a linear decision boundary.
    * But if a positive example and `theta'x>1`, loss is zero.
    * If positive example and `theta'x<1`, loss is `theta'x`.
        * Kinda weird that we choose `1` as the boundary, since
          anything greater than `0` would lead to this point being
          classified positive.
    * Called the *hinge loss*.
    * Regularization is important, `theta' * theta` should be
      minimized.
    * Effectively finds a "maximum margin" if the data is linearly
      separable.
* I think having the hing loss around `-1` and `1` is important, since
  that means you want to *keep a margin* between the two classes. If
  the bounds were equal, you could put the decision boundary anywhere
  in the margin. You wouldn't have to worry about the slope of the
  hyperplane, because it would by separate no matter what. But to get
  zero loss with the hinge loss as we've defined, you need to put it
  smack in the middle.
* Having non-zero regularization is important for the decision
  boundary to be in the middle.
    * But regularization also helps in the presence of outliers; if
      the data is not linearly separable, then the SVM can accept this
      if it keeps the regularization term small.
* Basically, you can choose smaller theta if the boundary is farther
  from the support. That is, you know you need to go from zero to one
  output in the distance from the boundary to the nearest point. If
  you can make that distance longer, than your slope can be less.
* One common kernel is to compute the similarity of the sample to a
  landmark. It's given by the probability density of that landmark,
  assuming a multivariate gaussian variable. This is called the
  *gaussian kernel*. Also called *radial basis function kernel*.
* You use the training datapoints as the centers of the gaussian
  distributions.
* TODO2: It sounds like SVM rescales the parameter vector `theta` to
  "rescale" it for regularization???
* TODO2: Why can we not use this for RLR? He answered: the
  computational tricks that apply don't generalize well to LR. It
  would be really slow.
* In addition to choosing regularization, you can change the stdev of
  the normal distributions for similarity. In that case, higher sigma
  means higher bias, but lower sigma means higher variance.
* Note that the radial basis kernel results in linear separability.
* Sometimes when no kernel is used we call it the *linear kernel*.
* Note: feature scaling is important when using gaussian kernel.
* SVMs for multiclass classification typically use one-vs-all.
* SVM vs LR:
    * Linear decision boundary from LR is as good as you're likely to
      get when you have lots of feature dimensions but few
      datapoints. You're not going to be able to learn a sophisticated
      model in this case without serious overfitting.
    * If you have smaller number of dimensions but an intermediate
      number of examples, you might benefit from using a gaussian
      kernel to gain non-linearity.
    * If you have *lots and lots* of examples, though, SVM packages
      with kernels might run slow. In that case, us LR or SVM without
      a kernel.
* LR or SVM without a kernel are about equally good. Typically perform
  quite similarly. Neural networks, OTOH, tend to be slow to train.
* BTW: SVM optimization problem is convex.
* TODO2: SMO algorithm is common. But I need to learn how to use that.
* We know we can tradeoff precision/recall in LR by changing threshold
  and looking at test set. I guess we can do likewise for SVMs?

## Week 8: Clustering

* K-Means.
    * Iterates between assigning to one of the clusters (via who is
      the closest).
    * Other phase is moving the centroid to the center of the items
      assigned to the cluster.
    * Randomly choose original centroids. For now, we'll pick K
      manually.
    * You can use this to separate into truly distinct clusters, or
      alternatively you can use it to segment a continuous range, to
      sort of discretize it. One example I saw was t-shirt sizing.
    * Optimization objective is to minimize average squared distance
      to centroid. This is sometimes called the *distortion function*.
    * We can see that phase1 tries to maximize expectation by varying
      assignment to centroids, while phase2 tries to maximize location
      of centroids given assignment to the centroid.
    * Algorithms like that are called *expectation-maximization*. BTW,
      each step should be bringing down the cost in an EM algorithm,
      so the cost function should be monitonically decreasing.
    * To randomly pick centroids, you might randomly pick some
      datapoints. That way everyone will have at least one element to
      be the centroid of (to start).
    * Subject to local optima. One common problem: two natural
      clusters get merged, while a single cluster gets
      split. Therefore, it's common to use random restarts to try to
      find something more likely to be a global optimum.
    * Most common to choose number of clusters by hand. You might
      choose this by the "elbow method"; basically, you look where the
      cost, as a function of the number of clusters, flattens out. But
      maybe there is no clear elbow...
    * Another way to determine the # of clusters is to evaluate them
      on some downstream purpose. E.g., if you're reducing
      dimensionality for a later ML algorithm.
* Dimensionality Reduction/PCA
    * Basically project data down onto a reduced number of dimensions.
    * Want to find a reduction that looses as little info as possible.
    * PCA is most typical approach.
    * Basically, we want to find a set of orthogonal vectors such that
      the proejction into the subspace minimizes the error.
    * PCA is in fact different from linear regression. In linear
      regression we try to minimize distance in just the `y` space,
      but here we want to minimize distance of the entire `x` vector
      from the hyperplane.
    * Feature scaling will be important so that the distance is not
      dominated by any feature. Mean normalization is also helpful,
      because then we can assume the hyperplane passes through the
      origin (maybe?).
    * He punts on SVD. Fuck Ng. I think you basically project the data
      onto itself, and then find the eigenvector decomposition. Then
      you pick a subset of them, but which ones?
    * Of course, you just project onto the chosen eigenvectors.
    * You can reverse the dimensionality reduction to recover the
      original projection onto the hyperplane. In that sense this is a
      lossy form of compression.
    * To choose the number of components, we choose the minimum `k` so
      that the amount of variance retained is `>99%`.
        * In other words, the average squared error should be less
          than 1% of the error of using just an average of the data.
        * You can calculate this easily given the singular value
          decomposition, but he doesn't explain how. Fuck
          you. **TODO2**: read Strang.
    * Common approach: reduce dimensionality via PCA, then on the
      reduced space run a supervised algorithm.
    * Ng suggests that use of PCA to prevent overfitting is not
      great. He prefers you just regularize.
        * One reason is that dimensionality reduction might throw away
          information *useful for predicting the
          output*.
        * He suggests at least trying without PCA first. If you don't
          have a good reason, don't use it!
* Cool exercise where you reduce a color pallet using K-means!

## Week 9: Anomoly Detection and Recommender Systems

* Anomoly detection might be seen to be related to density estimation.
* **TODO2**: I notice that time series models are never used throughout
  this course. What interesting folds might there be when trying to
  use a time-series part?
* He defines Gaussian distribution. Maximum likelihood estimate for
  `mu` is the mean of the sample data, while max likelihood estimate
  for `sigma**2` is the mean squared error.
    * Technically speaking, I think you want `sigma**2=(1/(m-1))sum(
      squared_error)`. But he mentions that and says that it
      practically makes no difference for large `m`.
* He assumes that density is product of the densities of the various
  components individually. That is, he makes an independence
  assumption. He assumes the densities are normally.
* To evaluate anomoly detection, we want labeled anomolies.
    * You split your data into training/cv/test sets. Your training
      set could just consist of unlabeled data. The assumption is that
      there aren't many anomolies that slip in there. Save the labeled
      anomolous ones for the cv/test sets.
    * Basically, use the cv set to choose a cutoff for your anomoly
      detection algorithm.
    * Of course, by definition, anomolies are anomolous, so you should
      use something like F1 score instead of classification accuracy
      to measure performance, since defaulting to no anomolies would
      otherwise give high classification accuracy.
* Why not use just regular supervised learning?
    * Very small number of anomolies in the data, so want to save that
      for cv/test sets.
    * Other idea: it might be hard to learn what an "anomoly" looks
      like. It just doesn't look like a typical example. In
      particular, you may have poor generalization.
* You might not have gaussian distributed variables. You might be able
  to transform so that you have a gaussian variable. That might
  increase performance.
* To decide features, you might look at examples of anomolies for what
  stands out, and then use these as features for your algorithm.
* We might use a multivariate Gaussian distribution, where there may
  be some covariance between variables.
    * This allows you to change it so that the steepest slope of the
      gaussian may not be parallel to one of the axes. Basically
      that's saying that each variable has some correlation with the
      other.
    * **TODO2**: I think I could understand the math here better...
    * To learn such a model, just take the sample mean and the sample
      (multi-dimensional) variance.
* The multidimensional model is much slower to calculate, because you
  have to find all the covariances. So if you can figure out what
  combinations of features have interesting relationships, you don't
  need the multidimensional model; you can just add the average of
  those features into your original model.
* Also, you need more datapoints to estimate the covariances well. In
  particular, the covariance matrix won't even be invertible if the
  number of examples is less than the number of features.

* The basic idea is that you try to project movies into some content
  space.
* Then you learn parameters for what kinds of movies each user
  likes. Then you predict whether they'll like that movie.
* You have a simple linear regresison problem per user after you
  project the movies into a feature space.
    * You can train many users at the same time if you consider a
      problem where you've concatenated their parameter vectors.
    * This is prolly really easy to distribute.
* But what if we don't know the content? That's *colaborative
  filtering*.
* If you knew the parameter vectors of the movie watchers, you could
  try to choose feature assignments to each movie to minimize the
  cost. That's just linear regression again.
* Kind of a chicken-and-the-egg problem. So maybe you randomly
  initialize and then do an EM back-and-forth.
* Actually, you can fold the two problems into a single gradient
  descent algorithm. Whoa.
    * TODO2: What's the convexity of this?
    * BTW: you want to randomly initialize so that symmetry is
      broken. Basically, for the same reason as with NN.
* Another way to see this algorithm: it is a *low rank factorization*.
    * We say that because we observe elements of the scores matrix.
    * But this is a product of a parameters matrix `Theta` (where the
      rows are the parameters for a user) and a matrix `X`, where the
      rows are the attributes of a movie.
* Once you've found these components of the movies, you can relate one
  movie to another.
* As a detail: what if someone has rated no movies? If you're doing
  this with regularization, you'll predict a zero parameter vector for
  them.
* You could mean normalize the data. Now a user's ratings are diffs
  against the average rating for that movie. After that, you can just
  run the algorithm on the diffs, but you add the mean at the end when
  you make a prediction about a movie's quality.

## Week 10: Large Scale ML

* More data can be better than better algorithms. Who has the most
  data wins, not necessarily best techniques.
* Classic situation is a high-variance, low-bias situation; where perf
  on validation set does not converge to perf on training set.
* Stochastic gradient descent just looks at one example, calculates
  the partials for the cost *with just this example* and takes a small
  step.
    * We repeat for all examples in shuffled order.
    * TODO2: How do you know when to stop?
    * Might use "mini-batch gradient descent"; that operates on
      `1<b<m` examples. That might give you more stable steps. But
      especially you can take advantage of vectorization.
    * To track progress, you can keep a running cost of the past
      thousand examples Then every thousand examples, you record this
      sum. You are expecting to see that this tends to go down.
    * It'll be noisy, but expectation is that the cost decreases a
      while, then plateaus. We can abort if we stop seeing improvement.
    * You might see just a plateau; either the algorithm didn't learn,
      or it learned less than the noise.
    * Finally, you might see an overall increase, which means the
      learning rate is too high.
    * Note: the larger the learn rate, the more noise at convergence,
      because you're sort of oscilating around the minimum.
    * To reduce that oscilation, you can decrease the learning rate
      over time. That's simulating annealing, but it's a little
      finnicky to setup because you need to figure out how fast to
      decrease the rate over time.
* Online learning:
    * Say you have a binary classification problem shaped like this:
        * Predict whether a customer (with certain features) will
          transaction for a good (with certain features) at a price.
        * If you lower the price enough, you increase prob they
          transact.
        * But you want to maximize the price, while still maintaining
          prob that they purchase.
        * You want to learn this online.
    * To do this, you can just run stochastic gradient descent
      online.
    * Will also track temporal changes in the model; if user decision
      making changes, the model will change accordingly.
    * Also gave an example of online learning of click-through rates.
        * One thing I found non-satisfying: links are in competition,
          if you present 10 links, clicking on the best one doesn't
          mean the others were worthless...
    * Batch gradient descent can be done with MapReduce of course.
    * Whenever the bulk of the work is summations over training set,
      then MR is going to be helpful.

## Week 11: Application Example

* Started to talk about extraction of text from a photo.
* You could train on just "windows" of people and non people.
* Then in your image, you might slide the window through the image,
  looking for people. The "step size" might be 50% of the image's
  width, and when you finish the row, step in y by 50% of the image's
  height.
* You might then try again with a larger window size (same aspect
  ratio). You might grow by maybe 50%.
* For text detection, you might look for characters. Then, you look
  for large areas of text and pick the bounding box. This is called an
  *expansion operator*.
    * This might expand by saying are you within a certain number of
      pixels from a detected character.
    * You might also throw away misshaped stuff; like if the area is
      taller than wide (since text is typically horizontal).
    * At this point, we can cutout these regions and try to do OCR.
* To do character segmentation, might train on examples of windows
  with a chracter break in the middle vs those which don't split two
  characters. We do the stepped windowing technique again to segment
  the letters.
* Finally, we can do char detection.
* One trick is to generate artificial data. You can do this with text
  by using fonts.
    * You might also introduce optical/audio distortions similar to
      what you might encounter in real world. This requires a model
      for how things might be distorted.
    * Incidentally, adding random noise normally doesn't help, you
      need distortions similar to what you would experience IRL.
    * Often Ng asks first how to get 10x the data. Soemtimes it's
      cheap to collect or label yourself, or hire Turks. You could do
      amplification if necessary.
* To optimize an ML pipeline, you can check the accuracy after each
  state of the pipeline:
    * Say you correct the output of stage1 so it is 100% accurate. You
      can measure the accuracy of the output of stage 3 (the last
      stage) to see that how much improvement you get from focusing on
      stage1 by comparing to just normal input.
    * Then you can correct the output from stage2; you can see how
      much further improvement you would see by just focusing on stage
      2.
    * Finally, you can see how much improvement is left by focusing on
      stage 3. That's just 100% minus the previous number.
