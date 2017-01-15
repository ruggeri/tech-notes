# Week 1: Basic Concepts/Introduction

* ML is good for tasks where humans don't know how to write a program
  that performs a task (vision). Or maybe it is inherently a
  guesstimate-style enterprise, where there aren't any simple or
  reliable rules (fraud detection).
    * Pattern recognition: object, facial, speech recognition
    * Anomoly detection: fraud
    * Prediction: stock prices, recommendation
* Kind of a different style of programming. The machine learning
  approach learns a model we could never learn.
    * I think this is important to my interest. Note that I'm less
      interested in learning the parameters of a very structured
      model. That's because we already know how to write that program,
      but we are unsure of some details.
    * Other programs, we have literally no idea how to write.
* Works well given that if patterns change, we can relearn (e.g.,
  fraudsters get more sophisticated).
* Digit recognition (MNIST) is a common evaluation problem.
    * Very low error rates.
    * ImageNet is a harder problem where best 2012 solution had 40%
      error. Deep nets were used.
    * 21% error on TIMIT speech recognition with deep nets.
* Biological Neural Nets
    * Talks about how architecture of brain could inspire style of
      parallel computation.
    * Talks about how motivates learning algorithms inspired by brain.
* Activations functions:
    * Linear, Binary threshold, Rectified linear (sometimes called
      "linear threshold").
    * Sigmoid (most common; derivatives make learning easy.)
    * Stochastic binary: basically one of these activation fns, but
      then uses it as a prob of producing a 1.
        * Hm. This binarizes the neuron output, and also adds noise,
          which might be good to avoid overtraining?
        * Not entirely sure what the benefit is.
* Showed a simple training of a NN that connected pixels to digit
  classes. For each example, if classified incorrectly, incremented
  weight from active pixels to correct answer, and decremented weights
  to the network's incorrect guess.
    * He mentions why this is insufficient. What is learned is
      templates for the numbers. The winner is the number which has
      the most overlap with the template.
    * So, for instance, if you allow random rotations of the numbers,
      you wouldn't learn anything.
* Types of learning:
    * Supervised: Learn to predict a given output given an input.
        * Regression vs Classification.
    * Reinforcement: Learn to select actions that maximize payoffs.
        * Difficult because rewards are typically delayed, so you
          don't know what went right and what went wrong.
        * Scalar reward doesn't convey much information. Hard to train
          many parameters given just a scalar reward.
    * Unsupervised: Trying to find good internal representation.
        * Often used for finding low-dimensional representation. PCA
          is a common technique.
        * Clustering.
        * One nice thing: you can learn from unlabeled data!

## Week 2: Perceptron Learning

* Feedforward nets: most common.
    * Any net with more than one hidden layer is "deep".
    * Activations need to be nonlinear function, so that we can
      transform the input in such a way that things that should
      produce an output become more similar to the thing that produces
      that output, while things that don't become less similar.
* Recurrent NN have directed cycles.
    * They can have complicated dynamics and can be difficult to
      train.
    * They can be used to model temporal data. In a FFNN, you could
      reproduce an RNN with one hidden layer per time-step. Each layer
      is trained seperately.
    * Presumably in an RNN you just hook the hidden layer outputs are
      the inputs for the next step. But this introduces a cycle in
      training...
    * RNN has the ability to remember recent states in the hidden
      layer, but apparently it's hard to train them to use this
      ability.
    * Symmetric networks have same weight in both direction. Hopfield
      realized symmetric nets are much easier to train. But they are
      more limited in what they can do.
        * I guess at each time you start with an input, go through the
          layer, and then reset the input for the next time slice.
        * If there is no hidden layer this is called a Hopfield
          network. But what, how can you have a hidden layer and be
          "symmetric". What does that mean?
        * If there is a hidden layer, this is a Boltzmann machine.
        * Wait. By "symmetric" they mean a Markov field. That there is
          an *affinitiy* between neurons, and an overall energy
          function.
* Perceptron (Rosenblatt)
    * Basically just learn some weights for each feature, if above a
      threshold, then you predict 1, if not predict 0. Very easy to
      train. You have a dummy input for the bias term.
    * Minsky showed limitations of perceptrons. But many thought this
      applied to all NN.
    * Rosenblatt's learning is to go through data, and for each
      misclassification, add or subtract the input from the weights.
    * Proof this works:
        * For a given training example, consider the weight space.
        * There is a hyperplane through the origin that results in a
          perceptron output of exactly zero.
        * Weights on one side of the hyper plane correctly classify
          this example, weights on the other incorrectly classify.
        * Note that this hyperplane is perpindicular to the input
          vector.
    * Given that, it is clear that adding or subtracting will move the
      weight vector toward the good side of the plane. In fact, it
      will do the best at correcting it given that magnitude of
      adjustment.
    * But we need a weight vector that works for *all* examples (we'll
      assume they are linearly separable). Will this procedure move us
      close toward those weight vectors?
    * The answer is yes. Consider a "generously feasible vector,"
      which is a distance of one step from every example's
      hyperplane. In particular, we know that this is at least one
      step from the current' example's hyperplane. So taking a step
      toward it will in no way overshoot.
    * Note that if there is any solution, there must be a "generously
      feasible solution", since we can always scale up the weight
      vector arbitrarily, move it farther from the edges of the
      hyperplanes.
* Perceptrons and Features
    * With enough features, you can linear separate. Same idea behind
      SVM. So you need features. But the difficulty is in finding
      features.
    * Given features perceptrons are limited. Can only linearly
      separate. Given two inputs `X` and `Y`, can't learn `X=Y` (since
      that's not linearly separable).
    * Also, you can't discriminate two bitmaps with same number of on
      pixels if you allow translation with wrap-around.
    * Thus we need multiple layers, and they have to have
      non-linearities in activation, else the system is still linear.

## Week 3: Backprop

* Starts with linear neuron, which is a perceptron but with a linear
  response. He uses this so that he can talk about the gradient.
    * For a single neuron, we can solve this analytically, but we want
      to be able to generalize to non-linear models.
* Describes gradient descent.
    * Because you are trying to minimize an error, you may never get
      it to zero, but you can move closer and closer to the best
      error.
    * Note that if two inputs are highly correlated, it may be hard to
      tease them apart. In particular, your update rule is perfect if
      the mixed partial is zero.
    * Interesting note about SGD. Basically, you're moving straight
      toward perfect classification of an example. So effectively you
      are zig-zagging toward the answer.
    * He covers logistic error.
* Backpropagation
    * People first tried to perturb weights and see if they improved
      performance. But that meant running forward passes on all your
      data, just to change one weight.
    * People tried do many perturbations and evaluating a set of
      changes. They then tried to correlate perturbations in the set
      with any improvements. But that meant they were trying to see
      through noise, which they did by collect samples, but that was
      wasteful...
    * It's much faster to do backprop. That lets you find the gradient
      and do gradient descent.
    * Basically, you run backprop for all training cases. This takes
      time proportional to number of connections and number of
      examples. But then you do an update of *all* the weights. That's
      way faster than perturbing and trying to run changes forward.
* He mentions that mini-batch GD is preferred to online or batch GD.
    * Also mentions that learning rate can change. If we're making
      progress, keep increasing rate, if we're oscilating, decrease
      the rate. You might have different rates for learning different
      connections, though he doesn't say how.
* Mentions overfitting as a problem.
    * You can try to regularize, or you can force connections to share
      weights (fewer params than connections).
    * There are also Bayesian approaches, and there is also model
      averaging.
    * "Generative pre-training"? I'm interested to hear more!
    * Dropout.

## Week 4: ???

* Let's say you have a dataset consisting of triples of the form
  `(obj1 relationship obj2)`. Some of these are "local"
  relationships. Some of these are redundant and can be inferred from
  the local relationships.
    * For instance, in the dataset, I may have `(ned dad robert)` and
      `(robert dad mario)` and finally `(ned granddad mario)`.
    * So there is the possibility to learn that my dad's dad is my
      granddad. So there is the possibility that if the dataset
      includes `(x dad y)` and `(y dad z)` BUT NOT `(x granddad z)`, I
      can still train a learner to know this.
    * What he does is trains a network where the inputs are `x` and
      `granddad`, and the output ought to be `z`.
* This is interesting, because the network would be learning logical
  rules.
    * He mentions a use-case is to train this on a dataset, then go
      back over the same dataset, finding those relationships that
      feel least likely to the network. Those training examples can be
      examined, because they may well be incorrectly labeled training
      data.
* He makes a note about cognitive science
    * Basically, he says that neurons in a ANN don't represent a
      single concept. Connections don't represent one kind of
      relationship.
    * He thinks that there is a *distributed representation* of
      concepts across neurons. That no individual neuron means one
      thing.
* He notes that squared loss isn't good for classification.
    * Andrew Ng said becuase the problem won't be convex.
    * Hinton says because derivative when logistic function is close
      to zero or one is itself very shallow. So it's hard to learn
      even when the answer is really wrong.
    * Derivative of logistic error wrt `z` is linear in magnitude of
      error.
    * By the way, this is called the **cross-entropy cost funciton**.
* Softmax
    * You have a group of neurons. The output of a neuron is `e**x/Sum
      e**x_i`. Basically, it's doing a probailistic normalization (and
      also fixing sign).
    * Has a nice derivative.
    * Talks more about the cross-entropy cost function. It makes sense
      when you're talking about probabilities.
* Talks about speech recognition
    * Speech recognition needs context to disambiguate. Audio signal
      just isn't good enough to clearly identify phonemes.
    * So we often use trigram models.
    * However, he wants us to be able to learn about semantically
      similar sentences from an example. "The cat got squashed in
      garden on Monday" should imply "The dog got flattened in the
      yard on Friday".
    * Basically, he uses a model that looks at two previous words,
      featurizes them, combines the feature vectors to predict the
      feature vector of the next word, and finally converts this to
      the next word (also connects feature representations of prev two
      words as input).
    * Apparently these NN are considerably better than trigram models.
* Problem: there are a very large number of outputs for word
  predictions (100k words).
    * This means possible overfitting.
    * We could try to restrict the size of the hidden layers, but that
      is limiting our learning.
    * We could try to train with huge amounts of data. That's cool,
      but what if we don't have that much?
* One solution: "serial architecture"
    * Basically, you put a candidate in as the *third* input.
    * Then you return just one value: logit score of the word.
    * That way there isn't possible overfitting.
    * To do prediction of next word, you have to run through all
      possibilities. This could be precomputed ahead of time to
      produce a table.
    * You can save a lot of time if, instead of considering all
      possible candidates, there is some simpler method to limit the
      set of candidates to choose from. E.g., NN could revise
      predictions of words that trigram model thinks are likely.
* Another: tree paths
    * Put words in a binary tree.
    * Convert context words into distributed representation, then
      connect this layer of featurized contexts to a *prediciton
      vector*.
    * Compare with a learned vector at each node of the tree. The
      vector decides which path to take.
    * Take scalar product and run through logistic function to predict
      probabilities of each branch.
    * You can train this with a kind of backprop. There's the
      featurization neurons, but also these other neurons with these
      activations that are scalar-product like.
    * Presumably you accumulate products as you go down the tree; you
      eventually get a bunch of outputs, which you can run a softmax
      on.
    * Basically, you're talking about a specialized version of a NN,
      with a weird activation fn.
    * I think when you're learning, you only need to do backprop on
      the path to the correc tanswer? That's because I think you just
      want to maximize the value of the correct answer?
    * Is that a general property of the softmax?
* Context featurization
    * They trained a NN to predict word based on context of
      surrounding words.
    * They trained with the right word, and also with random words.
    * They found that the featurization they got for individual words
      was useful for many other tasks.
    * I guess the idea is that you don't need the actual word for
      future tasks now that you have distributed representations of
      the words.
    * It appears that these featurizations find a lot of semantic
      details of words. This is interesting because we use a limited
      context. But maybe not so odd; we can easily learn what "She
      scrommed him with a frying pan", we might have an idea of what
      "scrommed" means.

## Lecturs 5-8: SGD and RNNs

* Object Recognition is hard
    * Segmentation is hard; finding out what parts of the scene are
      discrete objects. Also, harder when you think of overlapping.
    * In real life, we have stereo image, which is helpful.
    * Also, the lighting is very important; intensity of pixels are as
      much about lighting as the object itself.
    * Semantic understanding of objects. And the class is more a
      function of utility of the object, rather than the visual
      appearance. Objects of the same class can have totally different
      appearance. Imagine all kinds of chairs.
    * Changes in viewpoint cause changes in image that are very hard
      to understand.
* Approaches to viewpoint invariance
    * Redundant invariant features
        * Basically, transformations that are invariant under
          translations. E.g., have a feature which is positive if
          there are roughly parallel lines, no matter how they are
          arranged.
    * Put boxes around objects.
        * Basically, find out how to put boxes around objects. Account
          for rotation and translation
        * Then you can try to detect the thing.
        * But finding this box can be difficult itself.
        * In particular, to figure out the box and orrientation, we
          have to have an idea of what the thing is. So that's kind of
          chicken-and-egg.
    * Convolutional NN
    * Hierarchy of parts with poses relative to the camera?
* Convolutional NN
    * Basically, many identical feature detectors at different
      positions in the image. Basically, they're replicated features.
    * Typically just choose to replicate across translation, not scale
      or orientation. That is trickier and expensive.
    * Of course, parameters to learn is much lower. And of course you
      have many features.
    * We have to do a constrained form of backprop. If `w1` and `w2`
      must be equal, we use the sum of the partials for `w1` and `w2`
      for the gradient step. This way they'll always be equal.
    * I think that's, mathematically, the right answer.
    * Now there are many neurons for each feature, and the appropriate
      group will fire as you translate the image. But it's not
      literally the same set of neurons that will fire.
    * You can do a limited translation invariance by taking the max of
      several adjacent feature detectors. This helps you pool the
      replicated feature detectors.
    * But doing this averaging loses position of the items in the scene.
    * So you can't use this approach for identifying spatial
      relationships of high-level concepts. For instance, distance
      between eyes is important for face recognition.
    * Yann LeCun: one of the first to identify hand-written nets with
      FFNN. Had many layers, many replicated units, pooling of nearby
      replicated units.
* General point about prior knowledge
    * How do we inject it?
    * Connectivity/architecture.
    * Weight constraints.
    * Activation functions that are appropriate.
    * Less intrusive than hand-engineering features.
    * A totally different approach: generate a whole lot more training
      data. For instance, translation invariance.
        * Fascinating example of synthetic data plus real data did
          better than just real data.
* This approach can be successful
    * LeNet uses careful architecture of network.
    * But using a deep, dumb net with lots of synthetic data, and
      running on a GPU did much much better!
* BTW to measure performance, don't just use error ratio.
    * When comparing to algos, look only at those errors that one
      model made, but the other got right. Compare these ratios.
    * Those which they *both* got wrong, those are maybe just hard
      ones.
    * This is McNemar test.
* Can MNIST convolutional nets solve object recognition?
    * Many many more classes (there are only 10 for digits).
    * Many more pixels. And they're in color.
    * 2D images of a 3D scene. Lots of info has been lost.
    * Cluttered scenes requiring segmentation.
* ImageNet competition
    * Get the right label in the top 5. That's because there are
      sometimes more than one object in an image. So label is kind of
      arbitrary.
    * Also, identify *where* that image is. Put a box around it. Need
      greater than 50% overlap with true box.
    * Typical approaches have some hand-tuned early stages for
      featurization. Top-level is typically fully learned. Not
      end-to-end learning.
* Very deep convolutional NN hugely dominated perf of traditional
  systems.
    * 7 layers (not counting max-pooling layers), early ones were
      convolutional. Last 2 layers were globally connected.
    * Rectified linear units were used in the hidden layers. These
      train faster and are more expressive than logistic units. Why
      more expressive?
    * Compeitive normalization. That means we supress the hidden
      activities when nearby units have stronger activities.
    * Also used random patches (224x224 of the 256x256) to get more
      data of translations. Used reflections. So lots of synthetic
      data. Did not use up-down reflections, since gravity is very
      important.
    * Combines opinions of patches and refletions.
    * Uses *dropout* to regularize weights in globally connected
      layers to prevent overfitting. Basic idea is that half the
      hidden layers are randomly removed for each training example.
    * Basically, that means that hidden units can't rely on the
      presence of other units. This means the units need to be more
      individualist.
    * Used GPUs. ~1k cores, very fast at computation. TODO1: I think I
      need to do GPU programming to get good at this stuff.
    * Training took ~1week.
* Talks about mini-batch SGD
    * Can do more steps faster, but not so inaccurately as online SGD.
    * Mini-batch isn't necessarily that much slower, because with
      vectorization calculating the gradients on a bunch of examples
      is still just matrix math, which is hardware accelerated.
    * Mini-batch is good if the dataset is big and highly redundant,
      since a sample of the data is represenative of the whole.
* SGD Tips-and-Tricks
    * Random initialization.
    * Suggests mean normalization, so that isoquants of the error
      surface are not as sharp an elipse. Also suggests unit variance.
    * More than that, you can try to decorrelate the inputs. You can
      try to do this by apply PCA, dropping some low-value dimensions
      (achieving a dimensionality reduction) and then normalizing so
      that each dimension has unit mean and variance.
    * A lot about tuning speed of learning rate.
    * Trick is to use *momentum*; change velocity of particle, not
      position by the gradient. That way, you kind of reduce noise.
    * Use adaptive learning weight for each parameter, slowly
      adjusting based on consistency of previous gradients *for that
      param*. That way you can learn some params fast and others slow.
    * Rmsprop: this is a technique that doesn't use the magnitude of
      the gradient, just the sign relative to previous updates. It
      then accelerates or decelerates the momentum
      appropriately. Based on rprop.
    * I don't see how rmsprop is helpful, but I'll take it on faith
      for now.
* Momentum method
    * Also, if you have a sharp elipse, you'll move *across* the major
      axis quickly, but you'll have some momentum *along* the major
      axis. As you climb up the other side of the major axis, this
      will cancel out previous momentum, but you'll still have the
      momentum along the axis.
    * You also want to attenuate the previous velcoity vector a bit,
      so as to allow for some slowing.
    * Suggests having high attentuation in the beginning, since early
      opinions are changing very rapidly, and then decreasing
      attenuation later to move quickly across plateaux.
    * A refinement: make the move, then update the gradient based on
      where you are. Sort of reverse order of the steps.
* Adaptive Learning Rates per Connection
    * Magnitudes of gradients can be very different in different
      layers.
    * Fan-in of a node determines the amount of *overshoot* at that
      node: simultaneous change to many parameters to correct the same
      error.
    * A simple approach is to increase the gain for a weight whenever
      the gradient here does not change sign. That accelerates.
    * Additive increase, multiplicative decreases. That's like
      exponential backoff. That way big gains decay rapidly if
      oscillation starts.
    * Reasonable to limit the gain sizes.
    * Also, adaptive rates like this want full batch learning, since
      mini-batch will have variation in partials for connections, just
      from noise.
    * This is a method attributed to Robert Jacobs.
    * Can easily be combined with momentum.
* Rmsprop
    * Rprop looks at gradients, and takes a fixed sized step in the
      right direction. This is better than just turning up learning
      rate; it can escape plateaux quickly, but it won't oscilate when
      in a sharp curve.
    * But rprop is smarter; it is adaptive per weight, since a single
      global step size seems foolish. And the adaptation of the step
      size is mostly as above; we multiplicatively increase if
      gradients agree, and decrease if gradient disagrees.
    * Note that this is like Jacobs, but only looks at signs.
    * Can't really do SGD, since too much noise in the gradients.
    * Rmsprop combines idea of rprop with efficiency of mini-batches.
    * Hinton keeps a moving-average of the gradient, to dampen out
      noise. We then divide the gradient at a step by the sqrt of this
      accumulated gradient.
        * Basically, this is mini-batch rprop, where we're normalizing
          the gradient to get just its direction.
        * But we're normalizing by a smoothly changing vector.
        * He is also exploring adaptive weights combined with rmsprop.
* No one technique for SGD; reflects that there are many kinds of NN
  (deep with bottlenecks, recurrent, shallow and wide). Different
  techniques are appropriate in different scenario.
