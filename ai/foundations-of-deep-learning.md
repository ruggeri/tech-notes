## Week 1

* Installed Anaconda.
* Installed Jupyter Notebooks.
* Tried the style transfer application.
* Played with the deep traffic simulation.
* Looked at Deep-Q Flappy Bird

```
# Simple Regression Code
import pandas as pd
from sklearn import linear_model
import matplotlib.pyplot as plt

dataframe = pd.read_fwf("brain_body.txt")
x_values = dataframe[["Brain"]]
y_values = dataframe[["Body"]]

body_reg = linear_model.LinearRegression()
body_reg.fit(x_values, y_values)

plt.scatter(x_values, y_values)
plt.plot(x_values, body_reg.predict(x_values))
plt.show()
```

* They talk a bit about Linear Regression.
* They note the data ought to be linear to get a good result. Also,
  they note that the regression is sensitive to outliers.
* No discussion on the math behind linear regression.

* Now they discuss classification. They show a linear decision
  boundary and call this logistic regression. They mention that we
  want to minimize the log loss function, but don't say what that is.
* They mention GD, but still don't describe what that is.
* They start talking about linear decision boundaries. They also show
  an example where the decision boundary is defined by the region in a
  quadrant defined by two lines.
    * They are basically defining a network where there are two hidden
      layer neurons and the output is binarized based on a threshold.
    * This is a multi-layer perceptron.
    * They show how you need multiple layers to do an XOR.
* They mention activation functions and sigmoid.

* They describe error function (squared loss).
* They talk about the partial of squared loss.
* They describe how it is proportional to (1) the error, (2) the
  coordinate for the example.
* They mention how you want to normalize your data to zero mean,
  variance 1.

* You use Numpy to do vectorized operations to do a forward-pass.
* Now they teach you backprop.
* All pretty simple crap.

* I actually find numpy broadcasting a little annoying. It means you
  kind of don't know what the operation will do.
* For that reason, I kind of like the einsum method which lets me
  think easier.

* They talk about train/test split.
* sklearn has a method: `sklearn.model_selection.train_test_split`.
    * Args are `X, y, test_percentage`.
* They talk about some measures of classification error:
    * Accuracy: percentage of examples we properly classified.
    * Actually doesn't talk about recall or precision. Great.
* They mention absolute error and MSE.
    * They motivate MSE by differentiability.
* They mention R2 score.
    * R2 score is the proportion of variance in the output that can be
      explained by the input.
    * The baseline model is the mean, and you calculate the empirical
      variance.
    * Then you calculate the empirical variance with the linear model.
    * Then you divide.
    * You subtract this from one to get the R2 score, which is the
      proportion of the original variance explained away by the model.
* They mention underfitting vs overfitting.
    * They mention that underfitting happens when we have strong
      biases. We see bad error on the train set when we have too
      strong bias.
    * We see overfitting when the performance gets really good on the
      training set, but fails to generalize.
    * We say that this error is due to *variance*. The reason is that
      we say the model is too free so that the noise, or variance, in
      the training set.

* In the NN for IMDB sentiment, a major advance was binarizing the
  input. The theory presented was that commons words often didn't
  convey sentiment, and so by scaling them by count we were amplifying
  noise.
* I guess the idea is not to just train the network to learn that
  these words are stupid. We want it to learn smart stuff. So it makes
  sense to help it.
* Changing the batch size allowed me to make updates much faster. A
  batch size of 1 seemed to work totally fine. I guess it makes sense:
  this is *stochastic*.
* They did do what I suggested: keep those words with predictive power.
    * I used pseudocounts.
* He suggests the true value in this is the ability to train over much
  more data.

* We're going to start with TFLearn.
* Mentions how sigmoids have fallen out of favor because of vanishing
  derivatives. So we can use ReLU.
* They mention you need to be careful of "dead" units. If a unit is
  never active for any input, then you can't backpropagate through it
  to any examples, which mean the input weights stop being updated.
* They mention what a softmax group is.
* They mention the cross-entropy loss function. They don't mention
  what I know, which is that this is exactly the log likelihood of the
  data.
* TFLearn initializes weights, runs forward passes, backpropagates,
  updates. You basically just define the architecture of the network.
    * It does make it stupid easy to define a network and run it.

* They talk about word2vec, which does an embedding of words.
    * There are two methods (1) continuous-bag-of-words: we predict
      the current word from the bag of context words on either side,
      or (2) continuous skipgrams: we predict the context words from
      the current word. It is typical with skipgrams to weight the
      error on closer words more than the error on more distant words.
* They intro some RNN resources.

* In Siraj's sentiment program, the input is a sequence of
  integers. He then uses `tflearn.embedding` which I presume says: the
  input is between 1-#words, and I map this to a vector of 128
  units. The mapping is the same for different words, I assume.
* He also uses `tflearn.lstm`. I'm not really sure how TFLearn does
  RNN. For instance, how many LSTM units are being used?

* Now they're going into TF. It looks node based like what we had
  before. We start with `tf.constant`, and there's also
  `tf.placeholder`. When you run, you can use the `feed_dict`
  parameter to provide a constant value to the placeholders.
    * TF gives you operations to create new tensors, like
      `tf.add(node1, node2)`. They have a world of functions, and of
      course many matrix math ones.
* There's also `tf.Variable`, which takes a starting value but later
  can be updated. We need to use `init =
  tf.global_variables_initializer()` and do `sess.run(init)`. There
  are examples in my "coursera" homeworks repository.
* They talk about numerical stability a bit. In particular, the danger
  zone for floating point is when you add small numbers to big ones,
  because we don't have that many digits of precision.
    * For this reason, it is often helpful to normalize mean/variance
      to zero/one.
    * Also, "poorly conditioned problems", where there is very
      different variance, means that you may be making contours more
      eliptical (assuming that magnitude of the impact of variables
      tends to be approximately equal). By making things eliptical,
      you're making it harder for a linear technique like gradient
      descent.
        * Note that this shouldn't matter for a (truly) parabolic
          surface that computes the Hessian.
    * For example: to normalize pixels in the range 0...256, divide by
      128 and subtract by 128.
* For weight initialization, there are lots of techniques. But they
  recommend sampling randomly from a mean-zero normal with low
  variance.
    * I think maybe the idea is this. In the beginning, if it were
      possible, you want a lot of the logistic inputs to be
      approximately `0`. This way, they have the highest derivative,
      which means they can move the easiest.
    * Yep, that's what they on stats.stackexchange.
* They describe SGD. He emphasizes that we really want to choose
  random batches each time. Otherwise SGD technically may not converge.
* Here's the idea of momentum, and why it's a good fit for SGD. The
  reason is that we want info from previous batches at each step. By
  using momentum, we have a lossy memory of the prior data, which
  means even if we compute a bad gradient, if we've accumulated a lot
  of momentum in a good direction, then we'll still be traveling
  mostly in the right direction.
* Also, he mentions that learning rate decay can be important. That's
  because as you get close to the minimum, you want to make smaller
  and smaller steps. This sort of naturally happens with GD because as
  you approach a min the gradient approaches zero. But that may not
  happen with SGD, because of noisy batches.
* He mentions that experience with NN shows that how quickly you learn
  isn't that correlated with how well you'll do when fully trained.
* Mentions that SGD has many hyperparameters, which makes it seem
  magical.
* They show that you can do batching in TF just by doing a for loop.

* They claim that "architecture engineering" takes the place of
  "feature engineering", because the model is supposed to learn the
  features.
* He mentions that PCA is one way to do dimensionality reduction.
* When there are missing values, you can try to replace them with a
  mean. You might also try to do some smoothing to eliminate outliers.

**Convolutional NN**

A tensor flow example:

They claim that you can do better with fewer parameters by going
deeper rather than wider. That makes sense to me, I think. But I think
it would be valuable to give an example. He says that this can work
because there is often a naturally hierarchical nature to the problems
we're trying to solve. For instance, a face is composed of eyes, nose,
mouth, which are composed of upper lip, lower lip, nostrils, etc.

They show how to use `tf.learn.Saver` to save/load model
parameters. You want to give each variable a name, otherwise the saver
will have difficulty deserializing if you instantiate any variables
not in exactly the same order (since it gives variables a name like
"variable_N", so names are dependent on order of construction).

Mentions: why didn't we use deep nets forever? One problem is we
didn't have big enough datasets to train them. Another problem is that
we have learned how to do better regularization.

Some ways to prevent overfitting:

* Early termination: stop before you overfit.
* Mentions L2 regularization.
* Mentions dropout: as we know, zero out half the activations.
    * This effectively forces the network to learn redundant
      representations.
    * This sort of does voting, in the sense that you can have a
      number of slightly different feature detectors.
* If you do dropout, when you go to use the system, you don't want to
  dropout randomly anymore. You want to use *all* the redundant
  detectors.
    * But the weights weren't trained this way, thus potentially
      causing problems.
    * To fix this, if you drop half the activations, scale the other
      half by two.
    * This makes sense because you're training such that later you can
      expect to have all the units, not half of them.
    * There is a TF method `tf.nn.dropout` which does exactly this for
      you.
    * You give it a `keep_prob`. As explained, you'll want to use
      `1.0` when evaluating your network.

**ConvNets**

* You have *patches*, this is a rectangle in the image. You have a NN
  from the patch to `k` outputs.
* You run the same NN on every patch. This converts each patch into
  having depth `k`.
* If you have a *stride* of 1, you will lose two pixels in either
  dimension. If you have a stride of 2, you will lose about half the
  pixels.
* This is their motivation for the prisms. The input is flat, and has
  the biggest surface area. But then the surface area decreases for
  each convolution stage, with a greater depth. The depth is
  theoretically semantic information. As you go further into the net,
  you squeeze out the semantic info.
* You can see this as generating a new image with `k` channels, with a
  single pixel per patch. Thus the semantic information remains local
  to the pixels.
* The `k` channels are called a *feature map*.
* What about edges? You either do *valid padding* (don't go off the
  edge) or you let yourself go one the edges and pad with zeros, which
  results in the same image size. This is called *same padding*.
* This process of mixing adjacent information is called a
  convolution. We're learning what convolution to perform. This
  function is the *kernel function*.

```
# Output depth
k_output = 64

# Image Properties
image_width = 10
image_height = 10
color_channels = 3

# Convolution filter
filter_size_width = 5
filter_size_height = 5

# Input/Image
input = tf.placeholder(
    tf.float32,
    shape=[None, image_height, image_width, color_channels])

# Weight and bias
weight = tf.Variable(tf.truncated_normal(
    [filter_size_height, filter_size_width, color_channels, k_output]))
bias = tf.Variable(tf.zeros(k_output))

# Apply Convolution
conv_layer = tf.nn.conv2d(input, weight, strides=[1, 2, 2, 1], padding='SAME')
# Add bias
conv_layer = tf.nn.bias_add(conv_layer, bias)
# Apply activation function
conv_layer = tf.nn.relu(conv_layer)
```

In this example, the strides are `[1, 2, 2, 1]` because it's a stride
of one through the input data, 2 for the width and height dimensions,
and 1 for the channels. Obviously the data and channel strides will
typically be 1. It appears the channel stride is sorta redundant,
since I think the convolution will be get all the color channels as
input.

`bias_add` looks like it's just a special case of `tf.add` but allows
broadcasting. For instance, this adds the bias to all the convolution
outputs, even though this is a lower-dimensionality tensor.

We often use max pooling. This is basically another kernel, where the
kernel operation is to output the max value. I presume this reduces
noise. But it also adds another layer, and more hyperparameters.

A common architecture is a few layers of convolution and max pooling,
followed by a couple layers of fully-connected layers. This was the
typical strategy of LeNet (letter recognition) and AlexNet (image
classification).

Another form of pooling is average pooling. This does a very obvious
"blurring".

2-by-2 max pooling filters with a stride of 2 are common.

Pooling reduces the size of the output, and it also prevents
overfitting. But I think it should also reduce noise in the image,
too. But it appears that pooling may be on the decline; this is
because (1) dropout is often a better regularizer, (2) pooling throws
away information, and (3) overfitting is less of a problem with very
huge datasets; we're more focused on the problem of underfitting: our
models don't have enough parameters. Presumably we don't use more
parameters because our model would take too long to train?

So, you have these convolutions, and the convolution to perform is
learned by a depth-one network. But you can apply a 1x1 convolution
after, which effectively makes this a "deep convolution", if you like.

Another idea is "inception module" (I don't know what the fuck that
means; it maybe comes from GoogLeNet). But the idea is that it can be
hard to decide whether to do pooling, a 1x1 convolution, a 3x3, 5x5?
The idea is to do all of these, and then just take the image maps
computed by each and concatenate them. The next layer can look at all
these different forms of information and potentially synthesize them.

**TODO**: It strikes me, what is the pro-vs-con of L2 and dropout for
  regularization?

They show how easy it is to use Keras to build a network at a high
level. It's basically a builder pattern. This is worth covering and
exploring.

They have an already setup GPU instance in AWS. (But I built my own
anyway).

## Project Notes

Setup of AWS was fairly hard when you don't know what you're doing. I
wrote this up in `randos/aws-gpu-instance-setup.md`.

I used two conv layers, both doing 2x2 filters with 2x2 max pooling
and stride. I had 16 channels at each conv layer. I applied dropout,
then fed to a fully connected layer of 512 units. Then fed directly to
the output. I used RELU throughout.

Nothing worked well until I took the truncated normal and cranked it
down from a stddev of 1.0 to 0.1. Then everything worked wonderfully.

I let it run for a good long time, but it's hard to overfit with a
dropout of 50% applied. My test accuracy was 70%, which was exactly in
line with my validation accuracy.

## RNN/LSTM

* They just go over the basic idea of RNN and LSTM.
* They show you how to do an RNN with LSTM in TensorFlow, using a
  contrib package.
* You can control exploding gradient by "clipping", which means you
  just don't let it be bigger than a certain number.
* I wrote the text generation project; they showed a stock prediction
  project.
* The LSTMCell API is pretty straightforward. You did need to use
  LSTMStateTuple, and each cell you create needs to be in its own
  scope. You also need to `scope.reuse_variables()` to reuse the
  previous LSTM gate-weights.
* API Notes
    * MultiRNNCell wires up a number of layers.
    * A higher-level approach is to use the `static_rnn` or
      `dynamic_rnn` functions. I think `static_run` is if the examples
      always have a fixed length; and `dynamic_rnn` is for when
      different examples have different lengths?
* Projects: Text generation, stock prediction.
    * Stock prediction was in Keras, but I'm more focused on TF.

## Embedding

* Skipgram vs CBOW.
    * CBOW "smoothes" in the sense that all the context words are used
      to predict the center word. A larger bag of words are treated as
      an observation.
    * Skip-gram you predict context words from center word. Each pair
      of words (center word + one context word) is a potential example.
* The model you use is a hidden layer with no activation function,
  then fully connected and fed into a softmax layer.
    * I suppose this is overall linear, but forces a dimensionality
      reduction in the first layer.
* The inputs are one-hot, and the outputs are probabilities. The
  expected output is a one-hot.
* Tricks:
    * Subsampling. This has you throw away a proportion of words that
      occur too frequently.
    * Negative subsampling. In the second layer, you adjust the
      weights that connect the embedding to the correct word. Then you
      select ~5 negative words and adjust the weights connecting the
      embedding to these words.
    * This means you adjust a very small number of the weights for
      each example. This is helpful because your embedding dimension
      may be 300 and the number of words is maybe 10k. That would be
      30MM parameters to tweak on every example...
    * In the first layer, you would only change weights pertaining to
      the input word anyway.
* The TF website mentions that an alternative approach to *prediction
  based methods* is to use *count based methods*. What this does is
  basically convert each word into a vector of its co-occurrence
  counts with each other word. Then you do SVD. This is called latent
  semantic analysis.
* The TF website mentions a little more about the idea of negative
  subsampling. Basically, you are trying to learn to discriminate the
  context word from "noise" words. In a sense, the model is not trying
  to learn the probability distribution of the context words; it is
  instead trying to learn to *discriminate*.
    * They note that then you naturally do a Monte Carlo thing by
      picking only a small subset of noise words to discriminate
      against.
    * In the limit this is the same as the generative model, but it is
      much more scalable for the reason we noted above.
* Talks about t-SNE a little bit.
    * This is a dimensionality reduction technique.
    * Links to a Chris Olah post.
    * Consider MNIST; you can project down to two coordinate axes. But
      this won't do a good job of keeping close points close together.
    * You can do better by projecting down to the principal component
      axes.
    * Another way is to do an *optimization based approach*. For
      instance, let your cost be `Sum (distance_in_new_space -
      distance_in_old_space)**2`. This is called *multidimensional
      scaling* or MDS.
    * We could try different cost functions. For instance, Sammon's
      method divides this quadratic cost by the distance in the
      original space. That strongly prefers putting close points
      close, and cares less about the distance between far points.
    * But that isn't very different than MDS in a high-dimensional
      space becaue everything is far away from each-other in a
      high-dimensional space.
    * Another interesting idea. We do the quadratic penalty only for
      nearest neighbors of a point. For any other point not a nearest
      neighbor, we actually assess a penalty based on `1/distance`:
      the proximity. This basiclly puts springs between the nearest
      neighbors in the old space plus every point is a charged
      particle that wants to push away the others.
    * t-SNE in particular looks like it has an interesting
      motivation. But I haven't had a chance to study it deeply at
      this time.
    * It would be fun to implement these!

## Tensorboard

* Not much too it. The main things are `tf.summary.scalar`,
  `tf.summary.histogram`.
* You also use a `tf.summary.FileWriter` to log the output.
* `tf.summary.merge_all` gives you a single value you can run with a
  session. When this is evaluated you hand it to the `FileWriter`.
* You can look at these values overtime. But you can also look at the
  graph structure. If you use named scopes it will organize the graph
  nicely.
* A common trick is to choose a log subfolder named with your
  hyperparameters in the dirname. Tensorboard can be pointed at the root
  `logs/` folder and will present all the models side-by-side.

## Randoms

* Sriraj demos a style transfer using VGG.
* Sriraj demos an RNN to generate music based on Pat Metheney MIDI
  files.
* Talks about GloVe: global vectors. Sounds like it makes a
  co-occurrence matrix and then does a low rank
  factorization. Effectively like collaborative filtering.
* Talks about sequence-to-sequence generation of headlines. Uses a BBC
  dataset with text and headlines. He featurizes using GloVe, then
  uses an RNN to try to generate the given headlines.
    * But wait, the headline isn't equal to the corpus length, right?
    * I didn't really understand this. I think this is a notebook
      project later.
    * Also, it looks like attention is a big part of this.

## Weight Initialization

* Of course you need to break symmetry, and you won't have any signal
  backprop if you choose all zero weights.
* He shows dramatically that a random uniform `0-1` is *way* worse
  than `-1` to `+1`. Fascinating!
    * After 2 epochs on MNIST, the -1 to +1 weights had 89% accuracy
      vs 74%!
    * They show that picking the scale of the weights is important:
      plus or minus `0.1` can be much better than `1.0`!
    * Using this smaller weight range, they increased accuracy over
      two epochs from 90.5% to 97%.
    * They give `1/sqrt(# of inputs)` as a general rule for the limit.
* They claim that normal is better than uniform, though they don't
  give any justification for that.
    * They suggest truncating, presumably because extreme values means
      some neurons get saturated.
    * They mention this is more useful when you have large networks
      because there are more likely to be extreme weights output.
    * In the small MNIST example they provide, the benefit did not
      materialize.
* They link to the Xavier initialization paper, but also *batch
  normalization*.
    * The batch normalization paper says the typical way to handle
      saturation and exploding/vanishing gradients in deep networks is
      to use ReLU, careful initialization a la Xavier and Glorot, and
      low learning rates.
    * They were able to train an ImageNet classifier in 1/14th the
      number of training steps and get the same accuracy.
    * Of course, this allows you to simply train for longer!
    * So batch normalization looks important. It seems to be that they
      keep the inputs to each layer of mean zero and variance
      one. They also decorelate the inputs.
    * They add in two new parameters which allow you to change the
      mean or variance; but those are learned, and if you don't change
      them, you can be assured that they won't change from changes in
      the output of the previous layer, I believe.

## Transfer Learning

* They're going to take VGG and use it as a feature extractor.
    * You strip off the last FC layer (which was just for softmax
      purposes to the 1k ImageNet classes) and use this as your input.
    * The idea is that VGG has already learned how to detect useful
      features.
    * These features are sometimes called *CNN codes*.
    * You can use your own softmax for your task, or Karpathy suggests
      maybe linear SVM.
    * Karpathy mentions that sometimes you backprop into the last
      couple layers of the VGG. That's because as you get later, the
      features are more specific to the task of ImageNet. The earlier
      layers are more generic. But you don't want to backprop all the
      way, otherwise you'll get overfitting. There's a reason you're
      trying to *transfer* the previous learning.
* Again, I want to note that transfered embeddings like we're using
  VGG for should *reduce* train accuracy.
    * Now, if we port over knowledge, like how to extra useful visual
      features, we may help ourselves get better generalization
      performance.
    * Even if we don't port over knowledge, by forcing the network to
      do a dimensionality reduction, we may get it to generalize
      better.
    * However, I doubt that if we map one-hot vectors to embeddings,
      but our embedding is only learned by the current task, I don't
      think this should help. The reason I doubt this is because the
      one-hot vectors have no concept of what it means to be "similar"
      to each other, except insofar as they have similar output on the
      task. But that doesn't help you generalize to unseen examples
      (because you don't know their similarity to the task)
    * So, for instance, putting an embedding matrix in front of the
      LSTM for the sentiment analysis project, but not using word2vec
      and instead just training it for the given task, seems hopeless!
* TF has an Inception3 transfer learning tutorial. They talk about the
  "bottleneck", which is the penultimate layer and the one that has
  pulled out features for classification. They say: feed forward
  through Inception3 to the bottleneck for each example. Save these
  representations to disk. Now train as usual. Basically: don't feed
  forward through the whole Inception3 each time; that's unnecessary,
  since you won't be adjusting any weights in there. Just train on the
  representation that Inception3 gives you.

## Q-Learning

* So there's a table where the rows are states and the columns are
  actions. The entries are long-term expected rewards.
* The idea presumably is to balance exploration and exploitation.
* Also, I assume you need some way to backpropagate rewards.
* Bellman Equation:

    Q(s, a) = reward(s) + \gamma (max_{a'} Q(s', a'))

* Presumably `s'` corresponds to the state you transition to.
* Gamma determines how much to backprop the reward. Interesting. I'm
  not sure why they don't set it to one?
* They're going to learn by having all zeros in the table.
* When they receive a reward, they potentially update all the previous
  folks on their path if the reward changed the best play value.
* Of course, the problem is that there are too many actions that you
  could take.
* It appears like Q-Learning is pretty tempermental. The problem is
  that the Q-function is being approximated by the neural network, and
  errors in the Q-function approximation are fedback into the system
  because of the recurrence relationship. That leads to potential
  explosion of errors.
    * Karpathy recommends another technique called *policy gradients*.
* Sriraj shows a multi-armed bandit solver using policy gradients. But
  I didn't really follow what he was doing.

* BTW, they had us do a TV Script Generation project but it's exactly
  the same as the Anna Karenina one so I've left it out.

## Sequence-to-Sequence

* You'll have one RNN read in a whole sentence, producing a final
  output, that is then used to a second RNN that will produce a
  translated version.
* These are called *encoder* and *decoder*.
* There is a `tf.contrib.seq2seq.dynamic_rnn_decoder`. It takes in an
  RNN Cell and a decoder fn. When training, you give it inputs, which
  is the output you want.
    * The "input" to the decoder is the last word generated by the
      encoder. What it does next should depend on what it did
      previously.
    * The state of the RNN is also passed forward.
    * When training it is typical to give the right word even if the
      decoder had previously output the wrong word.
* From the TF tutorial, it looks like it is typical for the decoder to
  output to an embedding space, and then a softmax is performed on the
  embedded representation to select the appropriate word. This is
  easier than trying to train the RNN to output exactly the right
  word.
    * They again mention negative sampling as the way to train the
      embedding space.
* The TF tutorial mentions bucketing and padding. Basically, to handle
  the problem of arbitrary length inputs and outputs, they'll have
  `<PAD>` words. They'll also specify "buckets". They'll have buckets
  like `(0...5), (5...10), (10...20)`. The idea is that a length three
  input gets padded to length 5, but a length 13 input gets padded to
  length 20. I am a little confused though, do they train different
  models for each bucket?
    * Another point: the buckets actually have *two* ranges: one for
      the input length and one for the output length. You need to pick
      the bucket big enough for the input *and* the output.
    * It can't be different models for each: when you go to use this,
      it isn't going to know ahead of time how long the output will
      be, will it?
    * I think the padding is just used for batching, actually. That's
      because for efficiency everything in th batch needs the same
      size.
* There are four commonly reserved symbols:
    * PAD (explained above)
    * EOS: which the decoder uses to indicate termination.
    * UNK: this is used to replace low frequency words. However, I
      don't know how that is supposed to help when we need to *output*
      those words... Maybe we'll just try to avoid them? But what
      about proper nouns?
    * GO: This is a dummy symbol fed to the encoder at the first time
      step.
* They note a weird trick of reversing words. The Google Seq2Seq
  Learning with NN paper highlights this as one of their most
  important contributions. They say "it introduces short-term
  dependencies", which I don't think I understand.
* I think a main idea of sequence-to-sequence is that unlike an RNN,
  it doesn't need to output a token at a time (simultaneous
  translation). The decoder RNN makes sense as a way of building out a
  sequence of output. This is different than, for instance, running an
  RNN to read a sentence, and then make a single binary decision of
  whether the sentence is positive or negative.
* I think a perfect example is translation, where you read the whole
  sentence, you've built up an understanding of what it means, and
  then you start generating output in the target language
  word-by-word.
* Sriraj talks about *memory networks* and *dynamic memory networks*.
    * I have basically no idea what these are.
* In v1.1 TF switched the API to `BasicDecoder`, along with
  `TrainingHelper` and `GreedyEmbeddingHelper`.
    * I embed the inputs, then feed this to an RNN.
    * The last RNN state I feed to the decoder.
    * For training, the `TrainingHelper` provides the decoder (1) the
      start symbol, then (2) each word in the sentence, where (3) each
      of these is embedded.
    * It is trained to produce the output sequence, followed by a stop
      symbol. Learning the stop symbol is important otherwise we
      infinite loop.
    * The `TrainingHelper` ensures that the correct prior word is fed
      in at each time step. When performing inference, the
      `GreedyEmbeddingHelper` will take the most likely word from the
      prior timestep, embed it, and then provide this as input at the
      next decoder step.
    * The `GreedyEmbeddingHelper` will also see the stop symbol, and
      will cease decoding when it sees the stop symbol. But if the
      decoder RNN was never trained to produce the stop symbol, the
      `GreedyEmbeddingHelper` will let your decoder RNN run forever.
    * You use `dynamic_decode` to produce the `DecoderOutput`;
      `DecoderOutput#rnn_output` is the sequence of decoder
      productions.
    * Using this, you can use `sequence_loss`. I'm not exactly sure
      how you would do a sampled softmax loss with `sequence_loss`; I
      think you'd have to write that yourself. You'd want to if the
      vocabulary is large.
* Sriraj has another video about attention. Basically, if you want to
  do seq2seq, and the entire sentence is crammed into a single final
  LSTM state, you're asking a lot. You could increase the number of
  LSTM units, but that is going to be inefficient. I assume that for
  early steps you can't really use all that state.
    * So they use attention. I have to read more about this
      elsewhere. Basically, the LSTM state says how much to look at
      prior encoder outputs.
    * They further improve performance by using a *bidirectional
      RNN*. Basically, this uses context from both sides by running
      *two* RNNs. The outputs of the bidirectional RNN is the pair of
      outputs from both sides.
    * Google used 1 bidirectional layer, than 7 unidirectional layers
      for the encoder. They used 8 unidirecitonal decoder layers.
    * Note: they can do unidirectional layers in parallel.
