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
    * Note why increasing LSTM units is inefficient. Because as you
      increase LSTM units, you increase number of weights
      *quadratically*. That's because LSTM from a prior timestate
      effects LSTM of next timestate.
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
* In the Deep Learning School lecture Quoc Le mentions that it is
  common to do a beam search when decoding. You pick the response with
  the highest overall probability. This is different from a greedy
  decoding, which commits at each timestep to the currently mostly
  likely next word, but can paint itself into a corner.
* Quoc also mentions scheduled sampling, which is the thing where on
  training you slowly start feeding in the actually produced output
  (rather than the target output), so that the decoder can learn not to
  go off the rails too much.
* Quoc also notes that theoretically you can use seq2seq for
  transcription, but this doesn't seem to work very well.
* Quoc talks a little about memory vs attention. Imagine you are asked
  a question about a book. Attention says: go back through the book
  and find out what part is relevant. On the other hand, memory would
  store facts from the book in a memory store that is retrievable
  later.
    * Quoc mentions that an alternative is to use reinforcement to
      decide a single place in the text to look at? But how can that
      work with variable length inputs?
* There are also neural machines where you give them some operations,
  and they can push operands into a stack, and they can decide
  probabilistically whether to perform an operation.

Source: https://www.youtube.com/watch?v=G5RY_SUJih4

## More Attention Mechanism

* Let's think about an image. Rather than doing one summary of the
  image and then feeding it to a decoder, let's instead create
  summaries of many sections.
* Then, we take these section summaries plus the current RNN
  state. Each section summary is hooked into a hidden layer with the
  state. This gives scores for each section, which go through a
  softmax. These proabilities are how much attention to give to each
  section summary.
* We then take a weighted sum of the section summaries. This is the
  input to the decoder at the next timestep.
* This process is differentiable and is called *soft attention*. There
  is another approach called *hard attention*. Here the system samples
  a single part of the image.
    * This is not easily differentiable. You can do a kind of Monte
      Carlo thing to calculate a derivative.
    * It appears that there is not much performance difference, so the
      focus is on soft attention.
* As mentioned above, you can do this for sentences and
  translation. It is common to use a bidirectional RNN.
    * This process is sometimes called *alignment*, because when the
      decoder goes to generate the next word, it focuses on some part
      of the encoded sentence, and thus the newly output word reflects
      the preivously encoded state focused upon.
* There seems to be consensus that the problem is memorizing the whole
  sentence; especially the earliest bits, and also cramming this in
  one vector.
    * They mention that reversing the sentence for encoding helps
      because early parts of the sentence come last and thus closest
      to the beginning of the encoder. But it seems a little odd
      because then later words are even further away!
    * They also note that performance can be better if the encoder
      reads the sentence *twice*!
* Seems like Cho from Bengio's lab is the one who owns this field.
* End-to-End memory and Neural Turing Machines are always mentioned in
  the same discussion of attention and I've listed them to study more.
* There is a note that with attention, if we output `<unk>`, we can
  replace this with the word of input that is most attended; hopefully
  it's like a placename or other proper noun.
* A few notes about GNMT (Google Neural Machine Translation):
    * The final encoder state doesn't feed directly into the decoder
      anymore. For simplicity, the decoder is initialized presumably
      with zeros or randomly (presumably can be learned). The only way
      encoded information is accessed is by attention.
    * They also only use a bidirectional RNN on the first layer. That
      lets them parallelize computations in subsequent
      layers. Otherwise, you have to wait for an entire bidirecitonal
      layer to finish before you can start computing the next layer.
    * They use "teacher forcing", which is the technique where when
      training you give the "correct" translation word, not the one
      that was actually selected.
    * They use *residuals*. Basically, each layer adds a delta to the
      output of the last layer. This makes it easy to learn the
      identity function.

Source: https://blog.heuritech.com/2016/01/20/attention-mechanism/
Source: http://www.wildml.com/2016/01/attention-and-memory-in-deep-learning-and-nlp/
(Excellent) Source: *Neural Machine Translation and Sequence-to-sequence Models: A Tutorial*
    https://arxiv.org/pdf/1703.01619.pdf

## More Augmented RNNs

* Neural Turing machines have memory.
    * At each timestep the can read and write.
    * It's a soft read and a soft write, of course. Based on attention.
    * Where to attend? First, you do *content-based* attention: you
      have a query vector, and you pick those cells that are most
      similar (easiest to do with dot product). You then can mix this
      with the previous attention vector; the mixing ratio can be
      learned and dependent on the current RNN state.
    * Next, you can apply a *shift filter*. This allows *location
      based attention*. The shift filter does a 1d convolution and
      basically lets you move forward one from the previous time
      step. This is good for looping.
    * It looks like Sutskever is the one trying to use
      non-differentiable memory access and reinforcement.
* Adaptive computation time
    * It appears that you basically have a "halt bit." You continue to
      process until the halt bit falls below a threshold.
    * This lets the machine work harder at some time steps than
      others. Interesting.
    * This appears to be very new and I'm not sure when it would be
      very useful.
* Neural Programmer
    * You give it access to functions that it can use.

Source: http://distill.pub/2016/augmented-rnns/

## End-to-End Memory Networks

* Simple example. Take the facts and featurize them into `m_i` and
  `c_i`. Consider a query; featurize this as `u`. Now, take the dot
  product of `u` and `m_i`; using a softmax, we get a distribution
  over the memories. So take the weighted sum of the `c_i`, which are
  the content.
* The facts are normally sentences for a Q-and-A system.
* TODO: I was looking at some of the papers about this but they are
  quite unclear...

(I added these sources a bit later: I think they're releavnt)
Source: https://arxiv.org/pdf/1506.02075.pdf
(This one is about dynamic memory networks; seems very similar)
Source: https://arxiv.org/pdf/1506.07285.pdf

## Autoencoders and Variational Autoencoders

* Basically, a bottleneck layer, and you try to reconstruct the input
  from the bottleneck layer.
* But these are worse at compression than other traditional
  techniques, and they do poorly at generalization, apparently.
* But they appear to be good at dimensionality reduction and image
  denoising.
* They did succeed to take a 28x28 image and compress it to 32
  float32s. It's kinda impressive actually.
* To do denoising, they train on the noisy images. I guess the idea is
  that the network says: I don't know how to capture this noise, but I
  can capture the signal.
* As an example we did an autoencoder on MNIST with a *ton* of
  Gaussian noise added.
* A motivation for autoencoders: this is an unsupervised method, and
  we want to be able to learn in a world that has tons and tons of
  unlabeled data. Autoencoders are learning a representation of the
  data, based only on their desire to "simplify" their understanding
  of the data. But I guess a question is: how will we leverage this
  understanding to do tasks?
    * Say we have a semi-supervised setting: some data is labeled, but
      the vast majority is not.
    * So we use the unlabeled data to learn a representation.
    * We then try to learn a model for the labels using this
      representation.
    * Our hope is that the learned representation accurately
      summarizes those features which are important for the task.
    * That way, maybe we only need to use a small number of parameters
      for the supervised task (it's okay to use more parameters to
      transform the input to the representation; we have lots of data
      for that task).
    * Also, hopefully this allows for good generalization from the
      labeled data to the larger dataset.
    * This kind of semi-supervised learning is how the brain
      works. Geoff Hinton has said that there isn't enough supervised
      labels that you can collect over a lifetime. The only place to
      get enough usable data is from the input itself.
* Sriraj talks about variational autoencoders.
* I think the basic idea is this:
    * Your autoencoder is supposed to be reducing the dimensionality
      of the data.
    * One way to reduce the capacity is to reduce the number of units.
    * You can also perform regularization.
    * An alternative to regularization is to use a prior
      (regularization is often *equivalent* to some kind of prior).
* Here is the idea.
    * You want a simple model with which you can understand `x`. That
      means "reducing" `x` to a lower-dimensional latent space `z`.
    * Now, you know that your model won't be perfect, and won't be
      able to understand all the factors that cause `x` to take on its
      value. So the `x`s will look sort of random to any model that we
      come up with, because we have limited understanding. But a
      pretty good model should have `P(X | Z=z)` have low
      entropy. Otherwise, your `z` isn't telling you much about `X`!
    * To be useful, you need to be able to map observed `x`s to the
      latent space. That involves computing `P(Z | X=x)`. Again, if
      this has high entropy, then we won't have any idea what the true
      value of `Z` might be, and our latent representation is useless.
* We will use neural networks to compute these distributions.
    * The *encoder* will take an `x` sample and spit out the
      parameters of the conditional distribution of `Z`.
    * The *decoder* will take a `z` sample and spit out the parameters
      of a conditional distribution of `X`.
* Now, if we did a maximum likelihood thing, then the system might try
  to do this: learn an encoder that makes `P(Z | X=x)` be *super
  specific*, so that `z` is predicted extremely strongly by `x`. Then,
  make the decoder super sensitive to `z`, so that it produces just
  the right output.
    * Granted, we should be able to limit this somewhat by limiting
      the capacity of our model, which means using a small neural
      network size.
    * If you think about it, I believe this is what a standard
      autoencoder does. It maps a value to a *single exact latent
      representation*.
* The variational autoencoder approach is going to try to use a
  Bayesian approach. It will adopt a prior on `Z`: a spherical normal
  distribution.
    * This is probably a reasonable prior no matter the task, because
      even though the `P(X)` is not normally distributed, this
      presumably arises from the non-linearity of the network.
    * In other words, perhaps this is saying "All fundamental,
      Platonic qualities are normally distributed, and any non-normal
      quantities in the universe arise from non-linear systems."
    * That might be a reasonable amnd resilient basis for most
      learning.
* In the VAE literature, the encoder is sometimes called the inference
  *network* and the decoder is called the *generative network*.
* One thing that feels weird: the prior is on the distribution of the
  latent values: not the parameters of the networks!
* I believe the reason they call this *variational* is because the
  encoder and decoder functions have been chosen from a family of
  distributions (defined by the possible settings of the neural
  networks). SGD is finding the best approximation it can amongst this
  family.

## Generative Adversarial Networks

* Uses:
    * Text to photorealistic images.
    * Squiggles and lines in Adobe illustrator to image.
    * Apparently there's a tool to do this for drawings of cats.
    * Blueprints to a rendering of the building.
* They say that since GANs try to impersonate, they can be used to
  mimic human actions. They have also been used to do physics
  simulations (claim savings of millions of dollars).
* The idea is this. You have a generator model, and it gets feedback
  on how it is doing from a discriminator network.
* The generator is being trained in an adversarial way.
* You can have a single value function and have the discriminator try
  to maximize it at the same time the generator tries to minimize
  it. Eventually you should find yourself at a Nash equilibrium. The
  natural value function is the percentage of correctly classified
  examples, or rather the cross entropy.
* The best discriminator outputs a guess based on the density of the
  true distribution at a point versus the generator distribution.
    * Therefore, the best that the generator can do is exactly mimic
      the distribution of the original. Then the densities are exactly
      the same, and the discriminator can only get a CE loss of 1 nat.
* So here's a problem with iterative training.
    * The generator proposes only values at one mode of the true
      distribution.
    * Now the discriminator will hate that image and always reject it,
      even though those are actually good images!
    * The generator will then move on to choose another mode!
    * This continues with no convergence!
* They say that leaky ReLU is more common for GANs because this
  ensures signal can get all the way back to the generator, through
  the discriminator.
    * It appears common to use a tanh as the generator output layer.
    * The discriminator needs a sigmoid.
    * For loss, the discriminator will use the CE. And the generator
      will use the CE with the labels switched. This works better than
      the negative of the discriminator's CE.
    * Why? It seems like the generator wants to beat the discriminator
      by making it use lots of nats. But the problem is about how the
      derivative behaves as the logits approach 1.0.
    * Basically: CE has nice properties when you're *minimizing it*.
    * Note: a point that minimizes the "deceived CE" must maximize the
      "true CE", and vice versa. So these criteria specify the same
      optima, but the gradients are different.
* To generate images, Goodfellow suggests transposed convolution with
  a stride. But maybe you should do the resize plus regular
  convolution as discussed previously.
* Goodfellow recommends batch normalization for GANs.
* Summary:
    * Autoencoders learn a representation that allows reconstruction
      of an image. We can sort of abuse this, possibly, by inputting
      an image that is *not* from the original distribution, and thus
      maybe giving it features of the original distribution.
    * Alternatively, shouldn't you be able to input random noise into
      the latent layer?
    * I suppose that might not work, because the network may expect
      there to be correlations in that layer. Though ideally it should
      not, as that would be an inefficient representation.
    * GANs are fed noise, which the generator will think of as a
      latent representation of some image.
    * It gets to decide what image that latent representation means.
    * It will try to produce images that correspond to the unlabeled
      data distribution. It will not directly see any of the unlabeled
      data.
    * Basically, the GAN is producing random images, and asking: how
      close to an original image was I?
    * I don't understand at the moment how GANs can create new images
      with properties that you desire?
    * I suppose, if you do you have labels or descriptions, you can
      feed this to both generator and discriminator. Now, the
      discriminator is being asked: "Is this an image of a zebra?"
    * Now, you should be able to ask the generator to give you "zebra
      with wings" or some craziness.
    * The idea being that if it learns the underlying concepts, it
      might be able to mix these.
* Why does the GAN not just learn a single really good example image?
  The reason is that even if this is very good, the discriminator will
  learn to reject it!
    * So the GAN is incentivized to not just produce a small number of
      good examples of the real distribution, but truly to emulate the
      entire distribution.
* Here's another idea.
    * Imagine a text generator that was learning to caption images.
    * A truly supervised task would be to say: did you generate the
      exact caption that this image was labeled with?
    * But then you could generate a perfectly reasonable description,
      but it's not the same. "It is a yellow bike with black tires and
      brown handlebars" is different from "There are two black wheels
      and leather handlebars and a yellow frame."
    * So maybe the GAN is good at giving "partial credit". That seems
      more reasonable.

**Paper notes**

* They highlight Boltzman machines or deep belief nets as models that
  have an undirected component. But inference in that setting is very
  hard.
* They say instead of the generator trying to maximize `-log(1 -
  D(G(z)))`, it provides stronger gradients to minimize
  `-log(D(G(z)))`. They note, as above, that this has the same "fixed
  point dynamics": that is, minima.
    * They say this is important early on when it is very very for the
      discriminator to recognize the generated images, so that there
      are very weak gradients for the generator to do better.

Source: Generative Adversarial Nets (Goodfellow)
    https://arxiv.org/pdf/1406.2661.pdf

* They mention explicitly the idea of annotation of images which is
  one-to-many, in the sense that many possible outputs are reasonable.
* But not a lot profound here...

Source: Conditional Generative Adversarial Nets (Mirza, a Montreal person)
    https://arxiv.org/pdf/1411.1784.pdf

They note that generative modelling is a big part of unsupervised
learning. They say that the idea is that if you can generate, then you
probably have learned a good disentangled representation. They sound a
little skeptical. They note VAEs and GANs are the common techniques
here.

They assume the utility is in using the disentangled version
downstream.

They are going to show an approach that they say the quality of the
disentangled representations they learn are as good as when given
supervised information. Their technique is going to try to push the
GAN to learn a particularly good representation.

Here's what they do. In addition to `z`, they also feed in `c` (these
are both noise generated). The generator produces an output. The
difference from a vanilla GAN is that the generator is given a bonus
when `I(c ; x)` is high; that is, when `x` tells you a lot about the
code `c`. That basically says: I want the `c` to be high-level
semantic information, and the `z` to be what adds in random noise and
variation that are less semantically meaningful.

Let's go through this. They want to minimize the conditional entropy
`H(C|x)`. Without access to `P(C|x)` they cannot find this.

Now, `H(C|x)` is measured using the best enconding. If we used the
wrong encoding from a probability distributino `Q(C|x)`, this could
only do worse. Let us define `H' = E_{c \tilde
P}[-log(q(c|x))]`. Minimizing establish an upper-bound on the true
conditional entropy `H(C|x)`.

TODO: They do some fancy math regarding some kind of variational
statistics thing. I understand the idea: they're trying to capture the
most meaningful information in these variables. But I need to study
some statistics to understand this.

They were able to do some pretty fancy stuff with this. In an entirely
unsupervised way, they were able to learn the "angle" of a chair. That
means they could give the same `z`, but manipulate `c` to spin the
chair around in 3d space.

Source: InfoGAN: Interpretable Representation Learning by Information Maximizing Generative Adversarial Nets (Xi Chen, Sutskever, Abbeel)
    https://arxiv.org/pdf/1606.03657.pdf

Mentions some uses of generative models. Can generate time-series
future outcomes, so can be used for reinforcement learning. Or could
generate an imaginary environment to learn in. You can fill in missing
data; you can do semi-supervised learning. Generative models are
better at "multi-modal" output. For instance, rather than predict just
one outcome, which is smoothed out, you can generate a couple
samples. Another example is superresolution or art generation.

They go through some alternatives. The first set of alternatives have
explicit density models. A subset have tractable, explicit density
mdoels. An example is *fully visible belief networks*, which basically
generates a sample `x` one coordinate at a time, conditioned on all
prior coordinates. The probability distributino of the next coordinate
is conditioned on the prior coordinates. This was done with WaveNet by
DeepMind, and this is very succesful at generating speech, but it
takes 2min to generate 1sec. The downside is speed.

PixelRNN also by DeepMind works this way too, and inspired WaveNet.

Another approach is to set `p_x = p_z(g\inv(x)) * deg(jacobian of
g\inv at x)` where `z = g\inv(x)`. But that means you need a
one-to-one `g` mapping `z` into `x`, which means you need the same
dimensionality. And to tractably invert `g` could be hard; you also
need to invert the jacobian. This limits your choices of `g`.

Reminder: we want to find `p_x` for the model so that we can make sure
the model is maximizing the likelihood of the produced data. Even if
we want to produce samples, we presumably want some belief that this
corresponds with the observed data.

Another approach is to use a model which does explicitly give a
density to the observed, but where this density can only be
approximated. Variational autoencoders are an example. Here, we find
some approximation that is a lower bound for the likelihood; we then
maximize this lower bound. The problem, of course, is that if the
lower bound is not tight enough, we will not maximize the true
likelihood, and thus learn the wrong probability
distribution. Apparently this is possible even in the limit of data.

They say that, subjectively, GANs seem to produce better looking
results than VAEs. They note that FVBNs are easier to train than VAEs,
but that GANs are also hard to train.

Boltzman machines require MCMC both for training and generation. This
is the other direction than variational: MCMC. But MCMC is slow and
doesn't scale well to many dimensions. It is also hard to tell when it
has mixed, so that the samples are representative of the model. They
note that Boltzman machines are seldom used now because of these
problems.

Which brings us to implicit density models. Generative stochastic
networks generate samples through a Markov chain method. They don't
explain GSNs, but say that they are slow.

In summary. GANs are faster than FVBNs because they don't require
generating a coordinate at a time (can run in "parallel"). Boltzman
machines are slow and can only use certain distributions because those
allow MCMC to be tractable. GSNs are presumably also troubled by
MCMC. Doesn't rely on variational bounds.

The big downside: GANs can be difficult to train because they need to
find a Nash equilibrium, not just a minimum.

They then talk about how GANs work. They note that you can inject `z`
anywhere in the network. You can put some of it as input to the first
layer, and then you can use some more in a middle layer. That's
interesting; not sure if I see that as highly valuable, though...

They acknowledge the issue with the gradients. They acknowledge that
it is theoretically pleasing to use a single value function.

They note that GANs are sort of like reinforcement learning. But the
generator gets to see the gradient of the reward. That sounds good,
but the downside is the reward is non-stationary.

They note that if you do arithmetic on the latent representations, you
tend to get reasonable results. For instance, if you take the latent
representation for a man with glasses, then subtract a latent
representation for a man, then add a representation of a woman, you
will generate a woman with glasses.

The give some tips and tricks. Incorporating label data seems to help:
even if only the discriminator can see the labels! It's not entirely
clear why that would help. They note label smoothing: they say this
helps because it keeps the discriminator from becoming too hyper
confident. Hyperconfidence is exploitable by a network constructing
adversarial examples. They do note that you shouldn't smooth the fake
labels.

Source: NIPS 2016 Tutorial: Generative Adversarial Networks (Goodfellow)
    https://arxiv.org/pdf/1701.00160.pdf

* For conditional GANs, they didn't find it useful to use the noise
  vector; this ended up being ignored by the generator.
* They note conditional GANs tend to be convolution-deconvolution
  encoder/decoder approach. They also add *skip connections*, where
  the `i`th level is attached to the `n-i`th level. The idea is to
  always make the original image available, and not force everything
  through a really narrow final convolution layer.
* They note an important idea. Let's say you want to do image
  segmentation. Then, the network will probably output a segmentation
  that is "blurry", because it wants to be fairly correct. It sort of
  "averages over" the possibilities.
* But when the possiblities are truly multi-modal, then this creates a
  lot of blur. A discriminator of a GAN could see this blur and
  penalize the network. So the network is biased to hallucinate a
  less blurry segmentation.

Source: Image-to-Image Translation with Conditional Adversarial Networks (BAIR people I don't know)
    https://arxiv.org/pdf/1611.07004v1.pdf

## One Shot Learning

* Sriraj talks about Neural Turing Machines. So there's an LSTM which
  is the controller.
* And he talks about attention and reading/writing all the items, so
  that we have full differentiability.
* Mentions content based and location based attention.
* They did a "one-shot learning" task. This is character recognition
  with just a couple characters per class. It was very successful.
* But why?

Paper notes:

* They describe a Differentiable Neural Computer. They claim it is a
  little different than a Neural Turing Machine.
* First attention mechanism is content-based.
    * The lookup is key-value. This allows references.
* They also have an attention mechanism that records a matrix
  `L[addr1, addr2]`, which is `~1.0` if `addr2` is written after
  `addr1`, otherwise close to zero. This allows replay of sequences of
  writes. This works even if they were written at different timesteps.
* They specifically practiced on graph problems: like find the
  shortest path. The performance was much better than LSTMs.
* It appears that the big difference from the NTM is that while the
  NTM has address based addressing so that you can access a sequence
  of values, this has the matrix to store what was modified after what.
* Another problem: the NTM had no memory allocation strategy to keep
  it from overwriting other "allocations" of memory. So they have a
  memory allocation concept.

Source: Alex Graves, Demis Hassabis
    http://www.readcube.com/articles/10.1038/nature20101?shared_access_token=3UerOr1f0fy3oL_CytWdxtRgN0jAjWel9jnR3ZoTv0MggmpDmwljGswxVdeocYSujsARxGW1q2qxK0cTqi1Bup-nSH200cGUW_ET9MIG_6rvvXTcoxOnAX6B4E8dQs4FQ-yScxXe8EB0XnzqbUw3Qw%3D%3D

* One-shot learning tries to learn from just one, or sometimes a few,
  examples. Most methods require many many examples. And they can't do
  online learning well; if new categories are introduced, they need to
  totally reorganize themselves. The idea is that memory lets you
  record new experiences and do self-modification.
* They note that it is characteristic of human intelligence to have
  very new behaviors based on very little new information. For
  instance, we hear a word a couple times and know what it means.
* What they want to explore is *meta learning*. Here, they mean that
  over a long time you learn how to learn. But in a single task, you
  learn quickly, applying the strategies you've learned.
* (BTW, it looks like Alex Graves is active in this area).
* They note that while LSTMs, sort of address the idea of memory, they
  have trouble stably recording data. They have difficulty being
  selectively read by address. And also, they have a fixed amount of
  memory available and constantly in use.
* They call out both NTMs (Graves) and memory networks (Weston) as
  examples. They see the main idea as this: memory augmented networks
  have access to *external* storage, rather than *internal* memory
  like LSTMs.
* They describe a regime where you have a series of different
  tasks. The network sees an example, makes a guess, sees the label,
  repeat. The idea is to learn over the course of the task and
  maximize accuracy over the course of the task.
* Here's an example of what they were able to accomplish.
    * They do 100k "epsidoes". Each episode uses 5 image classes. The
      machine needs to learn how to learn how to recognize the image
      classes.
    * Now it's time to test! Again, the network is given 5 random
      image classes: never seen before!
    * Within an episode, the network accurately identifies the 2nd
      instance of a never-before seen class at 82% accuracy. It has
      95% accuracy after five examples.
    * So the network has clearly learned how to learn to a certain
      extent.
    * Performance is superhuman.
    * They scaled up to fifteen classes. They also used five character
      string labels.
* Fascinating, fascinating paper!

Source: One-shot Learning with Memory-Augmented Neural Networks (DeepMind people I don't know)
    https://arxiv.org/pdf/1605.06065.pdf

There is also *zero-shot* or *zero-data* learning.
https://pdfs.semanticscholar.org/00d3/fadfadc977ba4b6511bec1b2a3026d877099.pdf

## Hyperparameters

* Learning rates: Bengio suggests they're the most important
  parameter.
* They mention learning rate decay and adaptive learning rates.
* They mention that larger size can more efficiently use computational
  resources because vectorization. But you can run out of memory. They
  suggest up to 256 examples as reasonable.
* They mention that some of the jitter of minibatching can mean you
  won't get so stuck in local minima.
* They mention early stopping.
* Karpathy mentions that deeper than 3 layers seems not to help very
  much for conventional NNs. He notes that is unusual as deep
  convolutional nets can be very helpful.
* Mention that if training accuracy exceeds validation accuracy
  greatly, then too many units, or you need dropout or regularization.
* LSTM and GRU seem to run about even, though my understanding is that
  GRU is more efficient. They indicate that 2 RNN layers may be
  plenty, and that more can give mixed results.
    * They do mention that LSTM or GRU is much more common than
      vanilla RNN.
* I take all this advice with a big grain of salt, because I think it
  probably just really depends on the task.
    * E.g., some speech recognition tasks do better with five or seven
      layers.
* They also suggest that an embedding size of 500 is plenty. Some
  papers don't see improvement after 50 dimensions, while others see
  incremental improvements up to 200.
* Not a very insightful lesson.

## DCGANs

* Looks like Ian Goodfellow was supposed to do this and some content
  from him will be added later?
* We're going to do transposed convolutions with no pooling and with
  batch normalization. Transposed convolutions have a stride of two in
  the output and generate more layers than they start with.
* The discriminator will again do striding and not use max pooling.
* Batch Normalization
    * They're introducing it because it seems needed to make DCGANs
      work well.
    * Allows higher learning rates, makes initialization easier.
    * Some activation functions that you can't otherwise use become
      viable again. For instance, sigmoids are hard to use for RNNs,
      but if you're learning the gain separately, then it should be
      okay.
    * They suggest that it shouldn't actually make your networks
      perform better, just train much faster. I believe you would
      verify this by noting that it doesn't increase the
      representational power of the network.
    * But they mention you can get better results by training more
      different kinds of models faster.
* As mentioned in the Deep Learning book, you batch normalization
  before the activation. That makes sense because normalization is
  about linearity, and so you do it before the non-linearity.
* They do some tests. Batch normalization is quite a bit slower per
  example. On a four FC layer network for MNIST, it ran 2x-4x
  slower. But there was much more rapid and steady progress toward
  convergence.
    * With batch normalization, on MNIST you cross 80% accuracy 6x
      faster than without.
    * I suspect if you do GPU training batch norm is quite a bit
      faster. Also, I think the benefits are probably greater on
      deeper and especially recurrent networks.
* The difference is extremely dramatic with a sigmoidal activation
  function. Here batch normalization results in a 45x speedup.
* They show that using a high learning rate can rapidly accelerate
  training. This can be dangerous with ReLU normally because too many
  die out. But BN did perfectly fine.
* BN was much more resilient to poor intialization of weights,
  training in scenarios where there was no ability to learn without
  BN. They note that high learning rates and ReLUs seem to not mix at
  all.
* So let's talk about how to do BN in TF. The hardcore way is to use
  `tf.nn.batch_normalization`.
    * This simply does `(gamma(x - mu)/sigma) + beta`. You give
      tensors for each of these (plus a variance epsilon value.
    * Of course, gamma and beta can be trained by the model. But you
      need to supply mu and sigma. How?
    * You can create variables for them. You should make them not
      trainable.

```
def bn(z, training_mode):
    # Will be used at test time. TF shouldn't train these; they are
    # computed!
    pop_mean = tf.Variable(
        tf.zeros_like(z), trainable = False
    )
    pop_variance = tf.Variable(
        tf.ones_like(z), trainable = False
    )

    def train_helper():
        # Will be used for training this batch.
        batch_mean, batch_variance = tf.nn.moments(z, (0,))

        # The operations update the population mean/variance for future test
        # time use.
        update_pop_mean = tf.assign(
            pop_mean, pop_mean * decay + batch_mean * (1 - DECAY)
        )
        update_pop_variance = tf.assign(
            pop_variance, pop_variance * decay + batch_variance * (1 - DECAY)
        )

        # control_dependencies will make sure these are updated even
        # though they aren't necessarily required by the graph
        # structure.
        with tf.control_dependencies([update_pop_mearn, update_pop_variance]):
            return tf.nn.batch_normalization(
                x = z,
                mean = batch_mean,
                variance = batch_variance,
                offset = beta,
                scale = gamma,
                variance_epsilon = EPSILON
            )

    def test_helper():
        return tf.nn.batch_normalization(
            x = linear_output,
            mean = pop_mean,
            variance = pop_variance,
            offset = beta,
            scale = gamma,
            variance_epsilon = EPSILON
        )

    # This says do one if in train mode, else the other.
    return tf.cond(training_mode, train_helper, test_helper)

```

* Let's mention why you can't just use the batch mean/variance at test
  time. In the extreme example with one example, every `z` value will
  be set to zero because of mean normalization!
    * Regardless, this isn't what you want. The idea of the batch
      normalization is that the batch is a good estimate of the
      current mean/variance, and you want to make changes while
      controlling for that.
    * But when doing inference, there are no proposed changes. Here,
      you might as well just use the overall estimate of the
      mean/variance.
* Alternatively, you can just use `tf.layers.batch_normalization`
    * Behind the scenes, there are still now variables used for
      updating population means/variances.
    * So, you'll want to do `tf.control_dependencies` with
      `tf.get_collection(tf.GraphKeys.UPDATE_OPS)`.
* When training convnets, remember that you don't want to just batch
  normalize per `(x, y, layer_idx)`, but actually just per
  `layer_idx`. That's because all the pixels in a layer are produced
  by the same operation.
    * You can accomplish this with `batch_mean, batch_variance =
      tf.nn.moments(conv_layer, [0,1,2], keep_dims=False)`
* Likewise, you can use batch normalization for an RNN.

* I wrote up a project where I implement batch normalization for a
  very deep convolution network for MNIST.
* And I also implemented code that uses BN and a DCGAN to generate
  SVHN samples.

* Ian Goodfellow describes one way to use GANs for a semi-supervised
  task. Train the discriminator to predict whether an output is
  fake, or if not what output class it belongs to.
    * You're going to eventually throw the generator away but keep the
      discriminator.
    * The loss function is the cross entropy with the labels for real
      inputs, plus the CE for if it thinks a fake image is real at
      all.
    * He mentions that "feature matching" is important, but doesn't
      really describe what that means.
    * This video was effectively useless right now, because there
      isn't enough description.

* For the project, I used max pooling and `resize_nearest_neighbor`
  and felt like I got better results for the face generation project.
* I found this project very demoralizing, actually.
