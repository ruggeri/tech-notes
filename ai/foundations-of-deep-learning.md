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
  tf.global_variables_initializer()` and do `sess.run(init)`. Like so:


```
import tensorflow as tf
from tensorflow.examples.tutorials.mnist import input_data

def get_weights(n_features, n_labels):
    """
    Return TensorFlow weights
    :param n_features: Number of features
    :param n_labels: Number of labels
    :return: TensorFlow weights
    """
    return tf.Variable(tf.truncated_normal((n_features, n_labels)))


def get_biases(n_labels):
    """
    Return TensorFlow bias
    :param n_labels: Number of labels
    :return: TensorFlow bias
    """
    # TODO: Return biases
    return tf.Variable(tf.zeros((n_labels)))


def linear(input, w, b):
    """
    Return linear function in TensorFlow
    :param input: TensorFlow input
    :param w: TensorFlow weights
    :param b: TensorFlow biases
    :return: TensorFlow linear function
    """
    return tf.add(tf.matmul(input, w), b)

def mnist_features_labels(n_labels):
    """
    Gets the first <n> labels from the MNIST dataset
    :param n_labels: Number of labels to use
    :return: Tuple of feature list and label list
    """
    mnist_features = []
    mnist_labels = []

    mnist = input_data.read_data_sets('/datasets/ud730/mnist', one_hot=True)

    # In order to make quizzes run faster, we're only looking at 10000 images
    for mnist_feature, mnist_label in zip(*mnist.train.next_batch(10000)):

        # Add features and labels if it's for the first <n>th labels
        if mnist_label[:n_labels].any():
            mnist_features.append(mnist_feature)
            mnist_labels.append(mnist_label[:n_labels])

    return mnist_features, mnist_labels


# Number of features (28*28 image is 784 features)
n_features = 784
# Number of labels
n_labels = 3

# Features and Labels
features = tf.placeholder(tf.float32)
labels = tf.placeholder(tf.float32)

# Weights and Biases
w = get_weights(n_features, n_labels)
b = get_biases(n_labels)

# Linear Function xW + b
logits = linear(features, w, b)

# Training data
train_features, train_labels = mnist_features_labels(n_labels)

with tf.Session() as session:
    session.run(tf.global_variables_initializer())

    # Softmax
    prediction = tf.nn.softmax(logits)

    # Cross entropy
    # This quantifies how far off the predictions were.
    # You'll learn more about this in future lessons.
    cross_entropy = -tf.reduce_sum(labels * tf.log(prediction), reduction_indices=1)

    # Training loss
    # You'll learn more about this in future lessons.
    loss = tf.reduce_mean(cross_entropy)

    # Rate at which the weights are changed
    # You'll learn more about this in future lessons.
    learning_rate = 0.08

    # Gradient Descent
    # This is the method used to train the model
    # You'll learn more about this in future lessons.
    optimizer = tf.train.GradientDescentOptimizer(learning_rate).minimize(loss)

    # Run optimizer and get loss
    _, l = session.run(
        [optimizer, loss],
        feed_dict={features: train_features, labels: train_labels})

# Print loss
print('Loss: {}'.format(l))
```

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

```
# Solution is available in the other "solution.py" tab
import tensorflow as tf

output = None
hidden_layer_weights = [
    [0.1, 0.2, 0.4],
    [0.4, 0.6, 0.6],
    [0.5, 0.9, 0.1],
    [0.8, 0.2, 0.8]]
out_weights = [
    [0.1, 0.6],
    [0.2, 0.1],
    [0.7, 0.9]]

# Weights and biases
weights = [
    tf.Variable(hidden_layer_weights),
    tf.Variable(out_weights)]
biases = [
    tf.Variable(tf.zeros(3)),
    tf.Variable(tf.zeros(2))]

# Input
features = tf.Variable([[1.0, 2.0, 3.0, 4.0], [-1.0, -2.0, -3.0, -4.0], [11.0, 12.0, 13.0, 14.0]])

# TODO: Create Model
hidden_layer = tf.add(tf.matmul(features, weights[0]), biases[0])
hidden_layer = tf.nn.relu(hidden_layer)
output = tf.add(tf.matmul(hidden_layer, weights[1]), biases[1])

# TODO: Print session results
with tf.Session() as session:
    session.run(tf.global_variables_initializer())
    print(session.run(output))
```

I've copied multilayer_perceptron.py to show how easy it is to setup
with TF.

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
