* Projects:
* 1. Find lane markings.
* 2. Behavior copying (copy a human's driving behavior)
* 3. Advanced lane finding/vehicle detection.
* 4. Sensor Fusion (combining information from different kinds of
  sensors)
* 5. Localization
* 6. Control
* 7. Path planning: how to take a series of steps to accomplish an
  action.

* Some basic exercises where we use thresholding on pixel intensity +
  a geometrically defined focus area to detect lines.
* It has you play with basic Canny edge detection.
    * They describe that you select pixels above a high threshold, and
      throw out those below a low threshold. Then you keep those in
      between if they are connected to strong edge pixels.
    * Canny recommends a 1:2 or 1:3 ratio.
    * They do some smoothing to eliminate spurious gradients caused by
      noise. The Canny function from OpenCV actually does this, but
      you can't parameterize how much to smooth by.
* They talk about "Hough space". This is a space of `(slope,
  intercept)`. Each point represents a line in the original space.
    * The set of all lines that pass through a point `x` are
      represented as a line in the Hough space.
    * The set of lines through `x` and the set of lines through `y`
      must have exactly one line in common (the one through both), and
      thus are intersecting and non-parallel.
* So what they're going to do to find lines is do Canny edge
  detection, then try to find those points that many lines through
  *other* points pass.
    * BTW, because slope could be infinite for some lines, they're
      going to parameterize as `(theta, rho)`. They want to consider
      the line perpindicular to the vector described by theta and rho.
    * Their claim is that if we do this, then points in the image
      space become sine waves in the Hough space.
    * I haven't done the math on this, but it makes some intuitive
      sense.

**Edge Detection Aside**

From wikipedia, it looks like Gaussian smoothing is common. Then you
calculate discrete partials in `x` and `y`. This involves looking at
the pixels directly to the left/right. You may also choose to use the
*Sobel operator*, which uses diagonal elements too, but gives them
half the weight toward the derivative.

By the way, we might say that we "convolve" a kernel like the Sobel
operator with the input. But I won't pursue that line of mathematics.

From this, you can calculate the magnitude of the gradient, plus its
angle. This is given by `atan2(partial wrt y, partial wrt x)`.

Edges can still be "thick". That is, if the edge is sort of a rounded
transition, and not very sharp, several pixels orthogonal to the edge
direction could all have fairly high gradient magnitude. Now, the
gradient is perpindicular to the edge direction. If you want only one
pixel to lie on the edge, you can look at adjacent pixels that lie
along the gradient and only keep the one with the max gradient.

Note, gradient direction is typically rounded to the nearest
45deg. However, it is also possible to use interpolation and allow a
wider range of angles.

Next, we use two thresholds to eliminate noise. One is a low
threshold: if the gradient magnitude is below this threshold, we throw
it away. If it is above the low threshold, but below the weak
threshold, we consider this a "weak edge." If it is above the high
threshold, we consider this a "strong edge."

Next, we try to determine whether the weak edge pixels should be
preserved or thrown away. To make this decision, we will try to keep
those which make sense in the context of the strong edges. One way to
approach this is to keep those weak gradient pixels that are adjacent
to a strong gradient pixel.

This technique is called *hysteresis*, I guess.

BTW, OpenCV has a good writeup here:

http://docs.opencv.org/trunk/da/d22/tutorial_py_canny.html

Problems with Canny Detection are: Gaussian smoothes out edges, window
for discrete differentiation is too small, thresholds should be set
more locally, because lighting can be different in different parts of
the image.

The next stage in a pipeline to do edge detection would be this Hough
transform thing. Basically, this detects *lines* in the image, not
just pixels that may be involved in an edge. It is *connecting* the
pixels. This is needed because maybe some edge pixels didn't fire for
one reason or another.

So of course the general idea is for each potential edge pixel you
have a space of lines that pass through this pixel. To better handle
vertical lines, we will use *Hesse normal form*: that means the line
is written as:

    x cos(theta) + y sin(theta) = r

For each edge pixel, consider a quantized set of theta, probably
`kpi/4`. In that case, you calculate `r` and increment a count for the
bucket `(kpi/4, r)`.

At the end, you can look at buckets meeting a threshold count. You can
then look in the bucket, and try to connect those points in the bucket
which are sufficiently close.

## Basic NNs

* They do a basic explanation of least squares linear regression. They
  discuss what GD is.
* They do a very basic explanation of logistic regression. These are
  both very superficial explanations.
* They do a basic review of NNs. Again, very superficial. Shows how
  they can learn non-linear decision boundaries.
* They talk about basic GD using squared loss. They talk about
  backpropagation.
* They have you write a basic implementation of tensorflow using a
  graph structure. You do linear neurons, sigmoid, forward
  propagation, MSE, backprop.

## Deep Learning

* They discuss basic TF: constants, paceholders, variables, basic
  operations.
* They introduce the softmax function.
* They talk about CE.
* They mention about wanting inputs to have mean zero and
  variance 1. First, then you'll typically be working in a range at
  the center of the precision of the floating point number
  type. Second, your hope is that the gradient descent can work
  better: that the step size will be appropriate in both dimensions.
* They suggest weights sampled from a Gaussian. They mention that the
  higher the variance, the more "peaky" your softmax output
  distribution will be, because scaling logits results in more peaky
  results.
* Talks about validation/train/test. He gives a rule of thumb: a
  change in thirty labels is typically significant. So you might hold
  back 30k examples to get accuracy down to 0.1%. Of course, you can
  always try cross-validation techniques.
* They mention SGD. They mention that momentum and learning rate decay
  are useful.
* They tell you how to set up an AWS GPU instance.
* He mentions that linear models are the most "stable" in that their
  partial derivatives are constant.
* Shows the ReLU activation.
* They mention that it's typically more efficient to have a deeper
  model than having more hidden units at each layer.
* They describe regularization. They mention early stopping, and L2
  norm. They also mention dropout. He describes it as a consensus
  technique, similar to how Goodfellow describes it as bagging.

## CNNs

* Explains weight sharing and mentions CNN idea.
* Talks about feature maps and different padding modes.
* They seem to indicate that pooling has fallen out of favor. They
  seem to indicate that a major benefit to pooling was that it
  prevents overfitting, but that dropout is a better regularizer.
    * I thought the idea was to just detect a feature once as present,
      but I guess a fully connected layer can maybe learn that anyway,
      while pooling loses so much information.
    * One question: does that mean that average pooling is more
      popular now? And I don't know that I've seen a backlash against
      max pooling...
* He mentions an idea of 1x1 convolutions. Here, you're just mixing
  the layers at each pixel, so it adds some deepness but is very
  simple. Sometimes people alternative 1x1 convs with larger
  convolutions.
* He suggest the *inception architecture*, where several types of
  convolution are done: a 1x1, a 3x3, a 5x5, and even an AVG pooling
  followed by a 1x1. And then the results are concatenated at each
  pixel. That sort of gets the best of both worlds, without using too
  many parameters.
* They have you build a simple version of LeNet-5 in TF.

## Sign Detection

* I wroteup a review. But mostly I saw that BN was very useful, plus
  learning rate adjustment.
* Perturbation didn't help much, but grayscaling seemed to help a lot,
  oddly. I don't have a theory about why. I learned about adaptive
  histogram equalization, which seems to produce good contrast
  images. However, I didn't use it because it requires OpenCV which I
  haven't yet finished building for Python 3.6.
* I used 5x5 convolutions, but I think more 3x3s is generally
  preferred as more efficient.
* It was important to dropout *after* max pooling, presumably because
  max pooling resists dropout by doing pooling across nodes.
* I got much better performance by decaying the learning rate: 5%
  misclassification to 3%. I would do that again in the future. Batch
  normalization also appeared to help a lot.
* Fancy techniques like 1x1 convolutions or LeCun's multi-scale
  features didn't seem to help that much. In the future, I would start
  simpler and only use like two convolution layers of 3x3, since LeCun
  got good performance from that.

## Keras

* They introduce Keras, which is just a high-level version of TF. The
  Sequential class from Keras allows you to make simple networks that
  consist of a series of layers; you add the layers one-by-one to the
  network.

Here is a Keras example:

```
model = Sequential()
model.add(Conv2D(
    filters = 32,
    kernel_size = (3, 3),
    padding = "same",
    input_shape = (32, 32, 3)
))
model.add(MaxPooling2D())
model.add(Dropout(0.50))
model.add(Flatten())
model.add(Dense(128))
model.add(Activation("relu"))
model.add(Dense(5))
model.add(Activation("softmax"))
history = model.fit(
    X_normalized, y_one_hot, nb_epoch = 3, validation_split=0.2
)
```

* I would say that it would be well worth reading the Keras
  documentation. It seems like I could just use Keras mostly from now
  on.
* They see an LSTM as just a regular kind of layer. Here's an example:

```
model = Sequential()
model.add(LSTM(32, return_sequences=True,
               input_shape=(timesteps, data_dim)))  # returns a sequence of vectors of dimension 32
model.add(LSTM(32, return_sequences=True))  # returns a sequence of vectors of dimension 32
model.add(LSTM(32))  # return a single vector of dimension 32
model.add(Dense(10, activation='softmax'))
```

I guess you can think of an LSTM as operating not on a sequence but on
a vector input, with a particular kind of form of
transformation. There is also a `stateful` flag that allows you to use
the previous batch's LSTM hidden state vector as the initial state
vector in the next batch. This is useful if you want long sequences as
input, but you split it up into stretches of time. That makes things
more tractable.

There is functionality to save the structure of a model to a JSON or
YAML file. There is functionality to save and load weights. The
simplest way to save an architecture *and* weights is `Model#save`.

Methods of the `Sequential` model include `compile` (you specify
optimizer, loss function, metrics to track), `fit` (you give the
training set), `evaluate` (to run your validation or test set),
`predict` (obvious). Training, testing, and prediction can all be done
`_on_batch`. There are also `generator` versions, which presume that
you want to train the current batch on the GPU in parallel with
running the generator to produce the next batch (often with data
augmentation) on the CPU. These are helpful if your dataset won't fit
in memory.

There are of course many many layer types. `LocallyConnected2D` sounds
like something I've wanted; a sparse connectivity with unshared
weights.

There are many preprocessing functions. There are a number for
skipgrams and negative sampling. There's one for generating perturbed
images.

It's straightforward to add constraints to weight matrices and
biases. The typical choice would be max norm. Likewise you can easily
add simple regularizers like L2. Gradient clipping on the other hand
is an attribute of the optimizer classes.

There are a bunch of callbacks you can provide to the `fit`
method. You can use EarlyStopping to top early if you are not making
progress. You can use a LearningRateScheduler, or ReduceLROnPlateau
(which reduces learning rate when loss doesn't change for a specified
number of epochs).

You can acheive a bidirectional LSTM using a layer
*wrapper*. Bidirectional wants to take in your LSTM layer as an
argument.

Keras distributes a bunch of datasets for your easy use. They also
provide a bunch of pretrained models (mostly imagenet models) for your
transfer learning use; it is easy to strip off classification layers,
and also to selectively retrain a few top extraction layers, too, if
you like. To freeze a layer, you set `#trainable` to false.

## Transfer Learning

* Mentions that one advantage is that if a well-known network has been
  trained on a large dataset, you can get a big advantage if you're
  working with a smaller dataset.
* Common approachn when you have a small dataset of similar data is
  you freeze the convolutional layers, strip off the FC classification
  layers, and replace with a randomly initialized FC classification
  layers. Then you just train these with your small dataset. This
  avoids overfitting.
    * If your small dataset is quite different, you may even want to
      strip off some of the top convolution layers, because those may
      not be features of what you're looking for.
    * But early convolutions which look for shapes could still be very
      useful. So you might just keep the first couple convolutions,
      and again not training those.
* If you have a lot of data, you can use the pre-trained weights as an
  initial choice, but then retrain the whole thing.
* They claim increased amount of labeled data plus more computational
  power of GPUs is why NNs are a thing now.
* They talk about some history:
    * LeNet was from 1998 and was used for digit recognition.
    * AlexNet was similar (but deeper) and used for ImageNet.
        * Used GPUs, which was new. Also used ReLU, which was
          new. Also dropout, which was new. Error on ImageNet dropped
          from 26% to 15%.
* AlexNet architecture:
    * Input is 224x224x3. They do 11x11 convolution to 96
      layers. Stride is 4. That downsamples to 55x55. They use ReLU.
    * Then they do LRN followed by 3x3 pooling with a stride of
      two. That further downsampels to 27x27. They find overlapping
      pooling added about 0.5% accuracy.
    * Next they do two sets of 128 5x5 convolution on 48 layers
      each. I believe they do this on two parallel GPUs. The stride is
      now one.
    * They again do LRN and 3x3 pooling with a stride of two. This
      brings us to a size of 13x13. No more pooling will be done.
    * Now they do two sets of 192 3x3 kernels, each acting on *all*
      256 layers of the prior layer. This is the only time there is
      cross-communication in the convolution pipeline. At this point
      they stop doing LRN or pooling.
    * They repeat an identical layer of two pairs of 192 3x3 kernels,
      but this time they work on only half the previous 384 layers.
    * They have a pair of 128 3x3 kernels, as ever acting on halves of
      the previous layer's feature maps.
    * They now do a fully connected layer of 4096 units, followed by a
      second layer of 4096 units. This then goes to 1000 unit softmax
      layer, which is the output.
    * They note that CNNs are a good way to inject a bunch of
      knowledge: that features are invariant to translation, which is
      vital to learn a very hard task with limited data.
    * They note that training took 5 days on two GTX 580 GPUs. They
      used two GPUs primarily because their GPUs didn't have enough
      memory to fit the entire model otherwise.
    * They increase the dataset size by a factor of *2048* by doing
      224x224 crops of the 256x256 input images. At test time, they
      take a 256x256 image and take 5 224x224 patches: the corners and
      the center. They then average the results. Without this
      augmentation, they found that overfittingn was a huge problem
      and they would have been forced to use a much smaller network.
    * (BTW, the paper has a mathematical error; the input should be
      227x227x3; Karpathy notes this).
    * They further augment by doing PCA for the pixel intesnities
      across ImageNet and then adding small multiples of the principal
      components. This effectively changes the intensity of the
      lighting, they claim. This increases accuracy by one %age point.
    * They do weight decay, which they found was important to get good
      *training* accuracy; so they didn't use it as merely a
      regularizer. Weird that this would occur. Otherwise, they did
      standard SGD on batches of 128 with momentum. They initialized
      with Gaussian weights of 0.01 stddev. They used a bias initialized
      at 1.0 for some of the convolutional layers.
    * They decayed the learning rate by 10x when the validation error
      stopped improving.
    * One way they test visual knowledge is to find what pairs of
      images are closes at the last layer of 4096 hidden units,
      measured by L2 distance.
    * They note that accuracy decreases by 2 %age points if any layer
      is removed. So they claim depth is very important.
* They note that LRN is not typically used anymore, as it doesn't seem
  to add much if any value.
* They then start talking about VGG. This is a simpler architecture.
* GoogLeNet performs just a bit better than VGG. They used the
  "inception module" idea, which is very efficient.
    * Inception module was pooling followed by a 1x1.
    * AND, a 1x1, followed by: (a) nothing, (b) a 3x3 convolution, (c)
      a 5x5 convolution.
    * It ends up having a very small number of parameters, while still
      being very accurate. So it's a good choice for real-time
      applications they say.
* Resnet is also from 2015, and is from Microsoft.
    * It has 152 layers, vs 22 from GoogLeNet or 19 from VGG.
    * Resnet has 3% error which is better than human accuracy.
    * It can train fast because of the skip connections.
* This module was idiotic. They gave you some bottleneck features and
  had you train a simple logistic classifier.

## Conda

```
source activate myenv
python -m ipykernel install --user --name myenv --display-name "Python (myenv)"
source activate other-env
python -m ipykernel install --user --name other-env --display-name "Python (other-env)"
```

The above demonstrates how to setup other kernels for use in Jupyter.
