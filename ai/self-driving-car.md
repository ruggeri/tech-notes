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

## Conda

```
source activate myenv
python -m ipykernel install --user --name myenv --display-name "Python (myenv)"
source activate other-env
python -m ipykernel install --user --name other-env --display-name "Python (other-env)"
```

The above demonstrates how to setup other kernels for use in Jupyter.
