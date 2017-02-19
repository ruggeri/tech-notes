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
* They talk about "Hough space". This is a space of `(slope,
  intercept)`. The lines passing through a given point can be
  represented as a line in Hough Space. Any two points must be
  representedy by two non-parallel lines.
    * It is quite odd that the slope for any two points can never be
      the same...
    * But of course it is necessary because you can always draw a line
      through any two points.
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
