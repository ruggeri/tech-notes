These are my notes on the Goodfellow/Bengio/Courville textbook.

## Ch6: Deep Feedforward Networks

They introduce as a series of function approaches: one per layer. They
suggest the power is in generalizing beyond linear networks. A common
way to generalize from linear networks is to learn a
reparameterization of the input. You can use a radial basis function,
which is very general, but it will have poor ability to generalize.

You can build the feature mapping by hand, but this is very hard and
doesn't result in transferable techniques.

So the idea is to learn new feature mappings.

They go through some very basic stuff about nonlinearity, gradient
based optimization, initialization is important, cost functions. They
mention that most networks are trained on maximumm likelihood (either
assuming gaussian noise or finding the cross-entropy).

They mention that log probability is in practice also convenient
because it "undoes" some exponentiation, which is a cause of
saturation in units. For instance, extremely negative values have very
small gradients for the exponential function. They also note that CE
in practice never acheives minimum because it will never assign a zero
probability.

They state without proof (requires calculus of variations, which is
optimization of *functionals*: when the input is a function), that
minimizing the expected squared error means you need to predict for
each `x` the expectation `E[Y|x]`. Of course, you won't be able to
every compute the true expected squared error because you don't have
an infinity of datapoints. So you estimate this with the empirical
mean. Also: this function is in any case never within the set of
functions your network has the capacity to represent. Of course:
everything turns out okay.

**Output Unit Activation Functions**

Let's focus on the activation function used at the output layer. What
activation funcion would make sense? This is after all transformations
of the inputs is complete.

You could have no activation fn, in which case the last layer is an
affine transformation. It is typical to treat this then as a mean for
a Gaussian, and thus minimizing the squared error is how to maximize
the likelihood.

They make a note later: you can treat the outputs of the network as
*parameters* of the distribution of `p(y | x)`. That's how I like to
think!

For prediction of a binary variable, sigmoid makes sense. They show
this is a two-valued softmax. They don't mention why the logits might
be assumed to be linear in the inputs: this is natural if you assume a
Naive Bayes model or logistic regression. I mean, that's why it's the
logistic function...

Now, we want for the gradient not to shrink on us so that we can learn
effectively. With the sigmoid plus the cross-entropy, the gradient
with respect to the parameters does get very small in one scenario: as
we get more and more confident of the correct answer! That is good,
because it means the further away we are from the correct answer, the
bigger a step we will want to take in the correct direction.

This isn't true if we were to use the squared error loss function. In
that case, the sigmoid can saturate when it is very confident of the
*wrong* answer, and the squared loss will not "undo" this.

Another practical note: the use of the cross-entropy loss means that
we will care less and less about getting an example more-and-more
right, and shift our focus to those examples where we are not doing a
good job of predicting the label.

They note that softmax works like "lateral inhibition" which is a
winner-take-all phenomenon observed in neuroscience. Basically,
increasing the logits for one class necessarily reduces the
probabilities of the other classes. I don't know if I see this as a
particularly deep insight...

They suggest some fascinating possibilities. Your NN could try to
compute not only the mean of `p(y | x)` but also the variance. This
lets you build heteroscedastic models. Likewise, you could also
consider *multimodal distributions*! You could accomplish this with an
output of several means, which is basically trying to learn a
featurization to do a Gaussian mixture model.

Fascinating: with such a model you should be able to more accurately
pick a high-probability value of `y`, rather than just a mean value of
`y`.

I think the point is this: you're just trying to do featurization! You
can do anything at the output layer, at least in principle!

**Hidden Unit Activation Fns**

They acknowledge that this is unique to NNs. And that there isn't much
justification; mostly just use what works well. Most choices have at
least a left or right derivative everywhere. Some have derivative zero
at some points but that seemsd to be okay.

The ReLU is a default choice. Except at zero, it has a large *and
consistent* derivative, meaning it is a solid indicator for gradient
descent. They mention that you might try initializing the bias for
ReLU at 0.1 just to make sure they are active for most examples in the
beginning.

Sometimes people use absolute value, which can make sense for images
because features should be invariant to changes in *polarity*. A leaky
ReLU sets the slope to be very small (like 0.01) on the other
side. PReLU (parameteric ReLU) treats this as a learnable
parameter.

Maxout units partition the `z` coordinates and output a single maximum
value for each group. This means the activations have lower
dimensionality than the `z`. The idea is that this can learn
activation functions that are piecewise linear.

Sigmoidal functions (sigmoid and tanh) are discouraged now because
they saturate easily and slow learning.

There are tons of activation functions and they aren't that
interesting; in practice they all work pretty okay. Softplus is
notable for being actually worse than ReLU, which is counterintuitive.

They talk about universal approximation. You only need one hidden
layer, but it may have to be very very wide. So therefore it is
typically better to use more hidden layers and thus have fewer
parameters overall to train. This makes sense to me because a single
hidden layer can partition up the input space and map each locality to
a desired output value.

However, even if we choose an appropriate architecture, we may not
learn the right function because either (1) the optimization procedure
can't find it (gets stuck) or (2) it overfits! We also don't know how
big our network needs to be to represent what we want.

Depth does help. This was first shown for circuits: as you increase
depth, the number of representable functions grows
exponentially. Similar results exist for NNs with rectifiers.

Mostly a series of layers, but sometimes you have "skip connections"
between layers. Or you might have sparse connectivity, as with
convolutional nets.

They note some big advances. First, backprop. Second, larger dadtasets
for better generalization. More powerful computers. But some
algorithmic changes: MSE used to be widely used, but is now
obsolete. And also using ReLU.

They also note: ReLU was inspired by neuroscience, so there are some
positive contributions from that field.
