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

## Ch7: Regularization

Typical not to do decay on biases, just weights.

They explore L2 and L1 regularization. They note that L1 will tend to
make the weights sparse. They note that L2 does MAP with a Gaussian
prior on the weights.

They talk about weight constraint techniques. You can theoretically
enforce constraints by finding the appropriate penalty, but it is much
more common to project `\theta` to the nearest point that satisfies
the constraint. Hinton apparently suggests a high learning rate with a
weight constraint to explore a space fast. He suggested constraining
the norm of columns of the weight matrix, so that the input to any one
unit does not grow too large.

They note that sometimes a problem is "underdetermined" and has many
possible answers. So regularization forces there to be a "best"
solution. E.g., with the kernel trick an SVM would have many
possibilities, but we effectively regularize by choosing the maximum
margin hyperplane.

They mention dataset augmentation by rotation and translation. Can do
left-to-right flips, but be careful of "b" and "d"! Likewise,
top-to-bottom is almost always wrong.

They mention that adding noise can be beneficial, as NNs can be
sensitive to noise in the inputs. On the other hand, in a linear model
this should be the same as L2 regularization (as noted in the Hinton
class). But you can't argue with experience, I guess: the NNs are
*not* linear, so the action of adding Gaussian noise is *not* truly
the same as L2 regularization.

They note that doing a perturbation of the weights when running feed
forward will result in you adding a regularization term proportional
to the square of the gradient's magnitude. This means you prefer
settings where small changes in the weights would not have a large
impact on the output of the model.

You can also do *label smoothing*. Here, you can admit that the
dataset may have mislabeled examples. So instead of targeting exactly
the class, you use the full power of cross entropy and say that the
example has probability `\eps` of one class and `1-\eps` of the other
class. Apparently this is quite a common trick.

I don't see how this is really "regularization," but they mention
semi-supervised tasks, where the tradeoff between the clustering task
and the supervised task results in better generalization. And they
mention multi-task learning, where a base representation is
constructed that will be fed to many tasks, so they can reinforce each
other.

Early stopping is clearly the easiest. Evaluation of the validation
set can be expensive, but it can be done in parallel with
training. They mention that some people then try to add in the
validation data to train a bit more, monitoring accuracy on the
validation data. But it can keep going down due to overfitting! One
approach is to stop when it falls below the training set loss observed
at the time the validation loss stopped decreasing.

Why does early stopping work? Interesting! It actually is equivalent
to L2 regularization; basically, if you consider the trajectory of the
updates, you're only letting them get so close to the optimum.  But
here's why early stopping is so much better: it tells you exactly how
much regularization to use by observation of the validation set.

They mention parameter sharing: CNNs are by far the biggest example.

*Bagging* means *bootstrap aggregating*. It is an *ensemble method*
that works by doing *model averaging*. The idea is to sample with
replacement and train `k` models. It apparently works well for methods
that are unstable, and NNs are a good example of that. Indeed, simply
by training the same model with slightly different initialization and
minibatches selected is often very effective.

Boosting does the opposite: the ensemble members are trained to
correct the errors made by the others.

Dropout acts like bagging. Bagging is expensive, because you have to
train a separate NN for each bag. But each dropout mask for a network
is effectively specifying a different network; it shares many
parameters with other networks, but it also has a lot of
differences. Each "network" defined by a dropout mask is trained with
a different sample of examples. Now, we won't train all the way to
convergence, but training this huge number of models each just a
little is very effective.

To compute the prediction of the ensemble, technically we should
average over all possible masks. But there are an exponential number
of masks. Sometimes people do run inference 10 or 20 times with
different masks to get a very reliable estimate.

Hinton was the one to suggest *weight scaling inference*. That says:
if the dropout on any given unit is 50%, then use all the units in the
network and divide all activations by 2. There isn't actually a
theoretical justification for why this approximates the ensemble, but
empirically it performs very well. In a network where the units are
all linear except the outpout layers, this does truly correspond to
the ensemble average.

It has been found that in some cases that even an ensemble average of
1,000 subnetworks is outperformed by the weight scaling approach. But
then some other research shows the Monte Carlo approximation approach
outperforms for some other tasks. It's not entirely clear, but weight
scaling is very simple and preferred.

Srivastava (a Hinton student) showed dropout is more effective than
weight decay or norm constraints. Wang and Manning showed a technique
called *fast dropout*. Basically, it is less random than dropout, so
it can converge faster. The point is that the randomness isn't really
the point of dropout: it's the model averaging. They claim an order of
magnitude speedup.

This view as bagging doesn't seem to be the primary interpretation by
Hinton or Manning. They talk more about preventing co-adaptation of
features; the idea that an uncommon but highly indicative feature will
suppress the use of a common but less powerfully indicative
feature. They note that it is important to destroy *features* rather
than just add noise to the inputs; adding noise to the input doesn't
typically destroy the availability of features until it totally
destroys the input.

One thought that justifies this thinking about coadaptation: some
experiments show that dropout is superior to canonical bagging.

They mention another form of dataset augmentation, where examples are
adversarialy modified. That is, you take images in the training set,
and see what very similar images are classified very differently. Then
you add those as examples. The theory is that networks are
"excessively linear", and that near labeled examples you can still
change the output dramatically through small changes in a number of
dimensions, all of which compound. By using adversarial examples you
are encouraging the network to have "local constancy".

This technique can work well when you have semi-supervised
learning. Consider an unlabled point `x`; your superivsed model
guesses `y`. This might not be correct, but you can probably benefit
by forcing your model to have a consistent guess for `x'`
adversarially generated near `x`. This is called a *virtual
adversarial example*. Again, this is training robustness to small
changes.

They mention an idea called *tangent prop*. This basically says: you
may identify some axes along which an example can be modified without
effect on the label. For instance, translation of an image represents
a dimension along which you can modify an image. So what you do is add
a regularization penalty where you take the inner product of the
gradient and this invariance axis. That basically is saying: penalize
the model if changing this thing that makes no difference in real life
does make a difference in the model. This is very similar to dataset
augmentation, and requires prior knowledge.

They note some problems with tangent propagation vs dataset
augmentation. Tangent propagation trains the model to resist
"infintesimal changes", whereas dataset augmentation "confers
resistance to larger perturbations." They also note that tangent prop
doesn't work well with ReLU, but I don't understand this reasoning.

It is also related to *double backprop*, which applies regularization
to the gradient, preferring small gradients at the examples. This just
says do tangent prop along all axes. If you think about it, this is
like adversarial training, too! Again, adversarial training is the
non-infintesimal version of double backprop.

## Ch8: Optimization for Training Deep Models

There is some jargon. We typically have a performance measure we care
about, but because that is hard to define, we often settle for
minimizing a loss function. We want to minimize the *risk*: the
expectation of the loss. But since we don't know the distribution of
the data, we minimize the *empirical risk*. And even then, we
typically substitute a *surrogate loss function* like negative
log-likelihood of the correct class because a measure like accuracy
doesn't have useful derivatives.

They note that a surrogate loss function can learn more. For instance,
there's a difference between 51% belief in the correct answer and 99%
belief. A model with more confidence will be more resistant to
perturbation. So pure accuracy is not actually an ideal measure,
really.

Another thought: with early stopping, we can stop when the true loss
function stops improving on the holdout set. In that case, we may stop
earlier than when the surrogate loss function has been minimized.

They talk about stochastic and mini-batch learning. They note this
works because the standard error in the mean of the gradient improves
sublinearly in the number of examples: it is `sigma / \sqrt(n)`. So
doing 100x more examples only gives you a 10x reduction in
variance. So often you converge faster by taking more gradients
faster.

Why not a minibatch of size one? Well, you do get more stable
estimates. And multicore architectures often are underutilized with a
single example to work on at a time. They do note an interesting
phenomenon: small batches tend to have a regularizing effect.

Minibatches of size 100 can approximate a gradient pretty well,
empirically. But for 2nd order information you may need many more
examples: like maybe 10,000.

They mention that you at least want to shuffle before doing
batches. This seems to be good enough, even though your batches are
not truly random. They note that you can do parallelized versions of
SGD where many updates are made independently, and that this seems
actually to work fairly well.

They mention the problem of *local minima*. Of course, there are many
global minima: you can always permute the weights. But the worry is if
some local minima are much greater than global minima. They claim that
there isn't a lot of evidence this is a common and problematic
phenomenon. They say that experts feel that local minima are normally
very good points in the network space. They encourage practitioners
(who often complain about local minima) to explore whether the
gradient has truly vanished, or whether there is something else wrong.

They say that our problem is saddle points. They note that as the
number of dimensions increases, the ratio of saddle points to local
minima should grow exponentially. That's because a minimum has a
hessian with all positive eigenvalues, while a saddle point is free to
se have a mix. They note that for many families of random functions,
critical points of low cost typically have more positive
eigenvalues. That means that true minima are probably low cost, but
critical points of high cost are probably saddle points. Fascinating.

There are theoretical results about families where saddle points are
the real problem. And empirical studies back that up. So the message
is: fear the saddle points, not the local minima!

Now, for first-order methods, saddle points seem like they are often
escaped pretty well. Newton's method seems to be more susceptible to
jumping toward saddle points. But in any case Newton's method is not
tractable generally, so this is no great loss at this time.

They note the problem of *cliffs*. Here, the gradient is huge, and you
may take a huge step and jump off the cliff face to way too far
away. The solution is typically *gradient clipping*. The intuition is
that the gradient is telling you a *direction* not a step size,
really. Clipping is a problem in recurrent nets especially, because
the effect of the same weight is compounded over several steps. They
say they will study this more (including the *vanishing gradient*
problem) in the chapter on RNNs.

These concerns have all been about getting stuck or moving in the
wrong direction. But more typically, the problem isn't about where we
go, but how long it takes to get there.

They also mention theoretical bounds to what can be efficiently
learned. But they note that in practice these don't give any useful
indications of what to do, and that often a reasonably acceptable
solution is available.

They note that for SGD, you should be decreasing the learning rate
over time. That's because there is stochastic noise in the batches
that does not decrease over time; you won't converge unless you
attenuate learning rate. They note that a sufficient learning rate
condition is: that if you took an infinite number of steps, the steps
would add up to infinity (so that you can travel any distance
required), and the sum of squares of steps is less than infinity (that
is, the step size reduces to zero over time).

It is common to decay to 1% of the initial step size.

They talk about momentum, and how it helps when you have small but
consistent gradients, or noisy gradients. It can also help when you
have high curvature they say (I suppose because high curvature
represents acceleration?). Momentum is typically increased over
time. They mention Nesterov momentum.

They note some convergence results that show how fast you can expect
convergence in the case of convex functions, but these of course don't
really apply...

Now they talk about initialization. They admit this is very poorly
understood, so guidelines are heuristic. One problem is that with poor
initialization you may still do well on the training set but then
suffer poor generalization. Typically we try to initialize to have
some desirable property in the beginning, but then this might be lost
as we train...

Maybe the only truly known guideline is to break symmetry. If units
have the same activation and same input weights, they'll do the same
thing. That effectively reduces the dimensionality of the space that
can be represented by the network. Random initialization effectively
makes the units compute different functions. But in theory you could
choose the weights to be orthogonal to each other initially; then you
would be guaranteed every unit would compute something very
different. In practice, random weights work well.

In practice it doesn't appear like Gaussian vs uniform distribution
makes much difference. But scale definitely matters. They mention that
if you use small weights, you will lose signal; I don't know if I
totally understand why this is problematic to the next layer if
everything is reduced by a constant factor... But large weights can
cause saturation.

They give one possible reason. Early stopping is like a prior that
says the initial parameters were correct. So if the parameters are
large, that means it says that units have strong interconnected
effects.

I think I continue to be unsure on this point...

You might select a uniform choice in the range `+/- 1/sqrt(m)`, where
`m` is the number of inputs. That should give you unit variance. The
Glorot and Bengio initialization is `+/- sqrt(6/(m+n))`, which is a
compromise which also tries to ensure the gradient has unit
variance. Note these are only exactly true in linear networks, but
they seem to work well. But you can also treat the *gain* of the
weights at each layer as a hyperparameter and search for the best
choices.

Okay, what about initialization of weights? For ReLU, it is typical to
use small positive bias to avoid saturation at values below zero. For
output units, you might choose a bias such that, when you apply the
final activation, you obtain the marginal statistics. And last, for
multiplicative factors, like a gate multiplier on an LSTM, you might
try a bias of 1.0.

They mention the possibility of pre-training. Here, you train a
network for one task, and then use those weights when learning another
task. It is common to do this with unsupervised pre-training, I think.

They talk about adaptive learning rate algorithms like AdaGrad,
RMSProp, Adam. They mention that they all seem pretty good; there is
no conesnsus on the best one. It typically depends on what someone has
the most experience with.

They talk about some 2nd order methods. Newton's method I am familiar
with; but it's too expensive! *Conjugate gradients* are
interesting. Here's the idea. Let's say you start out in one
direction, and do a line search to find the minimum. Then there is
zero gradient in this direction at a minimum, so you will
*necessarily* move perpindicular to this gradient in the next
step. This creates zig-zagging.

To stop the zig-zagging, we can use Hessian information. We want
`prev_direction^T * H * current_direction = 0`. That's saying: at the
current point, moving in such a direction won't make us want to move
any more in `prev_direction`.

If we have a perfect quadratic function in `n` dimensions, we verify
solve in exactly `n` steps. That won't quite happen if the function is
not quadratic, but if it better approximated locally by a quadratic
(and it should be) then this can be better.

Now to find a conjugate direction! You *could* invert the Hessian,
that is the most obvious way. But apparently there are some other
ways: Fletcher-Reeves and Polak-Ribiere. But I'm not too interested in
the gritty math right now.

They talk about BFGS. This basically builds up an approximation, over
many steps, of the Hessian. It does this by making low-rank updates to
the approximation at each step. However, even though this is fast, it
takes `O(n**2)` memory. That is itself very intractable. Therefore,
L-BFGS keeps a low-rank approximation of the approximation of the
Hessian matrix.

**NB**: My impression is that focusing too much on this nitty-gritty
math would be a waste of time. I think the math behind these systems
is very circumspect anyway, so maybe exploring architectures would be
more profitable. OTOH, techniques like dropout and batch normalization
and ReLU are many of the advances that made these new architectures
possible...

Next they talk about **batch normalization**. They are very excited
about this. Basically: when you optimize all layers in parallel, you
break an assumption of each layer that its will be the sole
update. It's the same problem with adjusting all parameters of a layer
at once, but just moreso.

So here's what you do. You basically replace the activations of a
layer with the unit variance, zero mean version of those activations
on the minibatch. You also backpropagate through this operation. What
this does is make layer `k` not try to change itself just to change
its mean or variance. That can be handled at some subsequent layer by
a general increase in the weights.

Now, they note that for each layer you can learn an `\alpha` to scale
the activations and a `\beta` as a bias; basically the layer can take
on any variance or mean. That gives the network back representational
power that was lost by doing the batch normalization. But then what is
the point? Doesn't that take us back to where we started?

Answer: no. It used to be that the mean at layer `k` was determined by
a complicated interaction of *all layers prior to `k`*. Now, it
depends just on `\beta_k`, a single parameter. That means this is much
easier to train.

They talk about some pretraining approaches. Greedy *supervised*
pre-training has you train up to level `k`, and then use the outputs
of this last layer as a fixed input to train layer `k+1`. There are a
number of ways: the original is to just train a one-layer network,
then use the hidden activations as the input and train a second
one-layer network. If you like, you can feed both the hidden
representation *and* the original input.

The theory on why this works is that the middle layers get better
guidance of how they should organize themselves.

Another approach is transfer learning: train for some tasks, and then
use the output of the `k`th layer as input for another task.

They note: SGD with momentum continues to be a very popular approach,
and has been since the 80s. The big advances are in coming up with
architectures that do well with our optimization techniques. For
instance, LSTM plays nice with SGD on deep networks. Use of ReLU and
skip connections are also examples.

They mention *continuous learning*. Here, you use a series of
less-and-less blured versions of the objective function. The idea is
that maybe the original blurred one is convex, and each time maybe
you're in a part of the objective function where it is convex. This
sounds like it has been somewhat successful.

*Curriculum learning* is another idea. Here, you start out with easy
examples being given more weight by the objective function. Then, as
the network masters the easy examples, you increase the importance of
the hard ones. This is actually similar to how humans teach other
humans. It was found though that it is valuable to have a mix of easy
and hard examples; maybe because with the easy examples you overtrain
in a way not compatible with learning the hard examples?

## Ch9: CNNs

They note that what NN people call convolution is often just called
*cross-correlation*. Whatever, but good to know, since I had that
problem of flipping. They note that convolution is equivalent to
multiplication by a matrix with some entries constrained.

The ideas behind CNNs are: (1) sparse connectivity which reduces the
number of parameters to train and speeds up computations, (2)
parameter sharing which further reduces number of parameters to train
and memory, and (3) equivariance, which means that translations of
input results in translation of feature maps.

Max pooling can be used to make a layer invariant to small
translations; it is helpful if we care more about the presence of a
feature than exactly where it is. They note that you can pool over
locations, but you can also pool over feature maps, which means that
the feature maps can learn what they want to be invariant to; e.g.,
three maps could look for a five in any orrientation. This sounds less
about spatial properties than just the general maxout or saturation
idea.

They note that convolution and pooling are basically like restrictions
on the weight matrix. Thus, you can think of them as an "infinitely
strong prior." They note that this means you may encounter
underfitting, as the model has lost freedom.

They talk about striding and padding. They talk about *tiled
convolution*, where basically your convolution is made of a set of
sparsely connected regions. THis seems very uninteresting.

They briefly mention that you don't have to use CNNs just for
classification. For instance, you could do pixel-by-pixel
labeling. They note an architecture from Collobert which iteratively
applies convolutions to refine label predictions. But this isn't much
discussed, even though it sounds very interesting!

They note some possible applications. Audio is a 1-D application:
amplitude over time. Image is a 2d application: you can move the
kernel in both dimensions. Likewise, audio can be 2d if you do a
Fourier transform and slide the kernel along the frequency axis; then
that makes it respond regardless of the frequency: e.g., detect a sine
wave at any frequency. A 3d application could be video.

They make a note: because of the nature of convolution, it can process
different image sizes. Likewise, you can use pooling to reduce an
image to a desired size: e.g., ask if a feature is present anywhere in
each quadrant, thus reducing to 2x2.

So oftentimes a lot of our network is learning convolution operators,
and then we do some kind of simple classification. We can train the
whole network of the classification plus feature extraction, or we can
train feature extraction seperately. One idea used by Coates is to do
k-means clustering of images, and then use each centroid as a
kernel. Or you can learn them through some other unsupervised
approach (discussed in later chapters).

It is noted that random filters actually do pretty well! One practical
suggestion is to use random filters to pick an architecture of your
CNN, and then train that.

They mention that you can do greedy layer-wise pretraining. Here, you
leave the rest of the network untrained, but just train the first
layer, then leave that be, and train the second. You can do this also
with an unsupervised criteria at each layer; they'll talk about that
in Part III. But they note: now there is a lot more labeled data, and
because there is more compute resources, these techniques are not as
common.

They make some interesting historical notes. Convnets were relatively
successful at a time when other NNs were not considered
successful. It's not clear why they were succeeding. Nowadays, when we
use contemporary techniques from the 90s, they seem to perform
reasonably well. So maybe practitioners didn't have a lot of compute
resources, and thus the savings of convnets was very important. Or
maybe it was psychological.

## Ch10: RNNs

* Pretty basic discussion. They mention teaching forcing, and how this
  is a kind of maximum likelihood approach.
* They note that you have to assume a stationary conditional
  distribution for an RNN. But of course the advantage is you use far
  fewer parameters.
* They mention how do you decide when to stop? One way is a stop
  symbol.
* They discuss bidirectional RNNs. Seems like Graves is really into
  this. They mention it makes sense for tasks like speech recognition
  where information about subsequent sounds shapes our interpretation
  of the previous sounds.
* They describe encoder/decoder architecture briefly. The mention
  attention but that is discussed later in the book.
* They describe the idea of a recursive neural network. Here, pairs of
  adjacent elements are combined. Then this is repeated. Again and
  again until you get just one element. You use the same matrix to
  combine at every level.
    * The advantage they mention is that the path from the final
      result to the items of the sequence is now logarithmic, which
      helps with backprop.
* They discuss the long term dependency problem. The idea is this: the
  gradient for long-term dependencies is very small, whereas the
  gradient for short-term spurious dependencies is much greater. So
  it's really hard to learn long-term dependencies.
* They propose another approach, called *echo state networks*. The
  idea is this. The hard thing to learn is how to make sure the hidden
  weights capture the information over the sequence of values. So they
  want you to *manually* set these, and just learn the output weights.
    * If your output is a regression value with squared loss, your
      task is now a simple convex task.
    * The idea is to set the weights of the transition matrix such
      that all eigenvalues are close to one. If that is true, then you
      don't get blow up or shrinkage.
    * ESN weight setting techniques have been used as *initialization*
      values for normally trained networks with some success.
* They suggest other ways to avoid vanishing/exploding gradients:
    * Add skip connections.
    * Or add linear self-connections wiht a weight near one. That
      calculates a decaying average. These are called leaky units
      sometimes.
* They talk about LSTMs. This adds the ability to forget, whereas
  leaky units just remember. They also mention GRU which is a minor
  simplification. They note that many LSTM variants appear to work the
  same, but initially biasing the forget gate to 1.0 seems to help:
  that makes the LSTM be initially biased toward remembering fully.
* They discuss gradient clipping, which basically is worried that
  you'll be on the face of a cliff and take way too big a step.
    * You can either do element-wise clipping or clip the *norm*,
      which renormalizes the gradient to have a maximum value.
    * Norm clipping preserves direction. But it appears either way
      works about equally well.
    * It has actually been found that taking a *random* step works
      about as well. Presumably this can jitter you off the rockface.
    * They do note that theoretically doing norm clipping may cause
      SGD to no longer be an estimation of the overall gradient
      optimization. That's because some batches are unaffected, but
      other examples are. But the impact of this is negligable.
* They note an interesting technique to use *regularization* to
  encourage *information flow*.
    * Here's the idea. You want the derivative of the loss wrt the
      hidden unit activations at time `t` to have the same norm as for
      wrt hidden unit activations at time `t-1`.
    * Basically, that's saying that the network is not more or less
      sensitive to changes in activation at time `t-1` than at `t`.
    * So, to do this, you can add a regularization term.
    * This does appear to be pretty effective.
    * This is from work by Razvan Pascanu; I see him a lot.
* They talk about explicit memory.
    * This is the idea of memory networks (Weston) and neural Turing
      machines (Graves).
    * They talk about how they have content-based addressing; each
      cell of the memory contains a whole vector.
    * The problem of vanishing or exploding gradients doesn't exist
      here, because explicit memory is kept across timesteps.
    * They mention that reads/updates are typically *soft*; they
      affect all cells, but some more than others.
    * They mentino there is study of stochastically choosing a cell to
      operate on. This is called *hard attention*, and requires
      different training methods.

## Ch11: Practical Methodology

This is a chapter of general advice.

* First, choose an error metric and a desired level of performance.
* They mention precision and recall. They mention F-score. Typically,
  by varying a threshold for deciding, you can get a precision-recall
  curve. You can measure the area under the curve.
* They recommend fully-connected feedforward when you have fixed size
  vector, LSTM/GRU with sequences, convolutions with sequences.
* They recommend SGD with momentum and using a weight decaying
  strategy. Linear decay, exponential decay, or 2-10x decay when
  hitting a plateau are all popular.
* They recommend some regularization from the beginning unless you
  have tens of millions of examples. Early stopping is a no-brainer,
  but dropout is easy.
    * They mention that batch normalization allows you to skip
      dropout, because of noise in the estimates of the statistics
      used for normalization.
* They say don't get more data until performance on the training set
  is good. In that case, add more capacity, or twiddle
  hyperparameters. But if training set performance is good and test
  set isn't, then collecting data can be the easiest choice.
    * If this isn't possible, add regularzation to try to generalize
      better from a small dataset.
    * They recommend examining curves of performance given amounts of
      data. This can help you decide how much more to collect, since
      you can try to extrapolate performance. Typically, you have to
      double training set at least to see better performance.
* When tuning hyperparameters, learning rate is by far the most
  important.
* Common to do grid search. Logarithmic scales are frequently used:
  learning rates of `0.1, 0.01, 0.001...`.
* They suggest random search over grid search. You sample from some
  distribution that seems reasonable. Experiments have found this is
  better. It appears that this works better because normally only some
  parameters are important, and you will try different values for
  these under every trial. You won't waste time exploring the many
  possible settings of the unneeded variables.
* They mention that you can try to model validation set performance
  from the hyperparameters: like a hypermodel. But they say this often
  works very poorly.
* They next talk about debugging. They mention that you should look at
  the "worst mistakes" of the algorithm, to see what is going on with
  these. This can be measured by probability of the correct class
  assigned by the model.
* They recommend trying to find software bugs by trying to fit a
  classifier to a single exmple. If it can't do this perfectly,
  something is fucked.
* Histograms of activations and backpropagated gradients are
  useful. You can see if you are saturating neurons. Likewise, you can
  see if gradients are exploding/vanishing.
* They suggest comparing magnitudes of parameter gradients to the
  parameter values. Ideally, the update over a minibatch would be ~1%
  to each parameter: that way they are changing not too fast, but not
  too slow.

## Ch12: Applications

* They talk about GPUs, and how they have high-speed access to large
  amounts of RAM, plus high data-paralelism, which is enabled by low
  branching.
* Distributing over more computers is easy at inference time, since
  this is data parallel. For training, is harder...
* Distributed async SGD is typical. This basically reads and writes to
  a *parameter server* without a lock. This is inaccurate, but the
  higher volume of updates is worth it.
* They mention that it is common to train in clusters, but sometimes
  you deploy to user hardware like cellphones. They mention *model
  compression*: here, after trainng a complex model, you then generate
  random datapoints and see what the complex model says. You then use
  these to train a smaller, simpler model. You can do this because you
  now have an "infinite" number of datapoints.
* When detecting a rare event, you can train a *cascade* of
  classifiers. The first will tell you if the even is definitely not
  detected. If the confidence returned is not high enough, a second,
  more sensitive test is used. Et cetera. You can either have higher
  model capacity at each step, or have trained using boosting.
* They talk about mixture of experts: a neural network called a *gater*
  gives weights to a variety of networks, each of which has a
  decision.
* This can be used to accelerate inference (not just improve results)
  if you have *hard mixture*, which means the gater just chooses the
  best model.
    * Incidentally, they mention the idea of a decision tree where
      decisions are made by NNs.
* They mention ASICs and FPGAs were not that attractive when GPU perf
  was ever-increasing. But now this may be more useful, as single-core
  performance is not increasing quickly. They mention using fewer bits
  at inference time, but that 8 to 16bits are needed for backprop.
* Visual tasks:
    * Easy for humans; hard for computers. One of the most popular
      deep learning domains.
    * Reporting objects detected, bounding boxes, transcribing,
      segmentation by classifying pixels.
    * Synthesis isn't a primary goal, but can be useful for
      restoration, or when we want to remove and replace parts of an
      image. Synthesis is a byproduct of generative modeling which is
      part of deep learning research.
    * Preprocessing involves putting pixels in a range of `(0, 1)` or
      `(-1, 1)`. Often must crop to the same size, but convolutional
      models often don't care.
    * Augmentation: cropping differently, translations, rotations.
    * Other nonlinear distortions of an image.
* Contrast normalization is typially useful.
    * Global contrast normalization takes the overall image, subtracts
      out mean intensity, and then divides by the standard deviation.
    * This effectively maps all examples to a hypersphere, right?
      Because you're renormalizing.
    * Otherwise, neural networks would have to learn how to respond
      the same to images with different contrast-levels, which
      basically means that they would need features which are
      colinear, but with differently scaled biases.
* **TODO**: They explain whitening very briefly, but I don't understand.
* However, we can often lose a lot of valuable information. This can
  happen if half the image is bright and the other half dark. Contrast
  normalization will try to keep those seperated, but maybe lose a lot
  of detail inside those zones.
* Thus local contrast normalization, which does contrast normalization
  in a small window. This can be done as a convolution. Note that LCN
  is sometimes used as a non-linearity applied to hidden layers,
  though my understanding is that is no longer considered very
  helpful.
* Speech Recognition:
    * 20ms frames of audio data, typically spectral intensity
      presumably via FFT.
    * Historically had a HMM for phonemes, then a gaussian mixture
      model to produce audio.
    * First RBMs were used to model phoneme-audio data relationship.
    * 2d convolutional models: one axis is spectral intensities, and
      the other is time.
    * Also RNNs.
    * TIMIT is the main test here.
* NLP
    * Difficult because of the large output space, curse of
      dimensionality.
    * ngram models have zero counts all the time, because of curse of
      dimensionality. So you smooth. You can either smooth all words
      the same, or try to put words in categories and smooth similar
      words together.
    * Another idea is the *cascade* of models: you have a series of
      ngram models, of smaller and smaller ngrams. If there isn't
      enough count for a reliable estimate, you back off to a
      lower-order model.
    * This of couesre leads to word embeddings.
    * Next problem is high-dimensionality of outputs. Mapping from a
      hidden state to a huge softmax is `O(num_hidden *
      vocab_size)`. This is costly at both train and inference times.
    * One approach is the hierarchical softmax. At each node you make
      a decision using logistic regression. That is much faster to
      train, but to get probability distribution over all possible
      words is still very costly. Likewise, to do inference is
      similarly slow.
    * This can have lower performance because categories are sort of
      random.
    * Another idea is *importance sampling*. Here, you backprop the
      positive word, and also some negative ones. This is a proper
      Monte Carlo method if you pick the negative words in proportion
      to their current posterior probability.
    * Because you typically pick uniformly, you want to weight the
      negative sampled words.
    * Again, this doesn't help for inference.
* Neural Machine Translation
    * They just mention encoder/decoder and attention models.
    * They note how encoder/decoder is hard to make work with variable
      sentence sizes.
* Recommendation
    * Collaborative filtering is the typical technique, but you can do
      content-based filtering if you have access to content, which you
      can then use a NN to embed.
    * They talk about exploration vs exploitation in the context of
      recommendation. This is a *contextual bandit problem*. The
      context is what you know about the users or the ad opportunity
      or whatever.
* They mention question answering, but note that performance in this
  domain is weak.
    * They mention several knowledge bases which are available.

## Ch 13: Linear Factor Models

* These are unsupervised approaches. We're finding underlying
  features.
* Simplest is PCA. Then there is *probabilistic PCA*. This models `x`
  as a linear transformation of underlying `h`, plus some gaussian
  noise. `h` is normally assume distributed Gaussian.
* Independent component analysis is used to separate a signal into
  low-level signals that are added together.
    * The general idea is this. If you add multiple signals, they will
      tend to be Gaussian and noisey.
    * So seperate into deterministic components that when subject o
      affine transformation result in the observed signal.
    * But prefer those which are highly independent (have low mutual
      information). Another approach is to prefer those which are most
      non-Gaussian: you know that mixing results in a Gaussian (at the
      limit), so finding the least Gaussian is in a way "least mixed."
      This can be measured by Kurtosis.
    * This is used as a technique for *blind signal separation*. In a
      sense, that is what we are trying to do.
* Slow Feature Analysis
    * Slow feature principle says that features should change little
      frame-to-frame. That implies adding a penalty to features that
      change a lot. Common to use mean-squared difference.
    * So you featurize `x` by finding the linear transformation that
      changes the least frame-to-frame. But you need to require that
      the expectation is zero, else you could just add a constant; so
      non-unique solution. Also, you need to set something like the
      variance to one, so that you don't just always output zero or
      some small number.
    * Last, you want to force zero linear correlation between
      features.
    * This is all solvable in closed form!
    * It is typical to use a quadratic basis expansion by multiplying
      `x_i * x_j`. Then, you can repeat SFA several times even.
    * One problem is that what you really want is to be able to
      predict frame-to-frame, not just have generally invariant
      features. For instance, position may change rapidly if velocity
      is high, but that doesn't mean you should retain position...
* Sparse Coding
    * Again, you have factors which generate `x` by linear
      transformation, with Gaussian noise.
    * You use a Laplace prior to bias with sharp peaks at zero.
    * Instead of encoding using an affine transformation, you actually
      find that code which maximizes `p(h | x)` which involves an
      optimization procedure.
    * For training, you'll do an EM type thing: find the best codes
      `h`s for `x`s, then adjust `W` to maximize probabilities of `x`s
      given `h`s, then repeat.
    * A difficulty is that this can be slow, expecially if you were to
      try to stack them, because of the optimization problem of
      finding the best code.
    * Another problem is it is hard to backpropagage. A common
      technique for factor analysis is to do unsupervised pretraining
      and then use the supervised task to tweak these features.
* All factor models tend to produce poor samples, because of the
  independence assumptions about the underlying `h`.

## Ch14: Autoencoders

* You can restrict the number of units for an autoencoder. Or you can
  try to restrict the expressiveness of the encoder and decoder
  functions.
* For instance:
    * You don't want the identity encoder/decoder.
    * Likewise, you don't want to learn encoder that maps to training
      example number, and decoder that maps training example number to
      that exact example.
* Another approach is to regularize.
* Sparse autoencoders try to minimize reconstruction error while
  maximizing sparsity.
    * This puts a "prior" on the encodings `h`. That's a little
      different than a prior on weights!
* Denoising autoencoders try to minimize reconstruction error when
  some noise is added in. This means that the model is supposed to be
  learning some of the structure.
* Another technique is to penalize based on the gradient of `h` with
  respect to `x`. This is called a *contractive* autoencoder. This
  says the encoding shouldn't change much if the output changes just a
  little.
* Autoencoders are often used for dimensionality reduction. I assume
  it is also convenient to get a featurization for supervised learning
  that can be learned from unlabeled data.
* These lower-dimensional representations can be much more efficient
  to learn on.
* They also talk about how binary codes can make semantic hashing
  easy. In that case, you can hash a query, and look it up in a hash
  map. You can als flip one bit at a time and find those which are
  off-by-one.
    * To learn a binary encoding, you can use sigmoids, and then
      slowly add in larger and larger Gaussian noise, which will bias
      your inputs to the sigmoids to saturate.
    * Then, when encoding, don't add in the gaussian noise, and just
      round to zero or one.

## Ch15: Representation Learning

* They note that division is easy with Arabic numbers but not with
  Roman numerals. Tasks can be easy if you find the right
  representation.
* Feedforward networks can be seen to be learning a representation in
  the last layer, then applying a simple classification procedure.
    * The representation learned in the penultimate layer depends on
      how the final layer works. For instance, the classes should be
      linearly seperable in the embedding described by the penultimate
      layer if the classifier is logistic regression.
* Any task can be used to learn a representation. Obviously
  autoencoders do. And you can learn a representation with one task
  and then use it in another context.
* The idea of the chapter is that we can do semi-supervised learning:
  use unlabeled data to learn a representation, and then use that for
  supervised problems. That fits with huge datasets of unlabeled data.
* The first approach is to do *greedy layerwise unsupervised
  pretraining*. This basically trains a series of autoencoders: each
  layer is the hidden layer in an autoencoder. Then you can fine-tune
  the final result for a supervised task if you like.
    * You can use RBMs, one-layer autoencoders, sparse
      coding... Anything that learns latent representations.
    * This is the discovery of 2006 that kicked off the DL renesaince:
      that you can use this as an initialization for deep networks.
    * Also used as initialization for unsupervised networks like deep
      autoencoders, deep belief nets, deep Boltzman machines.
* They note that sometimes unsupervised pretraining yields worse
  results. That's weird. But first they want to take note of other
  semi-supervised approaches: (1) virtual adversarial, (2) training a
  representation which minimizes a sum of supervised and unsupervised
  losses.
* In the beginning, it was thought that unsupervised pretraining
  started you in a place where you were more likely to fall into a
  local minima that was "good", then if you started in a random
  location. But that presumes that you were going to stop when you
  were at a local minima. In fact, we typically stop before we hit
  local minima. So they say the effect is really a *regularizing*
  effect; that we start somewhere more good and then wander near
  there.
    * Again, I'm not sure I totally understand/buy how early stopping
      is supposed to be a regularizer. In a prior chapter they show
      that it is the same as applying an L2 penalty (when learning a
      linear function).
    * I presume the regularization is relative to the starting
      location?
* They note that pretraining will work better where unsupervised and
  supervised tasks can use similar features. Hopefully the
  unsupervised task finds features that allow you to linearly seperate
  the classes, for instance. They note that you can train unsupervised
  and supervised simultaneously (which is no longer greedy layerwise
  pretraining), and that helps both parts focus on learning shared
  valuable features.
* They note that representation learning will be more helpful when
  initial representations are poor. For instance, one hot word
  vectors. Images may have a rich representation.
    * They note that a major part of "richness" is the extent to which
      L1 or L2 norm accurately describes differences in the
      vectors. For instance: all one hot word vectors are the same
      distance apart, even though semantic meanings can be similar.
* When you have lots of unlabeled data, then unsupervised training can
  be very valuable. Another idea: normal regularization biases you
  toward representations that are "simple" in a certain sense:
  generated by mostly linear functions with weights penalized by L1 or
  L2 loss. When the underlying distribution is very complex, the
  unsupervised task can be a better regularizer.
* Most improvement is seen on the *test* error. It has been observed
  that with unsupervised pretraining, a network typically stays in a
  certain neighborhood and has low variance in the trained network,
  whereas there is very high variance in the trained function if the
  network is randomly initialized.
* They mention that training a network with two phases means you can
  explore hyperparameters slowly: the "real" way to do things would be
  to rerun *both* phases if you want to tweak an unsupervised phase
  parameter. But in practice we just tweak the parameters for the
  unsupervised task and then leave that part alone: we assume we did a
  good enough job.
* Another note is that it's hard to know how to control the amount of
  unsupervised regularization. This is not the case if you train
  supervised and unsupervised together and combine the penalties,
  using some weighting ratio.
* Today, unsupervised pretraining is mostly used for word vectors,
  where distances really suck. But for very large datasets, and also
  even medium size ones like CIFAR and MNIST (5k examples per class),
  regularization with dropout and with batch normalization seem to be
  superior. On very small sets, Bayesian methods are more popular and
  successful.
    * So it sounds like pretraining doesn't really have much it is
      really good at...
* On the other hand, supervised pretraining is popular, and networks
  like VGG even publish their weights for transfer learning.
* Transfer learning in particular can be useful. For visual tasks,
  even if the tasks aren't very similar, edge detection, invariance to
  geometry changes can be highly important.
* On the other hand, sometimes we keep the *last* layers if the
  similarity is in the *input*. That may be the case for transcription
  software, where we really just want to optimize to a specific
  individual's unique *vocal* features.
* In some competitions, it is found that training really deep
  unsupervised representations can really help in tasks where you then
  have to classify new categories. For instance, you train on cats and
  dogs, but then are asked to classify cars and boats.
    * The finding was specifically that you need *many fewer* examples
      from the new categories to reach asymptotic performance, so long
      as your representation learned was very deep.
    * This is sometimes called *one-shot* learning. Using even a
      single example, you can classify new examples which are close to
      the single example in the representation space.
    * That probably works great for, for instance, flower
      identification. Here you can train on some classes, identify the
      important factors of variation, and then a new class comes along
      which is just another specific setting of those factors of
      variation.
* A *zero-shot* problem works like this: you train a classifier that
  can look at images in a class, and a description of the class. Then
  you give it a *description* of a new class. It can then classify
  without ever seeing an example.
* They note that the general hypothesis behind semi-supervised
  learning is that you are learning the causal factors of X, and that
  these causal factors can also explain Y: maybe Y is one of the
  factors!
    * This will totally fail if X is totally random, and Y is purely a
      *function* of X.
    * **I believe this is a vital insight!** Note that you can learn Y
      from X, but modeling X will only help if you think the causes of
      X are highly correlated with Y.
* One problem is that there may be many factors of variation, and that
  if the representation space is constrained, Y may not be amongst the
  top few. They give an example of a robotics task: a representation
  of an image is learned by an autoencoder. But fine details are lost:
  in their example, the arm of the robot is well encoded, but the tiny
  ball it is trying to pick up is *not*.
    * That's because the ball is small in the image, and the
      autoencoder doesn't see it as particularly salient when
      evaluated against the L2 reconstruction cost.
    * But the problem is that the ball is *super* relevant for the
      robotics task! But how should the autoencoder know that?
* One approach is to use an adversary to determine what is
  salient. Say you present the reconstructed image and the original:
  can the adversary figure out which is which?
    * This implicitly learns a better error function.
    * Another example is head generation: the ears are often blurred
      when training on L2 error, but much less so when using an
      adversary, which can see blurred ears easily.
    * **I believe again that this is vital!** Note that instead of
      using a penalty, we are using an adaptively learned adversary!
* They talk about *distributed representations*. I think the point
  they are making is that you ideally learn independent dimensions of
  causality. I think it's like twenty questions: rather than ask "is
  it class 1? Class 2? Class 3?", if you learn features that some of
  the classes share, but others don't, you can narrow down to the
  answer with fewer questions.
* They talk about how these features are only likely to be learned by
  *deep representations*. That's because the features are highly
  nonlinear.
    * Now, they note that with enough units and connections, a
      one-layer hidden network can model anything. But they also note
      that deeper networks can be much more *efficient*.
    * You can prove that some families of functions can be represented
      with a linear number of neurons with a depth of `k`, but would
      require an exponential number of neurons if you didn't provide
      enough depth.
* I think the idea is basically what I have thought about in the past:
  to seperate classes, try to find dimensions of variation that are
  relevant across classes. Basically, try to find features that are
  0/1 for half the classes, rather than just 1 for a single class.

## Ch16: Structured Probabilistic Models

* Examples of PGM advantages are:
    * Density estimation.
    * Denoising.
    * Missing value imputation.
    * Sampling.
* Can't just use a lookup table:
    * Statistical efficiency: you need so much data to train such a
      model.
    * Sampling would mean you have to generate a number `U(0,1)` and
      then iterate through table until you surpass this. (You could
      fix this with preprocessing, actually).
    * But running arbitrary probabilistic queries would be painfully
      slow.
* Graphical structure can greatly limit interacting variables, so if
  CPDs involve few variables, then you can use far fewer parameters.
    * If the structure is convenient (like a tree), certain query
      operations can be pretty easy too. But arbitrary querying can be
      difficult, still.
* Undirected model makes sense when causal direction *could* run
  either way, or where we don't understand the causality. The example
  they give is: you, your coworker and your roommate. The variable is
  whether each of you has a cold. Then you and your coworker, and you
  and your roommate are both related, but it's not clear in what
  direction.
    * Every *clique* (group of nodes each one connected together) gets
      a *potential*. The unnormalized probability is the product of
      the clique potentials. The partition function is the normalizing
      constant.
* We often cosider *enery-based models*. These have the unnormalized
  probability equal `exp(-E(x))`, where `E` can be any kind of
  function you like. A probability distribution written this way is
  called a *Boltzmann distribution*. It sounds like any distribution
  can be considered a Boltzmann. Machines based on an energy model are
  called *Boltzmann machines*.
    * The term Boltzman machine is thus very elastic. But we can
      assume it is only used when the probability distribution is
      *naturally* a Boltzman distribution.
    * These are also called *log-linear* models. It turns out that we
      used "Boltzmann" primarily when there are *latent* variables,
      otherwise we say "log-linear" or just Markov Random Field.
    * The function `-E` is sometimes called *harmony* (mostly by this
      Smolensky guy). NB: the RBM is sometimes called a *harmonium*.
    * Hinton was the guy who figured out how to train these fast.
* We want to know if two sets of variables A and B are conditionally
  independent given C. For undirected models, the answer is exactly if
  every path between A and B involves a vertex in C. This is
  *seperation*.
* For directed models it is slightly more complicated: we talk about
  *d-seperation*. The problem is caused by *v-structure*, which causes
  *the "explaining-away" effect.
* Of course, not every independence will be implied by the graph
  structure. And some independencies may exist based on certain
  *contexts*. Maybe A and B are independent *if* C=1.

**TODO**: Up to 16.2.6.
