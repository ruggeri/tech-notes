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

They talk about some pretraining approaches. Greedy pre-training has
you train up to level `k`, and then use the outputs of this last layer
as a fixed input to train layer `k+1`. The theory on why this works is
that the middle layers get better guidance of how they should organize
themselves.

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

**TODO**
