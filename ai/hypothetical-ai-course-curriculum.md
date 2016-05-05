## Math

**Linear Algebra**

* Vector, dot product, matrix, linear transformation
* Inversion, Gaussian elimination
* QR decomposition, Graham-Schmidt, `Q=Q\trans`
* Projection onto a subspace, pseudo-inverse, least squares regression
* Eigenvectors, power method
* Singular Value Decomposition, principal components analysis

**Vector Calculus**

* Partial derivative, gradient, Jacobian
    * Do we ever use the Jacobian?
* Hessian

**Other**

* Fourier transform

## Basic Machine Learning

* Least Squares Regression and Gradient Descent
    * Test/Train/Validation sets
    * Normal equation form (projection onto subspace)
* Classification and Logistic Regression
    * Logistic cost function
    * Regularization
* Naive Bayes (and vs LR)
* Feed-Forward Neural Nets
    * Activation function
    * Backpropagation of errors
    * Random initialization
* Bias/Variance tradeoffs
    * Graph test error and validation error given different amounts of
      data.
    * If validation levels off, you have too much bias.
    * If there's a gap, you need more data to prevent over-fitting.
* SVM
    * Kernel functions
    * SMO
* `k`-means and Expectation Maximization for quantization
    * Mixture of gaussians?
* SVD for dimensionality reduction
* Collaborative Filtering
    * Expectation maximization
    * Low rank matrix factorization

## PGM

There's a lot on PGM, but I'm not sure how vital this stuff is...

* Bayesian Network
* Conditional independence in a network, markov blanket
* Hidden Markov Model
* Plate models
* CPDs can have structure, don't have to be tabular.
    * Can be a decision tree.
    * Noisy OR
    * Sigmoid of a linear transformation of the inputs.
    * Linear gaussian.
* Markov Networks
    * Affinities, not CPDs.
    * Conditional Random Field is a discriminative model.
    * Denoising?
* Variable Elimination
* Belief propagation
* Random sampling via MCMC
    * Forward sampling with rejection.
    * Gibbs Sampling
    * Metropolis-Hastings
* Dirichlet prior distribution
    * How to calculate "pseudo-counts".
* Structure Learning, BIC
* Learning with incomplete data (esp HMM)
    * "Soft-completion"
    * EM

## Data Mining

* tf-idf
* LSH, minhashing, Jacard similarity, cosine similarity
* PageRank, "taxation"
* Fast matching of standing subset queries to documents.
* Mining Social-Networks, affiliation model

## Potential Projects

* Semantic document retrieval
* Speech recognition
* Optical character recognition
* Image segmentation, tracking
* Game playing
* Image classification
* Face detection, recognition
* Spam detection
* Recommendation
* Robotic navigation, drones. Robotic control. Mapping.
* Theorem proving
* Chat bot
* Machine translation

## TODO

  -rw-r--r--   1 ruggeri  staff   2348 Mar 10 21:47 TODO.md
  -rw-r--r--   1 ruggeri  staff  22352 Mar 10 21:47 ai-a-modern-approach.md
  -rw-r--r--   1 ruggeri  staff  22244 Mar 10 21:47 neural-nets.md
