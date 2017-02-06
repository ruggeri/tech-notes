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
