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
