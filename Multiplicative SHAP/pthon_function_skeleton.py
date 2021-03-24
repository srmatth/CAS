import numpy as np
import pandas as pd
import shap
from sklearn.ensemble import GradientBoostingRegressor

# first, we need to create our data
# we want this to be a representative sample with both negative and positive
# values, so we will use a several random uniform distributions:
# one between -10 and 10, one between 0 and 20, and one between -5 and -1.
# Also we will only draw 1000 samples here, to speed it up

np.random.seed(15)

# the covariates
x1 = np.random.uniform(low = -10, high = 10, size = 1000)
x2 = np.random.uniform(low = 0, high = 20, size = 1000)
x3 = np.random.uniform(low = -5, high = -1, size = 1000)

dat = {"x1":x1, "x2":x2, "x3":x3}
X = pd.DataFrame(data = dat)

# the first set of responses
y1 = x1 + x2 + x3
y2 = 2*x1 + 2*x2 + 4*x3
y3 = y1 * y2

# the second set of responses
# z1 = x1 * x2 * x3
# z2 = (x1 - x2)*(x3 - x2)*(x1 - x3)
# z3 = z1 * z2

# Get the models
mody1 = GradientBoostingRegressor(loss = "ls", min_samples_leaf = 2)
mody1.fit(X, y1)
np.mean((mody1.predict(X) - y1)**2)
np.mean(np.abs(mody1.predict(X) - y1))

mody2 = GradientBoostingRegressor(loss = "ls", min_samples_leaf = 2)
mody2.fit(X, y2)
np.mean((mody2.predict(X) - y2)**2) #MSE
np.mean(np.abs(mody2.predict(X) - y2)) #MAE

mody3 = GradientBoostingRegressor(loss = "ls", min_samples_leaf = 2)
mody3.fit(X, y3)
np.mean((mody3.predict(X) - y3)**2)
np.mean(np.abs(mody3.predict(X) - y3))

# Get the explainers and the SHAP values
exy1 = shap.TreeExplainer(mody1)
y1ex = exy1.expected_value
shapy1 = exy1.shap_values(X)

exy2 = shap.TreeExplainer(mody2)
y2ex = exy2.expected_value
shapy2 = exy2.shap_values(X)

exy3 = shap.TreeExplainer(mody3)
y3ex = exy3.expected_value
shapy3 = exy3.shap_values(X)

# check to make sure the additive still holds
mody3.predict(X.iloc[0:2, :])
exy3.expected_value
shapy3[0:1, :]
exy3.expected_value + np.sum(shapy3[0:1, :])

preds3_real = mody1.predict(X) * mody2.predict(X)


preds3 = mody3.predict(X)
# test multiplicative SHAP method

def pred_fun(X):
  mody1.predict(X) * mody2.predict(X)

kernelex = shap.KernelExplainer(pred_fun, X)

