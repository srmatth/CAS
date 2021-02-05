import numpy as np
import pandas as pd
import os

path = os.getcwd()
print(path)

## Create fake data
np.random.seed(16)
x1 = np.random.uniform(low = 0, high = 1, size = 10000)
x2 = np.random.uniform(low = 0, high = 1, size = 10000)
y1 = x1 + x2
y2 = 2 * x1 + 2 * x2
y3 = (x1 + x2) * (2 * x1 + 2 * x2)

## Create Data Frame
d = {"x1":x1, "x2":x2, "y1":y1, "y2":y2, "y3":y3}
mod_dat = pd.DataFrame(data = d)

## Save the Data
mod_dat.to_csv("Multiplicative SHAP/data1.csv")
