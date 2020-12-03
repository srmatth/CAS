# Compute the SHAP values for all rows of my test data set, based on the model

library(h2o)
library(dplyr)
library(tictoc)

h2o::h2o.init(enable_assertions = FALSE)

bi_hf <- data.table::fread("bi_severity.csv") %>%
  mutate(
    X_VAR10 = as.factor(X_VAR10),
    X_VAR13 = as.factor(X_VAR13),
    X_VAR23 = as.factor(X_VAR23),
    X_VAR18 = as.factor(X_VAR18),
    X_VAR27 = as.factor(X_VAR27),
    X_VAR38 = as.factor(X_VAR38)
  ) %>%
  h2o::as.h2o() %>%
  h2o::h2o.splitFrame(ratios = c(.7, .1), seed = 16)

print("Data in!")

bi_train <- bi_hf[[1]]
bi_validate <- bi_hf[[2]]
bi_test <- bi_hf[[3]]

print("data split!")

mod <- h2o.loadModel("gb_mod")

print("Getting Predictions")
preds <- h2o.predict(mod, bi_test) %>%
  as.data.frame()

print("Writing Predictions!")
data.table::fwrite(preds, "gb_preds.csv")

print("Getting SHAP values!")
shap <- h2o.predict_contributions(mod, bi_test) %>%
  as.data.frame()

print("Writing SHAP values!")
data.table::fwrite(shap, "gb_shap.csv")

print("Writing Test .csv")
data.table::fwrite(bi_test %>% as.data.frame(), "bi_severity_test.csv")

q()