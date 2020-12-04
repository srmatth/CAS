# Tune a Baseline Model to predict Amount per Claim (Severity)

#### User Inputs ----

# 1. Choose the data set that you wish to use, "bi", "pd", or "coll"
# or create all three at once
data <- c("bi", "pd", "coll")

# 2. set the relative directory of the data and the output (with forward slash at the end)
data_loc <- "data/"
output_loc <- "output/"

# 3. Determine whether to predict the severity or the log of the severity
response <- "severity"
# response <- "log_severity"

#### Setup ----

usethis::ui_info("Loading libraries")
library(h2o)
library(usethis)
library(dplyr)
library(data.table)
library(stringr)

# start the h2o cluster
h2o::h2o.init()

for (i in data) {
  
  ui_info("Working on Baseline model for {i} data")
  
  #### Data Loading and Manipulating ----
  
  ui_info("Reading in the data...")
  
  train <- fread(str_c(data_loc, i, "_train.csv"), stringsAsFactors = TRUE) %>%
    filter(ULTIMATE_CLAIM_COUNT > 0) %>%
    as.h2o()
  ui_done("Training data read in!")
  validate <- fread(str_c(data_loc, i, "_validate.csv"), stringsAsFactors = TRUE) %>%
    filter(ULTIMATE_CLAIM_COUNT > 0) %>%
    as.h2o()
  ui_done("Validation data read in!")
  # dont filter test because we want to predict on ALL the test data
  test <- fread(str_c(data_loc, i, "_test.csv"), stringsAsFactors = TRUE) %>%
    as.h2o()
  ui_done("Test data read in!")
  
  ui_done("Data in!")
  
  #### Model Creation ----
  lm <- h2o.glm(
    y = response,
    # Don't include X_VAR2, X_VAR19, X_VAR34, X_VAR46 in data due to constant
    # levels or too many levels
    x = str_c("X_VAR", c(1, 3:18, 20:33,35:45)),
    training_frame = train,
    model_id = str_c(i, "_baseline_", response, "_mod")
  )
  
  ui_info("Predicting on the test data...")
  
  perf <- test %>%
    as.data.frame() %>%
    cbind(as.data.frame(predict(lm, test))) %>%
    mutate(residuals = !!rlang::sym(response) - predict)
  
  # get the MSE on the test dataset
  ui_info("The MSE is: {mean(perf$residuals^2, na.rm = TRUE)}")
  
  # get the RMSE on the test dataset
  ui_info("The RMSE is: {sqrt(mean(perf$residuals^2, na.rm = TRUE))}")
  
  # get the MAE on the test dataset
  ui_info("The MAE is: {mean(abs(perf$residuals), na.rm = TRUE)}")
  
  h2o.saveModel(lm, str_c(output_loc), force = TRUE)
  predictions <- perf %>%
    select(predict) %>%
    mutate(mod_num = 1, row_num = 1:nrow(.))
  fwrite(predictions, str_c(output_loc, i, "_baseline_", response, "_predictions.csv"))
  
  ui_done("Baseline model and data saved for {i} data")
  
}

#### Clean Up ----

h2o.shutdown(prompt = FALSE)
q(save = "no")