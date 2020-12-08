# Create a Baseline Model to predict frequency of claims

#### User Inputs ----

# 1. Choose the data set that you wish to use, "bi", "pd", or "coll"
# or create all three at once
data <- c("bi", "pd", "coll")

# 2. set the relative directory of the data and the output (with forward slash at the end)
data_loc <- "data/"
output_loc <- "output/"

# 3. Determine whether to predict the severity or the log of the severity
response <- "ULTIMATE_CLAIM_COUNT"

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
    mutate(ULTIMATE_CLAIM_COUNT = as.factor(ULTIMATE_CLAIM_COUNT)) %>%
    as.h2o()
  ui_done("Training data read in!")
  validate <- fread(str_c(data_loc, i, "_validate.csv"), stringsAsFactors = TRUE) %>%
    mutate(ULTIMATE_CLAIM_COUNT = as.factor(ULTIMATE_CLAIM_COUNT)) %>%
    as.h2o()
  ui_done("Validation data read in!")
  test <- fread(str_c(data_loc, i, "_test.csv"), stringsAsFactors = TRUE) %>%
    mutate(ULTIMATE_CLAIM_COUNT = as.factor(ULTIMATE_CLAIM_COUNT)) %>%
    as.h2o()
  ui_done("Test data read in!")
  
  ui_done("All data in!")
  
  #### Model Creation ----
  lm <- h2o.glm(
    y = response,
    # Don't include X_VAR2, X_VAR19, X_VAR34, X_VAR46 in data due to constant
    # levels or too many levels
    x = c("EARNED_EXPOSURE", str_c("X_VAR", c(1, 3:18, 20:33, 35:45))),
    training_frame = train,
    model_id = str_c(i, "_baseline_", tolower(response), "_mod"),
    family = "multinomial"
  )
  
  ui_info("Predicting on the test data and saving model...")
  
  perf <- h2o.performance(lm, test)
  
  logloss <- h2o.logloss(perf)
  
  ui_info("The Logloss is: {logloss}")
  
  h2o.saveModel(lm, str_c(output_loc), force = TRUE)
  predictions <- predict(lm, test) %>%
    as.data.frame() %>%
    mutate(mod_num = 1, row_num = 1:nrow(.))
  fwrite(predictions, str_c(output_loc, i, "_baseline_", tolower(response), "_predictions.csv"))
  
  ui_done("Baseline model and data saved for {i} data")
  
}

#### Clean Up ----

h2o.shutdown(prompt = FALSE)
q(save = "no")