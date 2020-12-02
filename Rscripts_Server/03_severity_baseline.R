# Tune a Baseline Model to predict Amount per Claim (Severity)

#### User Inputs ----

# 1. Choose the data set that you wish to use, "bi", "pd", or "coll"
data <- "bi"

# 2. set the relative directory of the data and the output (with forward slash at the end)
data_loc <- "data/"
output_loc <- NULL

# 3. Determine whether to predict the severity or the log of the severity
response <- "severity"
# response <- "log_severity"

#### Setup ----

usethis::ui_info("Loading libraries")
library(h2o)
library(usethis)
library(dplyr)
library(data.table)

# start the h2o cluster
h2o::h2o.init()

#### Data Loading and Manipulating ----

ui_info("Initializing values and reading in the data...")

df <- fread(str_c(data_loc, data, ".csv"), stringsAsFactors = TRUE) %>%
  filter(ULTIMATE_CLAIM_COUNT > 0) %>%
  mutate(
    severity = ULTIMATE_AMOUNT / ULTIMATE_CLAIM_COUNT,
    log_severity = log(severity)
  )

ui_done("Data in!")

# split into training, testing, and validation frames
ui_info("Splitting data...")
df_hf <- as.h2o(df) %>%
  h2o.splitFrame(ratios = c(.7, .1), seed = 16)
train <- df_hf[[1]]
validate <- df_hf[[2]]
test <- df_hf[[3]]
ui_done("Data split!")

#### Model Creation ----
lm <- h2o.glm(
  y = response,
  # Don't include X_VAR2, X_VAR19, X_VAR34, X_VAR46 in data due to constant
  # levels or too many levels
  x = str_c("X_VAR", c(1, 3:18, 20:33,35:45)),
  training_frame = train,
  model_id = str_c(data, "_baseline_mod")
)

perf <- test %>%
  as.data.frame() %>%
  cbind(preds = as.data.frame(predict(lm, test))) %>%
  mutate(residuals = severity - preds)

# get the MSE on the test dataset
ui_info("The MSE is: {mean(perf$residuals^2)}")

# get the RMSE on the test dataset
ui_info("The RMSE is: {sqrt(mean(perf$residuals^2))}")

# get the MAE on the test dataset
ui_info("The MAE is: {mean(abs(perf$residuals))}")

h2o.saveModel(lm, str_c(output_loc))
ui_done("Baseline model saved")

#### Clean Up ----

h2o.shutdown(prompt = FALSE)
q(save = "no")