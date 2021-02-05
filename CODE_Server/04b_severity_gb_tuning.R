# Tune Gradient Boosted Models to predict Amount per Claim (Severity)

#### User Inputs ----

# 1. Choose the data set that you wish to use, "bi", "pd", or "coll"
data <- "pd"

# 2. set the relative directory of the data and the output (with forward slash at the end)
data_loc <- "data/"
output_loc <- "output/"

# 3. Determine whether to predict the severity or the log of the severity
# response <- "severity"
response <- "log_severity"

# 4. Determine a frequency to save the predictions (ie. every "save_freq"th model the predictions get saved)
save_freq <- 25

# 5. Create a tuning grid
grid <- expand.grid(
  list(
    ntrees = c(300, 500, 1000),
    max_depth = c(1, 2, 3, 5, 7, 10),
    learn_rate = c(.001, .0001),
    min_split_improvement = c(.0001),
    distribution = c("gaussian", "gamma"),
    sample_rate = c(.632),
    nbins_cats = c(56),
    categorical_encoding = c("Eigen"),
    col_sample_rate_per_tree = c(.8),
    seed = 16
  ),
  stringsAsFactors = FALSE
)

#### Setup ----

usethis::ui_info("Loading libraries")
library(h2o)
library(tictoc)
library(usethis)
library(dplyr)
library(readr)
library(data.table)
library(stringr)

# start the h2o cluster
h2o::h2o.init(max_mem_size = "50G")

#### Data Loading and Manipulating ----

ui_info("Reading in the data...")

train_mem <- fread(str_c(data_loc, data, "_train.csv"), stringsAsFactors = TRUE) %>%
  filter(
    ULTIMATE_CLAIM_COUNT > 0) %>%
  distinct() 
if (response == "log_severity") {
  train_mem <- train_mem %>%
    filter(severity > 1)
}
train <- train_mem %>%
  as.h2o()
ui_done("Train data read in!")
validate_mem <- fread(str_c(data_loc, data, "_validate.csv"), stringsAsFactors = TRUE) %>%
  filter(
    ULTIMATE_CLAIM_COUNT > 0) 
if (response == "log_severity") {
  validate_mem <- validate_mem %>%
    filter(severity > 1)
}
validate <- validate_mem %>%
  as.h2o()
ui_done("Validation data read in!")
# dont filter test because we want to predict on ALL the test data
test_mem <- fread(str_c(data_loc, data, "_test.csv"), stringsAsFactors = TRUE) 
test <- test_mem %>%
  as.h2o()
ui_done("Test data read in!")

ui_done("Data in!")

#### Train Models and Record Results ----

# initialize the data frames where we will save the results
results <- data.frame(stringsAsFactors = FALSE)
predictions <- data.frame(stringsAsFactors = FALSE)

# run the loop across all rows of the training grid
ui_info("Starting for loop....")
for (i in 1:nrow(grid)) {
  grid_sub <- grid %>% dplyr::slice(i)
  
  if (grid_sub$distribution %in% c("huber", "laplace")) {
    ui_info("Sys.sleep to appropraitely shut down and reboot H2O...")
    tryCatch({
      h2o.shutdown(prompt = FALSE)
    },
    error = function(e) {ui_oops("Shutdown failed: {e}")})
    Sys.sleep(10)
    
    h2o::h2o.init(max_mem_size = "50G")
    
    ui_info("Copying the data back to H2O...")
    
    train <- train_mem %>%
      as.h2o()
    ui_done("Train data copied!")
    validate <- validate_mem %>%
      as.h2o()
    ui_done("Validation data copied!")
    test <- test_mem %>%
      as.h2o()
    ui_done("Test data copied!")
  }
  
  tryCatch({
    usethis::ui_info("Starting model {i}")
    tic(stringr::str_c("model", i, sep = " "))
    tmp_mod <- h2o::h2o.gbm(
      y = response,
      # Don't include X_VAR2, X_VAR19, X_VAR34, X_VAR46 in data due to constant
      # levels or too many levels
      x = str_c("X_VAR", c(1, 3:18, 20:33, 35:45)),
      training_frame = train,
      validation_frame = validate,
      nfolds = 5,
      distribution = grid_sub$distribution,
      model_id = "temp_gb_mod",
      ntrees = grid_sub$ntrees,
      max_depth = grid_sub$max_depth,
      learn_rate = grid_sub$learn_rate,
      min_split_improvement = grid_sub$min_split_improvement,
      sample_rate = grid_sub$sample_rate,
      nbins = grid_sub$nbins,
      categorical_encoding = grid_sub$categorical_encoding,
      seed = grid_sub$seed,
      col_sample_rate_per_tree = grid_sub$col_sample_rate_per_tree,
      max_runtime_sec = 1200
    )
    time <- toc()
    
    ui_info("Model {i} trained")
    
    results_tmp <- data.frame(
      mod_num = i,
      time_to_create = time$toc - time$tic,
      nfolds = 5,
      distribution = grid_sub$distribution,
      model_id = "temp_forest_mod",
      ntrees = grid_sub$ntrees,
      max_depth = grid_sub$max_depth,
      learn_rate = grid_sub$learn_rate,
      min_split_improvement = grid_sub$min_split_improvement,
      sample_rate = grid_sub$sample_rate,
      nbins = grid_sub$nbins,
      categorical_encoding = grid_sub$categorical_encoding,
      seed = grid_sub$seed,
      col_sample_rate_per_tree = grid_sub$col_sample_rate_per_tree,
      r_2 = h2o.r2(tmp_mod),
      r_2_valid = h2o.r2(tmp_mod, valid = TRUE),
      mse = h2o.mse(tmp_mod, train = TRUE),
      mse_valid = h2o.mse(tmp_mod, valid = TRUE),
      rmse = h2o.rmse(tmp_mod, train = TRUE),
      rmse_valid = h2o.rmse(tmp_mod, valid = TRUE),
      rmsle = h2o.rmsle(tmp_mod, train = TRUE),
      rmsle_valid = h2o.rmsle(tmp_mod, valid = TRUE),
      mae = h2o.mae(tmp_mod),
      mae_valid = h2o.mae(tmp_mod, valid = TRUE),
      stringsAsFactors = FALSE
    )
    
    ui_info("Model {i} metrics calculated")
    
    predictions_tmp <- predict(tmp_mod, test) %>%
      as.data.frame() %>%
      mutate(
        mod_num = i,
        row_num = 1:nrow(.)
      )
    
    ui_info("Model {i} predictions made")
    
    h2o.rm(tmp_mod)
    
    results <- rbind(results, results_tmp)
    predictions <- rbind(predictions, predictions_tmp)
    write_csv(results, str_c(output_loc, data, "_gb_", response, "_tuning_results.csv"))
    # only write the predictions data frame once every so often, as it takes a long time
    if (i %% save_freq == 0 | i == nrow(grid)) {
      ui_info("Saving Predictions...")
      fwrite(predictions, str_c(output_loc, data, "_gb_", response, "_predictions.csv"))
    }
    ui_done("Model {i} finished and data saved")
  },
  error = function(e) {
    usethis::ui_oops("Model {i} failed!")
    if (i %% save_freq == 0 | i == nrow(grid)) {
      ui_info("Saving Predictions...")
      fwrite(predictions, str_c(output_loc, data, "_gb_", response, "_predictions.csv"))
    }
  })
}

#### Clean Up ----
h2o.shutdown(prompt = FALSE)
q(save = "no")
