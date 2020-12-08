# Tune Random Forest Models to predict Amount per Claim (Severity)

#### User Inputs ----

# 1. Choose the data set that you wish to use, "bi", "pd", or "coll"
data <- "bi"

# 2. set the relative directory of the data and the output (with forward slash at the end)
data_loc <- "data/"
output_loc <- "output/"

# 3. Determine whether to predict the severity or the log of the severity
response <- "severity"
# response <- "log_severity"

# 4. Create a tuning grid
grid <- expand.grid(
  list(
    ntrees = c(100, 300, 500),
    max_depth = c(3, 5, 7, 10, 15, 20, 30),
    min_split_improvement = c(.01, .001, .0001),
    mtries = c(-1, 7, 20),
    histogram_type = c("UniformAdaptive"),
    sample_rate = c(.632),
    categorical_encoding = c("EnumLimited"),
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
h2o::h2o.init()

#### Data Loading and Manipulating ----

ui_info("Reading in the data...")

train <- fread(str_c(data_loc, data, "_train.csv"), stringsAsFactors = TRUE) %>%
  filter(ULTIMATE_CLAIM_COUNT > 0) %>%
  as.h2o()
ui_done("Train data read in!")
validate <- fread(str_c(data_loc, data, "_validate.csv"), stringsAsFactors = TRUE) %>%
  filter(ULTIMATE_CLAIM_COUNT > 0) %>%
  as.h2o()
ui_done("Validation data read in!")
# dont filter test because we want to predict on ALL the test data
test <- fread(str_c(data_loc, data, "_test.csv"), stringsAsFactors = TRUE) %>%
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
  tryCatch({
    ui_info("Starting model {i}")
    tic(stringr::str_c("model", i, sep = " "))
    tmp_mod <- h2o::h2o.randomForest(
      y = response,
      # Don't include X_VAR2, X_VAR19, X_VAR34, X_VAR46 in data due to constant
      # levels or too many levels
      x = str_c("X_VAR", c(1, 3:18, 20:33,35:45)),
      training_frame = train,
      validation_frame = validate,
      nfolds = 5,
      model_id = "temp_gb_mod",
      ntrees = grid_sub$ntrees,
      max_depth = grid_sub$max_depth,
      mtries = grid_sub$mtries,
      min_split_improvement = grid_sub$min_split_improvement,
      sample_rate = grid_sub$sample_rate,
      categorical_encoding = grid_sub$categorical_encoding,
      histogram_type = grid_sub$histogram_type,
      seed = grid_sub$seed,
      col_sample_rate_per_tree = grid_sub$col_sample_rate_per_tree
    )
    time <- toc()
    
    ui_info("Model {i} trained")
    
    results_tmp <- data.frame(
      mod_num = i,
      time_to_create = time$toc - time$tic,
      nfolds = 5,
      ntrees = grid_sub$ntrees,
      max_depth = grid_sub$max_depth,
      mtries = grid_sub$mtries,
      min_split_improvement = grid_sub$min_split_improvement,
      sample_rate = grid_sub$sample_rate,
      categorical_encoding = grid_sub$categorical_encoding,
      histogram_type = grid_sub$histogram_type,
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
    
    results <- rbind(results, results_tmp)
    predictions <- rbind(predictions, predictions_tmp)
    write_csv(results, str_c(output_loc, data, "_rf_", response, "_tuning_results.csv"))
    fwrite(predictions, str_c(output_loc, data, "_rf_", response, "_predictions.csv"))
    ui_done("Model {i} finished and data saved")
  },
  error = function(e) {
    ui_oops("Model {i} failed! {e}")
  })
}

#### Clean Up ----
h2o.shutdown(prompt = FALSE)
q(save = "no")