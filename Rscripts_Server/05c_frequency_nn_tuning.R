# Tune Neural Network Models to predict Frequency of Claims

#### User Inputs ----

# 1. Choose the data set that you wish to use, "bi", "pd", or "coll"
data <- "bi"

# 2. set the relative directory of the data and the output (with forward slash at the end)
data_loc <- "data/"
output_loc <- "output/"

# 3. Determine the Response (This shouldn't change)
response <- "ULTIMATE_CLAIM_COUNT"

# 4. Create a tuning grid
grid <- expand.grid(
  list(
    activation = c("Tanh"),
    hidden = list(100, c(100, 100), c(200, 200), c(100, 100, 100)),
    adaptive_rate = FALSE,
    rate = c(0.1, 0.01, 0.005, 0.001),
    rate_decay = c(0.5),
    momentum_start = c(0.5),
    momentum_stable = 0.99,
    input_dropout_ratio = c(0.1),
    initial_weight_distribution = c("Uniform", "Normal"),
    initial_weight_scale = c(1, 2),
    loss = c("Automatic", "Huber"),
    distribution = c("gaussian", "gamma", "laplace", "huber"),
    stopping_metric = "MAE",
    stopping_tolerance = c("0.001"),
    categorical_encoding = c("EnumLimited"),
    seed = 16,
    mini_batch_size = c(10, 100)
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

train <- fread(str_c(data_loc, data, "_train.csv"), stringsAsFactors = TRUE) %>%
  mutate(ULTIMATE_CLAIM_COUNT = as.factor(ULTIMATE_CLAIM_COUNT)) %>%
  as.h2o()
ui_done("Training data read in!")
validate <- fread(str_c(data_loc, data, "_validate.csv"), stringsAsFactors = TRUE) %>%
  mutate(ULTIMATE_CLAIM_COUNT = as.factor(ULTIMATE_CLAIM_COUNT)) %>%
  as.h2o()
ui_done("Validation data read in!")
test <- fread(str_c(data_loc, data, "_test.csv"), stringsAsFactors = TRUE) %>%
  mutate(ULTIMATE_CLAIM_COUNT = as.factor(ULTIMATE_CLAIM_COUNT)) %>%
  as.h2o()
ui_done("Test data read in!")

ui_done("All data in!")

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
    tmp_mod <- h2o::h2o.deeplearning(
      y = response,
      # Don't include X_VAR2, X_VAR19, X_VAR34, X_VAR46 in data due to constant
      # levels or too many levels
      x = c("EARNED_EXPOSURE", str_c("X_VAR", c(1, 3:18, 20:33, 35:45))),
      training_frame = train,
      validation_frame = validate,
      nfolds = 5,
      model_id = "temp_nn_mod",
      activation = grid_sub$activation,
      hidden = grid_sub$hidden[[1]],
      adaptive_rate = grid_sub$adaptive_rate,
      rate = grid_sub$rate,
      rate_decay = grid_sub$rate_decay,
      momentum_start = grid_sub$momentum_start,
      momentum_stable = grid_sub$momentum_stable,
      input_dropout_ratio = grid_sub$input_dropout_ratio,
      initial_weight_distribution = grid_sub$initial_weight_distribution,
      initial_weight_scale = grid_sub$initial_weight_scale,
      loss = grid_sub$loss,
      distribution = grid_sub$distribution,
      stopping_metric = grid_sub$stopping_metric,
      stopping_tolerance = grid_sub$stopping_tolerance,
      categorical_encoding = grid_sub$categorical_encoding,
      seed = grid_sub$seed,
      mini_batch_size = grid_sub$mini_batch_size
    )
    time <- toc()
    
    ui_info("Model {i} trained")
    
    perf <- h2o.performance(tmp_mod, test)
    
    results_tmp <- data.frame(
      mod_num = i,
      time_to_create = time$toc - time$tic,
      nfolds = 5,
      activation = grid_sub$activation,
      total_nodes = sum(grid_sub$hidden[[1]]),
      num_layers = length(grid_sub$hidden[[1]]),
      adaptive_rate = grid_sub$adaptive_rate,
      rate = grid_sub$rate,
      rate_decay = grid_sub$rate_decay,
      momentum_start = grid_sub$momentum_start,
      momentum_stable = grid_sub$momentum_stable,
      input_dropout_ratio = grid_sub$input_dropout_ratio,
      initial_weight_distribution = grid_sub$initial_weight_distribution,
      initial_weight_scale = grid_sub$initial_weight_scale,
      loss = grid_sub$loss,
      distribution = grid_sub$distribution,
      stopping_metric = grid_sub$stopping_metric,
      stopping_tolerance = grid_sub$stopping_tolerance,
      categorical_encoding = grid_sub$categorical_encoding,
      seed = grid_sub$seed,
      mini_batch_size = grid_sub$mini_batch_size,
      r_2 = h2o.r2(tmp_mod),
      r_2_valid = h2o.r2(tmp_mod, valid = TRUE),
      mse = h2o.mse(tmp_mod, train = TRUE),
      mse_valid = h2o.mse(tmp_mod, valid = TRUE),
      rmse = h2o.rmse(tmp_mod, train = TRUE),
      rmse_valid = h2o.rmse(tmp_mod, valid = TRUE),
      logloss = h2o.logloss(perf),
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
    write_csv(results, str_c(output_loc, data, "_nn_", tolower(response), "_tuning_results.csv"))
    fwrite(predictions, str_c(output_loc, data, "_nn_", tolower(response), "_predictions.csv"))
    ui_done("Model {i} finished and data saved")
  },
  error = function(e) {
    usethis::ui_oops("Model {i} failed! {e}")
  })
}

#### Clean Up ----
h2o.shutdown(prompt = FALSE)
q(save = "no")