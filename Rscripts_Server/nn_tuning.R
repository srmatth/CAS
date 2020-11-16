#!/usr/bin/env Rscript

library(h2o)
library(dplyr)
library(tictoc)

# start the h2o cluster
h2o::h2o.init()

# read the data into memory
bi <- data.table::fread("bi_severity.csv", stringsAsFactors = TRUE) %>%
  dplyr::mutate(log_sev = log(severity)) %>%
  dplyr::select(-severity)
print("Data in!")

# select the columns we want from the data
# and make everything a factor except the response and EARNED_EXPOSURE
#bi <- bi %>%
#  dplyr::mutate(
#    loss_cost = ULTIMATE_AMOUNT / EARNED_EXPOSURE
#  ) %>%
#  dplyr::select(
#    -V1,
#    -ULTIMATE_AMOUNT,
#    -ULTIMATE_CLAIM_COUNT,
#    -EARNED_EXPOSURE,
#    -CLAIM,
#    -X_VAR2,
#    -X_VAR19,
#    -X_VAR34,
#    -X_VAR46,
#    -form
#  ) %>% 
#  dplyr::filter(loss_cost > 0) %>%
#  dplyr::mutate_at(
#    .vars = stringr::str_c("X_VAR", c(1, 3:18, 20:33,35:45)),
#    .funs = as.factor
#  )

# split into training, testing, and validation frames
bi_hf <- h2o::as.h2o(bi) %>%
  h2o::h2o.splitFrame(ratios = c(.7, .1), seed = 16)
bi_train <- bi_hf[[1]]
bi_validate <- bi_hf[[2]]
bi_test <- bi_hf[[3]]
print("data split!")

## create a tuning grid
grid <- list(
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
) %>%
  expand.grid(stringsAsFactors = FALSE)

# initialize the data frames where we will save the results
results <- data.frame(stringsAsFactors = FALSE)
varimp <- data.frame(stringsAsFactors = FALSE)


# run the loop across all rows of the training grid
print("starting for loop....")
for (i in 1:nrow(grid)) {
  # Sys.sleep(10)
  # h2o.init()
  # bi_hf <- h2o::as.h2o(bi) %>%
  #   h2o.splitFrame(ratios = c(.7, .1), seed = 16)
  # bi_train <- bi_hf[[1]]
  # bi_validate <- bi_hf[[2]]
  # bi_test <- bi_hf[[3]]
  
  grid_sub <- grid %>% dplyr::slice(i)
  
  tryCatch({
    usethis::ui_info("Starting model {i}")
    tic(stringr::str_c("model", i, sep = " "))
    tmp_mod <- h2o::h2o.deepLearning(
      y = "log_sev",
      training_frame = bi_train,
      validation_frame = bi_validate,
      nfolds = 5,
      model_id = "temp_nn_mod",
      activation = grid_sub$activation,
      hidden = grid_sub$hidden,
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
    
    usethis::ui_info("Model {i} trained")
    
    results_tmp <- data.frame(
      mod_num = i,
      time_to_create = time$toc - time$tic,
      nfolds = 5,
      activation = grid_sub$activation,
      total_nodes = sum(grid_sub$hidden),
      num_layers = length(grid_sub$hidden),
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
      rmsle = h2o.rmsle(tmp_mod, train = TRUE),
      rmsle_valid = h2o.rmsle(tmp_mod, valid = TRUE),
      mae = h2o.mae(tmp_mod),
      mae_valid = h2o.mae(tmp_mod, valid = TRUE),
      stringsAsFactors = FALSE
    )
    
    usethis::ui_info("Model {i} tested")
    
    varimp_tmp <- h2o.varimp(tmp_mod) %>% dplyr::mutate(mod_num = i)
    
    results <- rbind(results, results_tmp)
    varimp <- rbind(varimp, varimp_tmp)
    readr::write_csv(varimp, "nn_tuning_varimp_log.csv")
    readr::write_csv(results, "nn_tuning_results_log.csv")
    usethis::ui_done("Model {i} finished and data saved")
  },
  error = function(e) {
    usethis::ui_oops("Model {i} failed! {e}")
  })
  print("shutdown h2o")
  h2o.shutdown(prompt = FALSE)
}

# terminate R
q()