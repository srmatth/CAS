#!/usr/bin/env Rscript

library(h2o)
library(dplyr)
library(tictoc)

# start the h2o cluster
h2o::h2o.init()

# read the data into memory
bi <- data.table::fread("data/bi.csv", stringsAsFactors = TRUE)
print("Data in!")

# select the columns we want from the data
# and make everything a factor except the response and EARNED_EXPOSURE
bi <- bi %>%
  dplyr::mutate(
    loss_cost = ULTIMATE_AMOUNT / EARNED_EXPOSURE
  ) %>%
  dplyr::select(
    -V1,
    -ULTIMATE_AMOUNT,
    -ULTIMATE_CLAIM_COUNT,
    -EARNED_EXPOSURE,
    -CLAIM,
    -X_VAR2,
    -X_VAR19,
    -X_VAR34,
    -X_VAR46,
    -form
  ) %>% 
  dplyr::filter(loss_cost >= 0) %>%
  dplyr::mutate_at(
    .vars = stringr::str_c("X_VAR", c(1, 3:18, 20:33,35:45)),
    .funs = as.factor
  )

# split into training, testing, and validation frames
bi_hf <- h2o::as.h2o(bi) %>%
  h2o::h2o.splitFrame(ratios = c(.5, .25), seed = 16)
bi_train <- bi_hf[[1]]
bi_validate <- bi_hf[[2]]
bi_test <- bi_hf[[3]]
print("data split!")

## create a tuning grid
grid <- list(
  ntrees = c(1000, 1500),
  max_depth = c(1, 2, 3, 5, 7, 10),
  learn_rate = c(.001, .0001),
  min_split_improvement = c(.0001),
  distribution = c("gaussian", "huber"),
  sample_rate = c(.632),
  nbins_cats = c(56),
  categorical_encoding = c("Eigen", "LabelEncoder"),
  col_sample_rate_per_tree = c(.8),
  seed = 16
) %>%
  expand.grid(stringsAsFactors = FALSE)

# initialize the data frames where we will save the results
results <- data.frame(stringsAsFactors = FALSE)
varimp <- data.frame(stringsAsFactors = FALSE)


# run the loop across all rows of the training grid
print("starting for loop....")
for (i in 1:nrow(grid)) {
  grid_sub <- grid %>% dplyr::slice(i)
  
  tryCatch({
    usethis::ui_info("Starting model {i}")
    tic(stringr::str_c("model", i, sep = " "))
    tmp_mod <- h2o::h2o.gbm(
      y = "loss_cost",
      training_frame = bi_train,
      validation_frame = bi_test,
      nfolds = 5,
      distribution = grid_sub$distribution,
      model_id = "temp_gb_mod",
      ntrees = grid_sub$ntrees,
      max_depth = grid_sub$max_depth,
      learn_rate = grid_sub$learn_rate,
      min_rows = grid_sub$min_rows,
      min_split_improvement = grid_sub$min_split_improvement,
      sample_rate = grid_sub$sample_rate,
      nbins = grid_sub$nbins,
      categorical_encoding = grid_sub$categorical_encoding,
      seed = grid_sub$seed,
      col_sample_rate_per_tree = grid_sub$col_sample_rate_per_tree
    )
    time <- toc()
    
    usethis::ui_info("Model {i} trained")
    
    results_tmp <- data.frame(
      mod_num = i,
      time_to_create = time$toc - time$tic,
      nfolds = 5,
      distribution = grid_sub$distribution,
      model_id = "temp_forest_mod",
      ntrees = grid_sub$ntrees,
      max_depth = grid_sub$max_depth,
      learn_rate = grid_sub$learn_rate,
      min_rows = grid_sub$min_rows,
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
    
    usethis::ui_info("Model {i} tested")
    
    varimp_tmp <- h2o.varimp(tmp_mod) %>% dplyr::mutate(mod_num = i)
    
    results <- rbind(results, results_tmp)
    varimp <- rbind(varimp, varimp_tmp)
    usethis::ui_done("Model {i} finished and data saved")
  },
  error = function(e) {
    usethis::ui_oops("Model {i} failed!")
  })
}

# write the results to CSV files
print("writing results...")
readr::write_csv(varimp, stringr::str_c("results_gb_varimp_", Sys.date(), ".csv"))
readr::write_csv(results, stringr::str_c("results_gb_metrics_", Sys.date(), ".csv"))


# terminate R
q()