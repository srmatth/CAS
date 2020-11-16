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
  dplyr::mutate_at(
    .vars = stringr::str_c("X_VAR", 1:46),
    .funs = as.factor
  ) %>%
  dplyr::select(
    -V1,
    -ULTIMATE_AMOUNT,
    -ULTIMATE_CLAIM_COUNT,
    -X_VAR2,
    -X_VAR19,
    -X_VAR34,
    -X_VAR46
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
  ntrees = c(200, 300, 500),
  max_depth = c(10, 15, 20),
  min_rows = c(1),
  mtries = c(7),
  min_split_improvement = c(.0001),
  sample_rate = c(.632),
  nbins = c(4),
  nbins_cats = c(4, 56),
  binomial_double_trees = c(TRUE, FALSE),
  categorical_encoding = c("OneHotInternal", "Eigen", "LabelEncoder", "EnumLimited"),
  col_sample_rate_per_tree = c(.8),
  stopping_metric = c("AUC", "mean_per_class_error"),
  balance_classes = c(TRUE, FALSE),
  seed = 16
) %>%
  expand.grid(stringsAsFactors = FALSE)

# initialize the data frames where we will save the results
results <- data.frame(stringsAsFactors = FALSE)
varimp <- data.frame(stringsAsFactors = FALSE)

# get the counts for each level in each frame
n_yes_train <- sum(bi_train["CLAIM"] == "yes")
n_no_train <- sum(bi_train["CLAIM"] == "no")
n_yes_valid <- sum(bi_validate["CLAIM"] == "yes")
n_no_valid <- sum(bi_validate["CLAIM"] == "no")
n_yes_test <- sum(bi_test["CLAIM"] == "yes")
n_no_test <- sum(bi_test["CLAIM"] == "no")

# run the loop across all rows of the training grid
print("starting for loop....")
for (i in 1:nrow(grid)) {
  grid_sub <- grid %>% dplyr::slice(i)
  
  tryCatch({
    usethis::ui_info("Starting model {i}")
    tic(stringr::str_c("model", i, sep = " "))
    tmp_mod <- h2o::h2o.randomForest(
      y = "CLAIM",
      training_frame = bi_hf_train,
      validation_frame = bi_hf_test,
      model_id = "temp_forest_mod",
      ntrees = grid_sub$ntrees,
      max_depth = grid_sub$max_depth,
      min_rows = grid_sub$min_rows,
      mtries = grid_sub$mtries,
      min_split_improvement = grid_sub$min_split_improvement,
      sample_rate = grid_sub$sample_rate,
      nbins = grid_sub$nbins,
      nbins_cats = grid_sub$nbins_cats,
      binomial_double_trees = grid_sub$binomial_double_trees,
      categorical_encoding = gird_sub$categorical_encoding,
      seed = grid_sub$seed,
      balance_classes = grid_sub$balance_classes,
      col_sample_rate_per_tree = grid_sub$col_sample_rate_per_tree,
      stopping_metric = grid_sub$stopping_metric,
      stopping_rounds = 5,
      stopping_tolerance = .0001
    )
    time <- toc()
    
    usethis::ui_info("Model {i} trained")
    
    perf <- h2o.performance(tmp_mod, bi_test)
    threshold_max_f1 <- h2o.find_threshold_by_max_metric(perf, "f1")
    threshold_max_accuracy <-  h2o.find_threshold_by_max_metric(perf, "accuracy")
    threshold_max_precision <- h2o.find_threshold_by_max_metric(perf, "precision")
    threshold_max_recall <- h2o.find_threshold_by_max_metric(perf, "recall")
    threshold_max_specificity <- h2o.find_threshold_by_max_metric(perf, "specificity")
    
    threshold_90_recall <- h2o.metric(perf, metric = "recall") %>%
      dplyr::mutate(recall = round(recall, 2)) %>%
      dplyr::filter(recall > .90) %>%
      dplyr::pull(threshold) %>%
      max()
    
    threshold_80_recall <- h2o.metric(perf, metric = "recall") %>%
      dplyr::mutate(recall = round(recall, 2)) %>%
      dplyr::filter(recall > .80) %>%
      dplyr::pull(threshold) %>%
      max()
    
    usethis::ui_info("Model {i} tested")
    
    results_tmp <- data.frame(
      mod_num = i,
      time_to_create = time$toc - time$tic,
      n_yes_train = n_yes_train,
      n_no_train = n_no_train,
      n_yes_valid = n_yes_valid,
      n_no_valid = n_no_valid,
      n_yes_test = n_yes_test,
      n_no_test = n_no_test,
      ntrees = grid_sub$ntrees,
      max_depth = grid_sub$max_depth,
      min_rows = grid_sub$min_rows,
      mtries = grid_sub$mtries,
      min_split_improvement = grid_sub$min_split_improvement,
      sample_rate = grid_sub$sample_rate,
      nbins = grid_sub$nbins,
      nbins_cats = grid_sub$nbins_cats,
      binomial_double_trees = grid_sub$binomial_double_trees,
      categorical_encoding = grid_sub$categorical_encoding,
      seed = grid_sub$seed,
      balance_classes = grid_sub$balance_classes,
      col_sample_rate_per_tree = grid_sub$col_sample_rate_per_tree,
      stopping_metric = grid_sub$stopping_metric,
      stopping_rounds = 5,
      stopping_tolerance = .0001,
      r_2 = h2o.r2(tmp_mod),
      r_2_valid = h2o.r2(tmp_mod, valid = TRUE),
      mse = h2o.mse(tmp_mod, train = TRUE),
      mse_valid = h2o.mse(tmp_mod, valid = TRUE),
      gini = h2o.giniCoef(tmp_mod, train = TRUE),
      gini_valid = h2o.giniCoef(tmp_mod, valid = TRUE),
      auc = h2o.auc(tmp_mod, train = TRUE),
      auc_valid = h2o.auc(tmp_mod, valid = TRUE),
      threshold_max_f1 = threshold_max_f1,
      sensitivity_max_f1 = as.numeric(h2o.sensitivity(perf, threshold_max_f1)),
      specificity_max_f1 = as.numeric(h2o.specificity(perf, threshold_max_f1)),
      precision_max_f1 = as.numeric(h2o.precision(perf, threshold_max_f1)),
      accuracy_max_f1 = as.numeric(h2o.accuracy(perf, threshold_max_f1)),
      threshold_max_accuracy =  threshold_max_accuracy,
      sensitivity_max_accuracy = as.numeric(h2o.sensitivity(perf, threshold_max_accuracy)),
      specificity_max_accuracy = as.numeric(h2o.specificity(perf, threshold_max_accuracy)),
      precision_max_accuracy = as.numeric(h2o.precision(perf, threshold_max_accuracy)),
      accuracy_max_accuracy = as.numeric(h2o.accuracy(perf, threshold_max_accuracy)),
      threshold_max_precision =  threshold_max_precision,
      sensitivity_max_precision = as.numeric(h2o.sensitivity(perf, threshold_max_precision)),
      specificity_max_precision = as.numeric(h2o.specificity(perf, threshold_max_precision)),
      precision_max_precision = as.numeric(h2o.precision(perf, threshold_max_precision)),
      accuracy_max_precision = as.numeric(h2o.accuracy(perf, threshold_max_precision)),
      threshold_max_recall =  threshold_max_recall,
      sensitivity_max_recall = as.numeric(h2o.sensitivity(perf, threshold_max_recall)),
      specificity_max_recall = as.numeric(h2o.specificity(perf, threshold_max_recall)),
      precision_max_recall = as.numeric(h2o.precision(perf, threshold_max_recall)),
      accuracy_max_recall = as.numeric(h2o.accuracy(perf, threshold_max_recall)),
      threshold_max_specificity =  threshold_max_specificity,
      sensitivity_max_specificity = as.numeric(h2o.sensitivity(perf, threshold_max_specificity)),
      specificity_max_specificity = as.numeric(h2o.specificity(perf, threshold_max_specificity)),
      precision_max_specificity = as.numeric(h2o.precision(perf, threshold_max_specificity)),
      accuracy_max_specificity = as.numeric(h2o.accuracy(perf, threshold_max_specificity)),
      threshold_90_recall =  threshold_90_recall,
      sensitivity_90_recall = as.numeric(h2o.sensitivity(perf, threshold_90_recall)),
      specificity_90_recall = as.numeric(h2o.specificity(perf, threshold_90_recall)),
      precision_90_recall = as.numeric(h2o.precision(perf, threshold_90_recall)),
      accuracy_90_recall = as.numeric(h2o.accuracy(perf, threshold_90_recall)),
      threshold_80_recall =  threshold_80_recall,
      sensitivity_80_recall = as.numeric(h2o.sensitivity(perf, threshold_80_recall)),
      specificity_80_recall = as.numeric(h2o.specificity(perf, threshold_80_recall)),
      precision_80_recall = as.numeric(h2o.precision(perf, threshold_80_recall)),
      accuracy_80_recall = as.numeric(h2o.accuracy(perf, threshold_80_recall)),
      stringsAsFactors = FALSE
    )
    
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
readr::write_csv(varimp, stringr::str_c("results_rn_varimp_", Sys.date(), ".csv"))
readr::write_csv(results, stringr::str_c("results_rn_metrics_", Sys.date(), ".csv"))


# terminate R
q()