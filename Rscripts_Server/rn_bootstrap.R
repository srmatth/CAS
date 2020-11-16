#### Setup ----
library(dplyr)
library(h2o)

#### Data Prep ----
bi <- data.table::fread("data/bi.csv", stringsAsFactors = TRUE)

bi <- bi %>%
  dplyr::mutate(
    ULTIMATE_AMOUNT = dplyr::if_else(
      ULTIMATE_AMOUNT < 1000,
      0,
      ULTIMATE_AMOUNT
    ),
    ULTIMATE_CLAIM_COUNT = dplyr::if_else(
      ULTIMATE_AMOUNT > 0,
      dplyr::if_else(
        ULTIMATE_CLAIM_COUNT < 1,
        1,
        round(ULTIMATE_CLAIM_COUNT)
      ),
      0
    )
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

#### H2O Setup ----
h2o.init()
#bi_1_h <- as.h2o(bi_1)

#### Bootstrap Sampling Design ----
df <- data.frame(stringsAsFactors = FALSE)
for (i in 1:50) {
  usethis::ui_info("Loading resampled data for iteration {i}")
  bi_sub <- bi_1[sample(1:nrow(bi_1), nrow(bi_1), replace = TRUE),]
  bi_sub_h <- as.h2o(bi_sub) %>%
    h2o.splitFrame(ratios = c(.75, .125))
  
  bi_train <- bi_sub_h[[1]]
  bi_valid <- bi_sub_h[[2]]
  bi_test <- bi_sub_h[[3]]
  
  n_yes_train <- sum(bi_train["CLAIM"] == "yes")
  n_no_train <- sum(bi_train["CLAIM"] == "no")
  n_yes_valid <- sum(bi_validate["CLAIM"] == "yes")
  n_no_valid <- sum(bi_validate["CLAIM"] == "no")
  n_yes_test <- sum(bi_test["CLAIM"] == "yes")
  n_no_test <- sum(bi_test["CLAIM"] == "no")
  
  usethis::ui_info("Creating random forest for iteration {i}")
  rf_tmp <- h2o.randomForest(
    y = "CLAIM",
    training_frame = bi_train,
    validation_frame = bi_valid,
    model_id = "tmp_mod",
    ntrees = 500,
    max_depth = 10,
    nbins = 4,
    nbins_cats = 4,
    binomial_double_trees = TRUE,
    mtries = 7,
    categorical_encoding = "LabelEncoder",
    balance_classes = TRUE,
    col_sample_rate_per_tree = 0.8,
    stopping_metric = "mean_per_class_error",
    stoppiing_rounds = 5,
    stpping_tolerance = .0001
  )
  usethis::ui_info("Getting and saving metrics...")
  perf <- h2o.performance(tmp_mod, bi_test)
  old_threshold_90_recall <- 0.0010468680
  threshold_90_recall <- h2o.metric(perf, metric = "recall") %>%
    dplyr::mutate(recall = round(recall, 2)) %>%
    dplyr::filter(recall > .90) %>%
    dplyr::pull(threshold) %>%
    max()
  auc <- h2o.auc(rf_tmp, valid = TRUE)
  gini <- h2o.giniCoef(rf_tmp, valid = TRUE)
  tmp_df <- data.frame(
    mod_num = i,
    time_to_create = time$toc - time$tic,
    n_yes_train = n_yes_train,
    n_no_train = n_no_train,
    n_yes_valid = n_yes_valid,
    n_no_valid = n_no_valid,
    n_yes_test = n_yes_test,
    n_no_test = n_no_test,
    ntrees = 500,
    max_depth = 10,
    min_rows = 1,
    mtries = 7,
    sample_rate = .632,
    nbins = 4,
    nbins_cats = 4,
    binomial_double_trees = TRUE,
    categorical_encoding = "LabelEncoder",
    seed = 16,
    balance_classes = TRUE,
    col_sample_rate_per_tree = 0.8,
    stopping_metric = "mean_per_class_error",
    stopping_rounds = 5,
    stopping_tolerance = .0001,
    auc = auc,
    gini = gini,
    threshold_90_recall =  threshold_90_recall,
    sensitivity_90_recall = as.numeric(h2o.sensitivity(perf, threshold_90_recall)),
    specificity_90_recall = as.numeric(h2o.specificity(perf, threshold_90_recall)),
    precision_90_recall = as.numeric(h2o.precision(perf, threshold_90_recall)),
    accuracy_90_recall = as.numeric(h2o.accuracy(perf, threshold_90_recall)),
    old_threshold_90_recall =  old_threshold_90_recall,
    old_sensitivity_90_recall = as.numeric(h2o.sensitivity(perf, old_threshold_90_recall)),
    old_specificity_90_recall = as.numeric(h2o.specificity(perf, old_threshold_90_recall)),
    old_precision_90_recall = as.numeric(h2o.precision(perf, old_threshold_90_recall)),
    old_accuracy_90_recall = as.numeric(h2o.accuracy(perf, old_threshold_90_recall)),
    stringsAsFactors = FALSE
  )
  df <- rbind(df, tmp_df)
  
  usethis::ui_info("Cleaning environment...")
  h2o.rm(bi_sub_h)
  h2o.rm(bi_train)
  h2o.rm(bi_valid)
  h2o.rm(bi_test)
  h2o.rm(rf_tmp)
  usethis::ui_done("Done with iteration {i}")
}

readr::write_csv(df, "rn_bootstrap.csv")
