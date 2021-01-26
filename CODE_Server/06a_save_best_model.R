# Train and save the Best models for the data

#### User Inputs ----

# 1. Choose the data set that you wish to use, "bi", "pd", or "coll"
data <- "bi"

# 2. set the relative directory of the data and the output (with forward slash at the end)
# Note that in this case, the output_loc should be the same as in 06_get_best_model.R
data_loc <- "data/"
output_loc <- "output/"

# 3. Choose the metric for evaluating the "best" model
metric <- "mse"
# metric <- "mae"

#### Setup ----

usethis::ui_info("Loading libraries")
library(dplyr)
library(h2o)
library(rlang)
library(stringr)
library(usethis)
library(data.table)

ui_info("Reading in data")
results <- fread(str_c(output_loc, data, "_model_comparison.csv"))


#### Pull out the "Best" Models and Save Them ----

# Get the best models based on the metric supplied in the user input section
best_models <- results %>%
  arrange(!!sym(metric)) %>%
  slice(1)
best_sev_mod <- fread(str_replace(best_models$sev_mod_file, "predictions", "tuning_results")) %>%
  filter(mod_num == best_models$sev_mod_num) %>%
  slice(1)
best_freq_mod <- fread(str_replace(best_models$freq_mod_file, "predictions", "tuning_results")) %>%
  filter(mod_num == best_models$freq_mod_num) %>%
  slice(1)

# start H2O
h2o::h2o.init(max_mem_size = "50G")

# Figure out what we should be modeling
if (str_detect(best_models$sev_mod_file, "log_severity")) {
  sev_response <- "log_severity"
} else {
  sev_response <- "severity"
}

# Reading in the data for model creation (don't need test data)
ui_info("Training and saving best Severity Model")
ui_info("Reading in the data...")
train <- fread(str_c(data_loc, data, "_train.csv"), stringsAsFactors = TRUE) %>%
  filter(ULTIMATE_CLAIM_COUNT > 0) %>%
  distinct() %>%
  as.h2o()
ui_done("Training data read in!")
validate <- fread(str_c(data_loc, data, "_validate.csv"), stringsAsFactors = TRUE) %>%
  filter(ULTIMATE_CLAIM_COUNT > 0) %>%
  as.h2o()
ui_done("Validation data read in!")

# determine which model type we need to create, and create it and save it
if (str_detect(best_models$sev_mod_file, "baseline")) {
  
  best_sev <- h2o.glm(
    y = sev_response,
    x = str_c("X_VAR", c(1, 3:18, 20:33, 35:45)),
    training_frame = train,
    validation_frame = validate,
    nfolds = 5,
    model_id = str_c(data, "_best_severity_mod")
  )
  h2o.saveModel(best_sev, output_loc, force = TRUE)
  
} else if (str_detect(best_models$sev_mod_file, "rf")) {
  
  best_sev <- h2o::h2o.randomForest(
    y = sev_response,
    x = str_c("X_VAR", c(1, 3:18, 20:33,35:45)),
    training_frame = train,
    validation_frame = validate,
    nfolds = 5,
    model_id = str_c(data, "_best_severity_mod"),
    ntrees = best_sev_mod$ntrees,
    max_depth = best_sev_mod$max_depth,
    mtries = best_sev_mod$mtries,
    min_split_improvement = best_sev_mod$min_split_improvement,
    sample_rate = best_sev_mod$sample_rate,
    categorical_encoding = best_sev_mod$categorical_encoding,
    histogram_type = best_sev_mod$histogram_type,
    seed = best_sev_mod$seed,
    col_sample_rate_per_tree = best_sev_mod$col_sample_rate_per_tree
  )
  h2o.saveModel(best_sev, output_loc, force = TRUE)
  
} else if (str_detect(best_models$sev_mod_file, "gb")) {
  
  best_sev <- h2o::h2o.gbm(
    y = sev_response,
    x = str_c("X_VAR", c(1, 3:18, 20:33, 35:45)),
    training_frame = train,
    validation_frame = validate,
    nfolds = 5,
    distribution = best_sev_mod$distribution,
    model_id = str_c(data, "_best_severity_mod"),
    ntrees = best_sev_mod$ntrees,
    max_depth = best_sev_mod$max_depth,
    learn_rate = best_sev_mod$learn_rate,
    min_split_improvement = best_sev_mod$min_split_improvement,
    sample_rate = best_sev_mod$sample_rate,
    nbins = best_sev_mod$nbins,
    categorical_encoding = best_sev_mod$categorical_encoding,
    seed = best_sev_mod$seed,
    col_sample_rate_per_tree = best_sev_mod$col_sample_rate_per_tree
  )
  h2o.saveModel(best_sev, output_loc, force = TRUE)
  
} else if (str_detect(best_models$sev_mod_file, "nn")) {
  
  best_sev <- h2o::h2o.deeplearning(
    y = sev_response,
    x = str_c("X_VAR", c(1, 3:18, 20:33, 35:45)),
    training_frame = train,
    validation_frame = validate,
    nfolds = 5,
    model_id = str_c(data, "_best_severity_mod"),
    activation = best_sev_mod$activation,
    hidden = best_sev_mod$hidden[[1]],
    adaptive_rate = best_sev_mod$adaptive_rate,
    rate = best_sev_mod$rate,
    rate_decay = best_sev_mod$rate_decay,
    momentum_start = best_sev_mod$momentum_start,
    momentum_stable = best_sev_mod$momentum_stable,
    input_dropout_ratio = best_sev_mod$input_dropout_ratio,
    initial_weight_distribution = best_sev_mod$initial_weight_distribution,
    initial_weight_scale = best_sev_mod$initial_weight_scale,
    loss = best_sev_mod$loss,
    distribution = best_sev_mod$distribution,
    stopping_metric = best_sev_mod$stopping_metric,
    stopping_tolerance = best_sev_mod$stopping_tolerance,
    categorical_encoding = best_sev_mod$categorical_encoding,
    seed = best_sev_mod$seed,
    mini_batch_size = best_sev_mod$mini_batch_size
  )
  h2o.saveModel(best_sev, output_loc, force = TRUE)
  
}
h2o.rm(train)
h2o.rm(validate)

ui_info("Training and saving best Frequency Model")
ui_info("Reading in the data...")

train <- fread(str_c(data_loc, i, "_train.csv"), stringsAsFactors = TRUE) %>%
  mutate(ULTIMATE_CLAIM_COUNT = as.factor(ULTIMATE_CLAIM_COUNT)) %>%
  as.h2o()
ui_done("Training data read in!")
validate <- fread(str_c(data_loc, i, "_validate.csv"), stringsAsFactors = TRUE) %>%
  mutate(ULTIMATE_CLAIM_COUNT = as.factor(ULTIMATE_CLAIM_COUNT)) %>%
  as.h2o()
ui_done("Validation data read in!")

freq_response <- "ULTIMATE_CLAIM_COUNT"

if (str_detect(best_models$freq_mod_file, "baseline")) {
  
  best_freq <- h2o.glm(
    y = freq_response,
    x = c("EARNED_EXPOSURE", str_c("X_VAR", c(1, 3:18, 20:33, 35:45))),
    training_frame = train,
    validation_frame = validate,
    nfolds = 5,
    model_id = str_c(data, "_best_frequency_mod"),
    family = "multinomial"
  )
  h2o.saveModel(best_freq, output_loc, force = TRUE)
  
} else if (str_detect(best_models$freq_mod_file, "rf")) {
  
  best_freq <- h2o::h2o.randomForest(
    y = freq_response,
    x = c(stringr::str_c("X_VAR", c(1, 3:18, 20:33, 35:45)), "EARNED_EXPOSURE"),
    training_frame = train,
    validation_frame = validate,
    nfolds = 5,
    model_id = str_c(data, "_best_frequency_mod"),
    ntrees = best_freq_mod$ntrees,
    max_depth = best_freq_mod$max_depth,
    mtries = best_freq_mod$mtries,
    min_split_improvement = best_freq_mod$min_split_improvement,
    sample_rate = best_freq_mod$sample_rate,
    categorical_encoding = best_freq_mod$categorical_encoding,
    histogram_type = best_freq_mod$histogram_type,
    seed = best_freq_mod$seed,
    col_sample_rate_per_tree = best_freq_mod$col_sample_rate_per_tree
  )
  h2o.saveModel(best_freq, output_loc, force = TRUE)
  
} else if (str_detect(best_models$freq_mod_file, "gb")) {
  
  best_freq <- h2o::h2o.gbm(
    y = freq_response,
    x = c("EARNED_EXPOSURE", str_c("X_VAR", c(1, 3:18, 20:33, 35:45))),
    training_frame = train,
    validation_frame = validate,
    nfolds = 5,
    distribution = best_freq_mod$distribution,
    model_id = str_c(data, "_best_frequency_mod"),
    ntrees = best_freq_mod$ntrees,
    max_depth = best_freq_mod$max_depth,
    learn_rate = best_freq_mod$learn_rate,
    min_split_improvement = best_freq_mod$min_split_improvement,
    sample_rate = best_freq_mod$sample_rate,
    nbins = best_freq_mod$nbins,
    categorical_encoding = best_freq_mod$categorical_encoding,
    seed = best_freq_mod$seed,
    col_sample_rate_per_tree = best_freq_mod$col_sample_rate_per_tree
  )
  h2o.saveModel(best_freq, output_loc, force = TRUE)
  
} else if (str_detect(best_models$freq_mod_file, "nn")) {
  
  best_freq <- h2o::h2o.deeplearning(
    y = freq_response,
    x = c("EARNED_EXPOSURE", str_c("X_VAR", c(1, 3:18, 20:33, 35:45))),
    training_frame = train,
    validation_frame = validate,
    nfolds = 5,
    model_id = str_c(data, "_best_frequency_mod"),
    activation = best_freq_mod$activation,
    hidden = best_freq_mod$hidden[[1]],
    adaptive_rate = best_freq_mod$adaptive_rate,
    rate = best_freq_mod$rate,
    rate_decay = best_freq_mod$rate_decay,
    momentum_start = best_freq_mod$momentum_start,
    momentum_stable = best_freq_mod$momentum_stable,
    input_dropout_ratio = best_freq_mod$input_dropout_ratio,
    initial_weight_distribution = best_freq_mod$initial_weight_distribution,
    initial_weight_scale = best_freq_mod$initial_weight_scale,
    loss = best_freq_mod$loss,
    distribution = best_freq_mod$distribution,
    stopping_metric = best_freq_mod$stopping_metric,
    stopping_tolerance = best_freq_mod$stopping_tolerance,
    categorical_encoding = best_freq_mod$categorical_encoding,
    seed = best_freq_mod$seed,
    mini_batch_size = best_freq_mod$mini_batch_size
  )
  h2o.saveModel(best_freq, output_loc, force = TRUE)
  
}

ui_done("Best Severity and Frequency models saved to {output_loc}")

#### Clean Up ----
h2o.shutdown(prompt = FALSE)
q(save = "no")