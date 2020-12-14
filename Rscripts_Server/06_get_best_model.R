# Get the Best Overall Model for the Data

#### User Inputs ----

# 1. Choose the data set that you wish to use, "bi", "pd", or "coll"
data <- "bi"

# 2. set the relative directory of the data and the output (with forward slash at the end)
# Note that in this case, the pred_loc is the same file that was the output
# directory in the tuning files)
data_loc <- "data/"
pred_loc <- "output/"
output_loc <- "output/"

# 3. Choose the metric for evaluating the "best" model
metric <- "mse"
# metric <- "mae"

#### Setup ----

usethis::ui_info("Loading Libraries")
library(dplyr)
library(stringr)
library(fs)
library(data.table)
library(rlang)

ui_info("Determining which files to read in...")
severity_pred_files <- dir_ls(pred_loc) %>%
  str_subset(str_c(data, ".*severity_predictions.csv")) %>%
  str_subset("log", negate = TRUE)
log_severity_pred_files <- dir_ls(pred_loc) %>%
  str_subset(str_c(data, ".*log_severity_predictions.csv"))
frequency_pred_files <- dir_ls(pred_loc) %>%
  str_subset(str_c(data, ".*frequency_predictions.csv"))

#### Read in the Data ----

ui_info("Reading in the data!")
test <- fread(str_c(data_loc, i, "_test.csv"), stringsAsFactors = TRUE)
severity_preds <- purrr::map_dfr(
  .x = severity_pred_files,
  .f = ~{
    ui_info("Reading in file: {.x}")
    fread(.x) %>%
      mutate(file = .x)
  }
) %>%
  rbind(
    purrr::map_dfr(
      .x = log_severity_pred_files,
      .f = ~{
        ui_info("Reading in file: {.x}")
        fread(.x) %>%
          mutate(
            predict = exp(predict),
            file = .x
          )
      }
    )
  )

frequency_preds <- purrr::map_dfr(
  .x = frequency_pred_files,
  .f = ~{
    ui_info("Reading in file: {.x}")
    fread(.x) %>%
      mutate(file = .x)
  }
)

#### Figure out all combinations ----

severity_mods <- severity_preds %>%
  select(file, mod_num) %>%
  distinct()
frequency_mods <- frequency_preds %>%
  select(file, mod_num) %>%
  distinct()

#### Combine predictions for all Model Combinations ----

results <- data.frame()
for (i in 1:nrow(severity_mods)) {
  sev_mod <- severity_mods %>%
    slice(i)
  sev_mod_preds <- severity_preds %>%
    filter(
      file == sev_mod$file,
      mod_num == sev_mod$mod_num
    )
  for (j in 1:nrow(frequency_mods)) {
    freq_mod <- frequency_mods %>%
      slice(j)
    combined_preds <- frequency_preds %>%
      filter(
        file == freq_mod$file,
        mod_num == freq_mod$mod_num
      ) %>%
      select(-predict) %>%
      left_join(sev_mod_preds, by = "row_num") %>%
      mutate(
        amount_1_claim = predict,
        amount_2_claim = 2 * predict,
        amount_3_claim = 3 * predict,
        expected_ultimate_amount = amount_1_claim * p1 +
          amount_2_claim * p2 +
          amount_3_claim * p3,
        actual_ultimate_amount = test$ULTIMATE_AMOUNT
      )
    
    results_tmp <- data.frame(
      sev_mod_file = sev_mod$file,
      sev_mod_num = sev_mod$mod_num,
      freq_mod_file = freq_mod$file,
      freq_mod_num = freq_mod$mod_num,
      mae = mean(abs(combined_preds$actual_ultimate_amount - combined_preds$expected_ultimate_amount)),
      mse = mean((combined_preds$actual_ultimate_amount - combined_preds$expected_ultimate_amount)^2),
      stringsAsFactors = FALSE
    )
    
    results <- rbind(results, results_tmp)
    
    fwrite(results, str_c(output_loc, data, "model_comparison.csv"))
  }
}

#### Pull out the "Best" Models ----

best_models <- results %>%
  arrange(!!sym(metric)) %>%
  slice(1)

best_sev_mod <- fread(str_replace(best_models$sev_mod_file, "predictions", "tuning_results")) %>%
  filter(mod_num == best_models$sev_mod_num) %>%
  slice(1)

best_freq_mod <- fread(str_replace(best_models$freq_mod_file, "predictions", "tuning_results")) %>%
  filter(mod_num == best_models$freq_mod_num) %>%
  slice(1)

