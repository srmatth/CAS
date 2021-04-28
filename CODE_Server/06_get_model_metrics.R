# Get Metrics for all Possible Model Combinations

#### User Inputs ----

# 1. Choose the data set that you wish to use, "bi", "pd", or "coll"
data <- "bi"

# 2. set the relative directory of the data and the output (with forward slash at the end)
# Note that in this case, the pred_loc is the same file that was the output
# directory in the tuning files)
data_loc <- "data/"
pred_loc <- "output/"
output_loc <- "output/"

#### Setup ----

logger::log_info("Loading Libraries")
library(dplyr)
library(stringr)
library(fs)
library(data.table)
library(usethis)
library(logger)

log_info("Determining which files to read in...")
severity_pred_files <- dir_ls(pred_loc) %>%
  str_subset(str_c(data, ".*severity_predictions.csv")) %>%
  str_subset("log", negate = TRUE)
log_severity_pred_files <- dir_ls(pred_loc) %>%
  str_subset(str_c(data, ".*log_severity_predictions.csv"))
all_severity_pred_files <- data.frame(
  file = severity_pred_files,
  transform = FALSE
) %>%
  rbind(
    data.frame(
      file = log_severity_pred_files,
      transform = TRUE
    )
  )

frequency_pred_files <- dir_ls(pred_loc) %>%
  str_subset(str_c(data, ".*ultimate_claim_count_predictions.csv"))

#### Read in the Data ----

log_info("Reading in the test data!")
test <- fread(str_c(data_loc, data, "_test.csv"), stringsAsFactors = TRUE)

log_info("Reading in the Frequency Predictions.")
frequency_preds <- purrr::map_dfr(
  .x = frequency_pred_files,
  .f = ~{
    log_info("Reading in file: {.x}")
    fread(.x) %>%
      mutate(file = .x)
  }
)

#### Figure out all combinations ----

log_info("Getting unique frequency models...")
frequency_mods <- frequency_preds %>%
  select(file, mod_num) %>%
  distinct()

#### Combine predictions for all Model Combinations ----

results <- data.frame()
for (i in 1:nrow(all_severity_pred_files)) {
  log_info("Starting loop {i} out of {nrow(all_severity_pred_files)}.")
  curr_sev_file <- all_severity_pred_files %>%
    slice(i)
  
  log_info("Reading in file: {curr_sev_file$file}")
  severity_preds <- fread(curr_sev_file$file) %>%
    mutate(file = curr_sev_file$file)
  # If it is a log_severity file, we want to untransform the response
  if (curr_sev_file$transform) {
    severity_preds <- severity_preds %>%
      mutate(predict = exp(predict))
  }
  
  log_info("Getting unique models in {curr_sev_file$file}")
  severity_mods <- severity_preds %>%
    select(file, mod_num) %>%
    distinct()
  
  for (j in 1:nrow(severity_mods)) {
    sev_mod <- severity_mods %>%
      slice(j)
    log_info("Computing metrics for model {sev_mod$mod_num} from file {sev_mod$file} with all freq models")
    sev_mod_preds <- severity_preds %>%
      filter(
        file == sev_mod$file,
        mod_num == sev_mod$mod_num
      )
    if (nrow(sev_mod_preds) > nrow(test)) sev_mod_preds <- sev_mod_preds[1,nrow(test),]
    for (k in 1:nrow(frequency_mods)) {
      freq_mod <- frequency_mods %>%
        slice(k)
      log_info("Working on frequency model {freq_mod$mod_num} from file {freq_mod$file}")
      freq_mod_preds <- frequency_preds %>%
        filter(
          file == freq_mod$file,
          mod_num == freq_mod$mod_num
        ) 
      if (nrow(freq_mod_preds) > nrow(test)) freq_mod_preds <- freq_mod_preds[1,nrow(test),]
      combined_preds <- freq_mod_preds %>%
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
      log_info("Predictions calculated and combined with actual!")
      
      results_tmp <- data.frame(
        sev_mod_file = sev_mod$file,
        sev_mod_num = sev_mod$mod_num,
        freq_mod_file = freq_mod$file,
        freq_mod_num = freq_mod$mod_num,
        mae = mean(abs(combined_preds$actual_ultimate_amount - combined_preds$expected_ultimate_amount)),
        mse = mean((combined_preds$actual_ultimate_amount - combined_preds$expected_ultimate_amount)^2),
        stringsAsFactors = FALSE
      )
      log_info("metrics computed")
      
      results <- rbind(results, results_tmp)
      
      fwrite(results, str_c(output_loc, data, "_model_comparison.csv"))
      log_success("metrics saved")
    }
  }
}

#### Clean Up ----
q(save = "no")

