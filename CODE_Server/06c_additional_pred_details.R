# Get metrics about the individual model average predictions
# Across all data sets

# This will allow us to see if some models have lower overall MAE's just
# because the frequency model is always predicting 0 claims

# 1. Enter the tuning results directory
tune_dir <- "output/"

# 2. Enter the output directory 
out_dir <- "output/"

#### Setup ----

# Load Libraries
library(data.table)
library(dplyr)
library(purrr)
library(fs)
library(stringr)

# Get all the files
freq_files <- dir_ls(tune_dir) %>%
  stringr::str_subset("_ultimate_claim_count_predictions.csv")
sev_files <- dir_ls(tune_dir) %>%
  stringr::str_subset("_severity_predictions.csv")

#### Summarize the Frequency Predictions ----

map_dfr(
  .x = freq_files,
  .f = ~{
    logger::log_info("Working on file {.x}")
    fread(.x) %>%
      as.data.frame() %>%
      group_by(mod_num) %>%
      summarize(
        min_p0 = min(p0),
        q1_p0 = quantile(p0, 0.25),
        med_p0 = median(p0),
        mean_p0 = mean(p0),
        q3_p0 = quantile(p0, 0.75),
        max_p0 = max(p0),
        min_p1 = min(p1),
        q1_p1 = quantile(p1, 0.25),
        med_p1 = median(p1),
        mean_p1 = mean(p1),
        q3_p1 = quantile(p1, 0.75),
        max_p1 = max(p1),
        min_p2 = min(p2),
        q1_p2 = quantile(p2, 0.25),
        med_p2 = median(p2),
        mean_p2 = mean(p2),
        q3_p2 = quantile(p2, 0.75),
        max_p2 = max(p2),
        min_p3 = min(p3),
        q1_p3 = quantile(p3, 0.25),
        med_p3 = median(p3),
        mean_p3 = mean(p3),
        q3_p3 = quantile(p3, 0.75),
        max_p3 = max(p3),
        pct_0 = mean(predict == 0),
        pct_1 = mean(predict == 1),
        pct_2 = mean(predict == 2),
        pct_3 = mean(predict == 3)
      ) %>%
      ungroup() %>%
      mutate(file = .x)
  }
) %>%
  fwrite(
    str_c(out_dir, "freq_mod_pred_summary.csv")
  )

#### Summarize the Severity Predictions ----

map_dfr(
  .x = sev_files,
  .f = ~{
    logger::log_info("Working on file {.x}")
    d <- fread(.x) %>%
      as.data.frame() 
    if (str_detect(.x, "_log_")) {
      d <- d %>%
        mutate(predict = exp(predict))
    }
    d %>%
      group_by(mod_num) %>%
      summarize(
        min = min(predict),
        q1 = quantile(predict, 0.25),
        med = median(predict),
        avg = mean(predict),
        q3 = quantile(predict, 0.75),
        max = max(predict)
      )
  }
) %>%
  fwrite(
    str_c(out_dir, "sev_mod_pred_summary.csv")
  )


#### Clean Up ----
q(save = "no")
