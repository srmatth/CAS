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
library(purrr)
library(furrr)
library(future)

log_info("Determining which files to read in...")
severity_pred_files <- dir_ls(pred_loc) %>%
  str_subset(str_c(data, ".*severity_predictions.csv"))
frequency_pred_files <- dir_ls(pred_loc) %>%
  str_subset(str_c(data, ".*ultimate_claim_count_predictions.csv"))

all_files <- list(
  sev = severity_pred_files,
  freq = frequency_pred_files
) %>%
  expand.grid()

#### Read in the Data ----

log_info("Reading in the test data!")
test <- fread(str_c(data_loc, data, "_test.csv"), stringsAsFactors = TRUE)

#### Run the Comparison ----

log_info("Starting the mapping")
source("06b_compute_metrics_function.R")

all_results <- map2_dfr(
  .x = all_files$sev,
  .y = all_files$freq,
  .f = compute_metrics,
  test = test
)

fwrite(all_results, str_c(output_loc, data, "_model_comparison.csv"))
log_success("metrics saved")

#### Clean Up ----
q(save = "no")