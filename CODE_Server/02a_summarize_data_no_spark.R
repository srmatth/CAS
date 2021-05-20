# Get summaries by variable and level for a selected data set

#### User Inputs ----

# 1. Choose the data set that you wish to use, "bi", "pd", or "coll"
data <- "bi"

# 2. set the relative directory of the data and the output (with forward slash at the end)
data_loc <- "data/"
output_loc <- NULL

#### Setup ----

logger::log_info("Loading Libraries...")

# Load libraries
library(dplyr)
library(stringr)
library(data.table)
library(tidyr)
library(ggplot2)
library(tictoc)

logger::log_success("Finished Loading Libraries.")

logger::log_info("Initializing values and reading in the data...")

df <- fread(str_c(data_loc, data, ".csv"))

logger::log_success("Data in!")

#### Factor Summarizing ----
logger::log_info("Beginning to summarize Factor Variables...")
df_long <- df %>%
  dplyr::mutate(
    EARNED_EXPOSURE = as.numeric(EARNED_EXPOSURE),
    ULTIMATE_AMOUNT = as.numeric(ULTIMATE_AMOUNT),
    ULTIMATE_CLAIM_COUNT = as.numeric(ULTIMATE_CLAIM_COUNT)
  ) %>%
  mutate_at(str_c("X_VAR", 1:46), as.character) %>%
  pivot_longer(names_to = "variable", values_to = "level", cols = str_c("X_VAR", 1:46)) %>%
  as.data.table()
logger::log_info("Pivoted the data longer and converted to DT")

fac_sum <- df_long[,.(
  count = .N,
  min_earned_exposure = min(EARNED_EXPOSURE),
  q1_earned_exposure = quantile(EARNED_EXPOSURE, .25),
  med_earned_exposure = quantile(EARNED_EXPOSURE, .5),
  avg_earned_exposure = mean(EARNED_EXPOSURE),
  q3_earned_exposure = quantile(EARNED_EXPOSURE, .75),
  max_earned_exposure = max(EARNED_EXPOSURE),
  tot_earned_exposure = sum(EARNED_EXPOSURE),
  min_ultimate_amount = min(ULTIMATE_AMOUNT),
  q1_ultimate_amount = quantile(ULTIMATE_AMOUNT, .25),
  med_ultimate_amount = quantile(ULTIMATE_AMOUNT, .5),
  avg_ultimate_amount = mean(ULTIMATE_AMOUNT),
  q3_ultimate_amount = quantile(ULTIMATE_AMOUNT, .75),
  max_ultimate_amount = max(ULTIMATE_AMOUNT),
  tot_ultimate_amount = sum(ULTIMATE_AMOUNT),
  min_ultimate_count = min(ULTIMATE_CLAIM_COUNT),
  q1_ultimate_count = quantile(ULTIMATE_CLAIM_COUNT, .25),
  med_ultimate_count = quantile(ULTIMATE_CLAIM_COUNT, .5),
  avg_ultimate_count = mean(ULTIMATE_CLAIM_COUNT),
  q3_ultimate_count = quantile(ULTIMATE_CLAIM_COUNT, .75),
  max_ultimate_count = max(ULTIMATE_CLAIM_COUNT),
  tot_ultimate_count = sum(ULTIMATE_CLAIM_COUNT)
), by = list(variable, level)]

# Save the summary
logger::log_info("Saving factor summaries...")
fwrite(fac_sum, str_c(output_loc, data, "_fac_sum.csv"))
logger::log_success("Finished factor summaries!")

#### Numerical Summarizing ----

logger::log_info("computing summary statistics for numeric variables...")
num_sum <- summary(df$EARNED_EXPOSURE) %>%
  rbind(
    summary(df$ULTIMATE_CLAIM_COUNT)
  ) %>%
  rbind(
    summary(df$ULTIMATE_AMOUNT)
  ) %>% 
  magrittr::set_rownames(1:nrow(.)) %>%
  as_tibble() %>%
  mutate(variable = c("EARNED_EXPOSURE", "ULTIMATE_CLAIM_COUNT", "ULTIMATE_AMOUNT")) %>%
  select(variable, everything())

# save the summary
logger::log_info("saving numeric variable summaries...")
fwrite(num_sum, str_c(output_loc, data, "_num_sum.csv"))
logger::log_success("Done with the numeric summaries and data saved!")

#### Quit R ----
q(save = "no")
