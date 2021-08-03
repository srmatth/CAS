#### Taking a Loot at the Best Models ----

#### Setup ----
library(readr)
library(dplyr)
library(ggplot2)
library(stringr)

#### BI Data ----

dataset <- "bi"

bi <- read_csv("output/model_comparison/bi_model_comparison.csv")

bi_new <- bi %>%
  mutate(
    sev_mod_type = case_when(
      str_detect(sev_mod_file, "nn") ~ "nn",
      str_detect(sev_mod_file, "rf") ~ "rf",
      str_detect(sev_mod_file, "gb") ~ "gb",
      TRUE ~ "lm"
    ),
    freq_mod_type = case_when(
      str_detect(freq_mod_file, "nn") ~ "nn",
      str_detect(freq_mod_file, "rf") ~ "rf",
      str_detect(freq_mod_file, "gb") ~ "gb",
      TRUE ~ "lm"
    )
  )

# Get the best model that used tree based model for both parts
best_bi_mod <- bi_new %>%
  arrange(mae) %>%
  slice(1)

# See how the best model compares as far as prediction summaries go
freqs <- readr::read_csv("output/model_comparison/freq_mod_pred_summary.csv") %>%
  filter(str_detect(file, dataset))
sevs <- readr::read_csv("output/model_comparison/sev_mod_pred_summary.csv") %>%
  filter(str_detect(file, dataset))


# frequency part
freq_perf <- freqs %>%
  filter(
    mod_num == best_bi_mod$freq_mod_num,
    file == best_bi_mod$freq_mod_file
  )
plot(density(freqs$mean_p0))
mean(freqs$med_p0 < freq_perf$med_p0)
mean(freqs$mean_p0 < freq_perf$mean_p0)

mean(freqs$mean_p1 < freq_perf$mean_p1)
mean(freqs$mean_p2 < freq_perf$mean_p2)
mean(freqs$mean_p3 < freq_perf$mean_p3)

# severity part
sev_perf <- sevs %>%
  filter(
    mod_num == best_bi_mod$sev_mod_num,
    file == best_bi_mod$sev_mod_file
  )
plot(density(sevs$med))
mean(sevs$med > sev_perf$med)
mean(sevs$avg < sev_perf$avg)

# Get the tuning parameters for the best models

freq_params <- readr::read_csv(
  best_bi_mod %>%
    pull(freq_mod_file) %>%
    str_replace("output", "output/bi_freq_tuning") %>%
    str_replace("predictions", "tuning_results")
) %>%
  filter(mod_num == best_bi_mod$freq_mod_num)
View(freq_params)

sev_params <- readr::read_csv(
  best_bi_mod %>%
    pull(sev_mod_file) %>%
    str_replace("output", "output/bi_severity_tuning") %>%
    str_replace("predictions", "tuning_results")
) %>%
  filter(mod_num == best_bi_mod$sev_mod_num)
View(sev_params)

#### PD Data ----
dataset <- "pd"

pd <- read_csv("output/model_comparison/pd_model_comparison.csv")

View(pd)

pd_new <- pd %>%
  mutate(
    sev_mod_type = case_when(
      str_detect(sev_mod_file, "nn") ~ "nn",
      str_detect(sev_mod_file, "rf") ~ "rf",
      str_detect(sev_mod_file, "gb") ~ "gb",
      TRUE ~ "lm"
    ),
    freq_mod_type = case_when(
      str_detect(freq_mod_file, "nn") ~ "nn",
      str_detect(freq_mod_file, "rf") ~ "rf",
      str_detect(freq_mod_file, "gb") ~ "gb",
      TRUE ~ "lm"
    )
  )

# Get the best model that used tree based model for both parts
best_tree_mod <- pd_new %>%
  dplyr::filter(
    sev_mod_type == "gb" | sev_mod_type == "rf", 
    freq_mod_type == "rf" | freq_mod_type == "gb"
  ) %>%
  arrange(mae) %>%
  slice(1)

# See how the best model compares as far as prediction summaries go
freqs <- readr::read_csv("output/model_comparison/freq_mod_pred_summary.csv") %>%
  filter(str_detect(file, dataset))
sevs <- readr::read_csv("output/model_comparison/sev_mod_pred_summary.csv") %>%
  filter(str_detect(file, dataset))


# frequency part
freq_perf <- freqs %>%
  filter(
    mod_num == best_tree_mod$freq_mod_num,
    file == best_tree_mod$freq_mod_file
  )
plot(density(freqs$mean_p0))
mean(freqs$med_p0 < freq_perf$med_p0)
mean(freqs$mean_p0 < freq_perf$mean_p0)

mean(freqs$mean_p1 < freq_perf$mean_p1)
mean(freqs$mean_p2 < freq_perf$mean_p2)
mean(freqs$mean_p3 < freq_perf$mean_p3)

# severity part
sev_perf <- sevs %>%
  filter(
    mod_num == best_tree_mod$sev_mod_num,
    file == best_tree_mod$sev_mod_file
  )
plot(density(sevs$med))
mean(sevs$med > sev_perf$med)
mean(sevs$avg > sev_perf$avg)

# Get the tuning parameters for the best models

freq_params <- readr::read_csv(
  best_tree_mod %>%
    pull(freq_mod_file) %>%
    str_replace("output", "output/pd_freq_tuning") %>%
    str_replace("predictions", "tuning_results")
) %>%
  filter(mod_num == best_tree_mod$freq_mod_num)
View(freq_params)

sev_params <- readr::read_csv(
  best_tree_mod %>%
    pull(sev_mod_file) %>%
    str_replace("output", "output/pd_severity_tuning") %>%
    str_replace("predictions", "tuning_results")
) %>%
  filter(mod_num == best_tree_mod$sev_mod_num)
View(sev_params)

#### COLL Data ----
dataset <- "coll"

coll <- read_csv("output/model_comparison/coll_model_comparison.csv")

View(coll)

coll_new <- coll %>%
  mutate(
    sev_mod_type = case_when(
      str_detect(sev_mod_file, "nn") ~ "nn",
      str_detect(sev_mod_file, "rf") ~ "rf",
      str_detect(sev_mod_file, "gb") ~ "gb",
      TRUE ~ "lm"
    ),
    freq_mod_type = case_when(
      str_detect(freq_mod_file, "nn") ~ "nn",
      str_detect(freq_mod_file, "rf") ~ "rf",
      str_detect(freq_mod_file, "gb") ~ "gb",
      TRUE ~ "lm"
    )
  )

# Get the best model that used tree based model for both parts
best_tree_mod <- coll_new %>%
  dplyr::filter(
    sev_mod_type == "gb" | sev_mod_type == "rf", 
    freq_mod_type == "rf" | freq_mod_type == "gb"
  ) %>%
  arrange(mae) %>%
  slice(1)

# See how the best model compares as far as prediction summaries go
freqs <- readr::read_csv("output/model_comparison/freq_mod_pred_summary.csv") %>%
  filter(str_detect(file, dataset))
sevs <- readr::read_csv("output/model_comparison/sev_mod_pred_summary.csv") %>%
  filter(str_detect(file, dataset))


# frequency part
freq_perf <- freqs %>%
  filter(
    mod_num == best_tree_mod$freq_mod_num,
    file == best_tree_mod$freq_mod_file
  )
plot(density(freqs$mean_p0))
mean(freqs$med_p0 < freq_perf$med_p0)
mean(freqs$mean_p0 < freq_perf$mean_p0)

mean(freqs$mean_p1 < freq_perf$mean_p1)
mean(freqs$mean_p2 < freq_perf$mean_p2)
mean(freqs$mean_p3 < freq_perf$mean_p3)

# severity part
sev_perf <- sevs %>%
  filter(
    mod_num == best_tree_mod$sev_mod_num,
    file == best_tree_mod$sev_mod_file
  )
plot(density(sevs$med))
mean(sevs$med > sev_perf$med)
mean(sevs$avg > sev_perf$avg)

# Get the tuning parameters for the best models

freq_params <- readr::read_csv(
  best_tree_mod %>%
    pull(freq_mod_file) %>%
    str_replace("output", "output/coll_freq_tuning") %>%
    str_replace("predictions", "tuning_results")
) %>%
  filter(mod_num == best_tree_mod$freq_mod_num)
View(freq_params)

sev_params <- readr::read_csv(
  best_tree_mod %>%
    pull(sev_mod_file) %>%
    str_replace("output", "output/coll_severity_tuning") %>%
    str_replace("predictions", "tuning_results")
) %>%
  filter(mod_num == best_tree_mod$sev_mod_num)
View(sev_params)
