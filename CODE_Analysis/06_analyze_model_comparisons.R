library(readr)
library(dplyr)
library(ggplot2)
library(stringr)
dataset <- "bi"

bi <- read_csv("output/model_comparison/bi_model_comparison.csv")

View(bi)

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

bi_new %>%
  filter(sev_mod_type == "gb", freq_mod_type == "gb") %>%
  View()

bi_new %>%
  group_by(sev_mod_type, freq_mod_type) %>%
  summarize(mae = mean(mae), mse = mean(mse)) %>%
  ungroup() %>%
  View()

sev_files <- fs::dir_ls("output", recurse = TRUE, type = "file", glob = "*/bi_*severity_tuning_results.csv")

sev <- purrr::map_dfr(
  .x = sev_files,
  .f = ~{
    readr::read_csv(.x) %>%
      dplyr::mutate(sev_mod_file = stringr::str_c("output/", fs::path_file(.x))) %>%
      dplyr::select(
        sev_mod_file,
        sev_mod_num = mod_num,
        sev_r_2 = r_2,
        sev_r_2_valid = r_2_valid,
        sev_mse = mse,
        sev_mse_valid = mse_valid,
        sev_mae = mae,
        sev_mae_valid = mae_valid
      )
  }
)

freq_files <- fs::dir_ls("output", recurse = TRUE, type = "file", glob = "*/bi_*ultimate_claim_count_tuning_results.csv")

freq <- purrr::map_dfr(
  .x = freq_files,
  .f = ~{
    readr::read_csv(.x) %>%
      dplyr::mutate(freq_mod_file = stringr::str_c("output/", fs::path_file(.x))) %>%
      dplyr::select(
        freq_mod_file,
        freq_mod_num = mod_num,
        freq_r_2 = r_2,
        freq_r_2_valid = r_2_valid,
        freq_mse = mse,
        freq_mse_valid = mse_valid,
        freq_logloss = logloss
      )
  }
)


all_data <- bi_new %>%
  mutate(
    sev_mod_file = str_replace(sev_mod_file, "predictions", "tuning_results"),
    freq_mod_file = str_replace(freq_mod_file, "predictions", "tuning_results")
  ) %>%
  left_join(sev, by = c("sev_mod_file", "sev_mod_num")) %>%
  left_join(freq, by = c("freq_mod_file", "freq_mod_num"))

all_data %>%
  filter(sev_mod_type != "nn", freq_logloss < 1.5) %>%
  ggplot() +
  aes(x = freq_logloss, y = mae, color = sev_mod_type) +
  geom_point()

all_data %>%
  filter(sev_mod_type != "nn") %>%
  ggplot() +
  aes(x = sev_mae_valid, y = mae, color = freq_mod_type) +
  geom_point()

all_data %>%
  filter(sev_mod_type != "nn") %>%
  ggplot() +
  aes(x = freq_logloss, y = mae, color = freq_mod_type) +
  geom_point()
