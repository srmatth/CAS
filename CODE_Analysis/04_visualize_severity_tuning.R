#### Analyze PD Severity Tuning ####

#### Setup ----
dataset <- "pd" # or "bi" or "coll"
# Noe that if the dataset is changed, limits on many of the plots will need
# to be changed as well

# Load Libraries
library(readr)
library(dplyr)
library(ggplot2)
library(GGally)
library(stringr)

# Read in Data
rf_log_sev <- read_csv(
  str_c(
    "output/",
    dataset,
    "_severity_tuning/",
    dataset,
    "_rf_log_severity_tuning_results.csv"
  )
)
rf_sev <- read_csv(
  str_c(
    "output/",
    dataset,
    "_severity_tuning/",
    dataset,
    "_rf_severity_tuning_results.csv"
  )
)
gb_log_sev <- read_csv(
  str_c(
    "output/",
    dataset,
    "_severity_tuning/",
    dataset,
    "_gb_log_severity_tuning_results.csv"
  )
)
gb_sev <- read_csv(
  str_c(
    "output/",
    dataset,
    "_severity_tuning/",
    dataset,
    "_gb_severity_tuning_results.csv"
  )
)
nn_log_sev <- read_csv(
  str_c(
    "output/",
    dataset,
    "_severity_tuning/",
    dataset,
    "_nn_log_severity_tuning_results.csv"
  )
)
nn_sev <- read_csv(
  str_c(
    "output/",
    dataset,
    "_severity_tuning/",
    dataset,
    "_nn_severity_tuning_results.csv"
  )
)

#### RF Severity Models ----

# This plot shows how the combination of max_depth, mtry, and min_split_improvement
# combine to affect the MAE
rf_sev %>%
  # filter(min_split_improvement < 0.01) %>%
  ggplot() +
  aes(x = max_depth, y = mae_valid, color = as.factor(mtries)) +
  geom_point() +
  geom_jitter() +
  facet_wrap(~min_split_improvement) +
  geom_smooth(se = FALSE) +
  theme_bw() +
  xlab("Max Depth") +
  ylab("Validation MAE") +
  labs(color = "Mtry") +
  theme(
    legend.position = c(0.95, 0.8),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  scale_y_continuous(
    limits = c(2700, 2850),
    breaks = c(2700, 2750,2800, 2850),
    minor_breaks = NULL
  ) +
  scale_color_viridis_d() +
  ggtitle(
    "Effect of Mtry and Max Depth on MAE",
    "Shown by Minimum Split Improvement"
  )






gb_log_sev %>%
  mutate(learn_rate = as.factor(learn_rate)) %>%
  ggparcoord(
    columns = c(19, 20, 23,24),
    groupColumn = 8,
    scale = "globalminmax",
    alphaLines = 0.5
  ) +
  theme_classic() +
  scale_color_viridis_d()



