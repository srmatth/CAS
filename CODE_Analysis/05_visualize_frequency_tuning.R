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
library(extrafont)
library(tidyr)

# Only run the following line once
#font_import()

# But this line needs to be run once per session
suppressMessages(loadfonts())

# Read in the Data
rf_freq <- read_csv(
  str_c(
    "output/",
    dataset,
    "_freq_tuning/",
    dataset,
    "_rf_ultimate_claim_count_tuning_results.csv"
  )
)
gb_freq <- read_csv(
  str_c(
    "output/",
    dataset,
    "_freq_tuning/",
    dataset,
    "_gb_ultimate_claim_count_tuning_results.csv"
  )
)
nn_freq <- read_csv(
  str_c(
    "output/",
    dataset,
    "_freq_tuning/",
    dataset,
    "_nn_ultimate_claim_count_tuning_results.csv"
  )
)


#### RF Frequency Models ----

rf_freq %>%
  ggplot() +
  aes(x = max_depth, y = logloss, color = as.factor(min_split_improvement)) +
  geom_line(lwd = 1.5) +
  facet_wrap(~mtries) +
  scale_color_viridis_d() +
  # geom_smooth(se = FALSE) +
  theme_classic() +
  xlab("Max Depth") +
  ylab("Logloss") +
  labs(color = "Minimum Split\nImprovement") +
  theme(
    legend.position = c(0.15, 0.3),
    legend.background = element_rect(
      color = "black"
    )
  ) +
  guides(
    color = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1
    )
  )

#### GB Frequency Models ----

gb_freq %>%
  ggplot() +
  aes(x = max_depth, y = logloss, color = as.factor(learn_rate)) +
  geom_line(lwd = 1.5) +
  facet_wrap(~ntrees) +
  theme_classic() +
  xlab("Max Depth") +
  ylab("Logloss") +
  labs(color = "Learn Rate") +
  guides(
    color = guide_legend(
      nrow = 1,
      title.position = "top",
      title.hjust = 0.5
    )
  ) +
  theme(
    legend.position = c(0.3, 0.3),
    legend.background = element_rect(
      color = "black"
    )
  ) +
  scale_color_viridis_d()

#### NN Frequency Models ----
nn_freq %>%
  filter(logloss < 3) %>%
  ggplot() +
  aes(x = total_nodes, y = logloss, color = as.factor(rate)) +
  geom_point()
