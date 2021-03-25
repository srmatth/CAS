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
  facet_wrap(
    ~min_split_improvement,
    labeller = label_both
  ) +
  geom_smooth(se = FALSE) +
  theme_bw() +
  xlab("Max Depth") +
  ylab("Validation MAE") +
  labs(color = "Mtry") +
  theme(
    legend.position = c(0.95, 0.8),
    plot.title = element_text(
      hjust = 0.5,
      family = "Times New Roman",
      face = "bold"
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      family = "Times New Roman",
      face = "bold"
    ),
    axis.text = element_text(
      family = "Times New Roman"
    ),
    axis.title = element_text(
      family = "Times New Roman"
    ),
    legend.text = element_text(
      family = "Times New Roman"
    ),
    legend.title = element_text(
      family = "Times New Roman",
      hjust = 0.5
    ),
    legend.box.background = element_rect(
      color = "#000000"
    ),
    strip.text = element_text(
      family = "Times New Roman"
    )
  ) +
  scale_y_continuous(
    limits = c(2700, 2850),
    breaks = c(2700, 2750,2800, 2850),
    minor_breaks = NULL,
    labels = scales::number_format(accuracy = 1, big.mark = ",")
  ) +
  scale_color_viridis_d() +
  ggtitle(
    "Effect of Mtry and Max Depth on MAE",
    "Faceted by Minimum Split Improvement"
  )

## Create a heatmap for max depth and mtry showing MAE

rf_sev %>%
  group_by(max_depth, mtries) %>%
  summarize(avg_mae = mean(mae_valid)) %>%
  ungroup() %>%
  filter(max_depth < 20) %>%
  ggplot() +
  aes(x = as.factor(max_depth), y = as.factor(mtries), fill = avg_mae, label = round(avg_mae, 0)) +
  geom_tile() +
  scale_fill_viridis_c(direction = -1) +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme(
    aspect.ratio = 2/3,
    legend.position = "top",
    plot.title = element_text(
      hjust = 0.5,
      family = "Times New Roman",
      face = "bold"
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      family = "Times New Roman",
      face = "bold"
    ),
    axis.text = element_text(
      family = "Times New Roman"
    ),
    axis.title = element_text(
      family = "Times New Roman"
    ),
    legend.text = element_text(
      family = "Times New Roman"
    ),
    legend.title = element_text(
      family = "Times New Roman",
      hjust = 0.5
    ),
    strip.text = element_text(
      family = "Times New Roman"
    )
  ) +
  xlab("Max Depth") +
  ylab("Mtry") +
  labs(fill = "Validation MAE") +
  guides(
    fill = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = unit(20, "lines"),
      barheight = unit(.5, "lines")
    )
  ) +
  ggtitle("Heatmap of MAE by Mtry and Max Depth")#  +
  # Can add label if wanted, I don't think it looks that good though
  # geom_text(color = "#ffffff", size = 3)
  
 
#### GB Severity Models ----

gb_sev %>%
  # Filter for only the laplace distribution, since it did best
  filter(distribution == "laplace") %>%
  ggplot() +
  aes(x = max_depth, y = mae_valid, color = as.factor(learn_rate)) +
  geom_point() +
  geom_jitter() +
  facet_wrap(~ntrees, labeller = label_both) +
  geom_smooth(method = "loess", se = FALSE) +
  theme_bw() +
  scale_color_viridis_d() +
  labs(color = "Learn Rate") +
  xlab("Max Depth") +
  ylab("Validation MAE") +
  scale_y_continuous(
    limits = c(2500, 2550),
    breaks = c(2500, 2525, 2550),
    minor_breaks = NULL,
    labels = scales::number_format(accuracy = 1, big.mark = ",")
  ) +
  scale_x_continuous(
    limits = c(0, 10),
    breaks = c(0, 5, 10),
    minor_breaks = NULL
  ) +
  theme(
    legend.position = c(0.8, 0.85),
    plot.title = element_text(
      hjust = 0.5,
      family = "Times New Roman",
      face = "bold"
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      family = "Times New Roman",
      face = "bold"
    ),
    axis.text = element_text(
      family = "Times New Roman"
    ),
    axis.title = element_text(
      family = "Times New Roman"
    ),
    legend.text = element_text(
      family = "Times New Roman"
    ),
    legend.title = element_text(
      family = "Times New Roman",
      hjust = 0.5
    ),
    legend.box.background = element_rect(
      color = "#000000"
    ),
    strip.text = element_text(
      family = "Times New Roman"
    )
  ) +
  ggtitle(
    "Effect of Max Depth and Learn Rate on MAE",
    "Faceted by Num Trees"
  )
  

#### NN Severity Models ----

nn_sev %>%
  # rates below this caused severe over-fitting, for a more 
  # informative graph we should filter them out
  filter(rate >= 0.001) %>%
  ggplot() +
  aes(x = total_nodes, y = mae_valid, color = as.factor(rate)) +
  geom_point() +
  geom_jitter() +
  facet_wrap(~distribution, labeller = label_both) +
  geom_smooth(se = FALSE) +
  theme_bw() +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 1, big.mark = ",")
  ) +
  labs(color = "Rate") +
  xlab("Total Nodes") +
  ylab("Validation MAE") +
  ggtitle(
    "Effect of Total Nodes and Rate on MAE",
    "Faceted by Distribution"
  ) +
  theme(
    legend.position = c(0.8, 0.85),
    plot.title = element_text(
      hjust = 0.5,
      family = "Times New Roman",
      face = "bold"
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      family = "Times New Roman",
      face = "bold"
    ),
    axis.text = element_text(
      family = "Times New Roman"
    ),
    axis.title = element_text(
      family = "Times New Roman"
    ),
    legend.text = element_text(
      family = "Times New Roman"
    ),
    legend.title = element_text(
      family = "Times New Roman",
      hjust = 0.5
    ),
    legend.box.background = element_rect(
      color = "#000000"
    ),
    strip.text = element_text(
      family = "Times New Roman"
    )
  ) +
  scale_color_viridis_d()



#### All Log Severity Models ----

## This plot shows the relationship between the MAE and MSE values
## (albeit quite roughly)
gb_log_sev %>%
  select(mae, mae_valid, mse, mse_valid) %>%
  mutate(model_type = "Gradient Boosted Model") %>%
  bind_rows(
    rf_log_sev %>%
      select(mae, mae_valid, mse, mse_valid) %>%
      mutate(model_type = "Random Forest Model")
  ) %>%
  bind_rows(
    nn_log_sev %>%
      select(mae, mae_valid, mse, mse_valid) %>%
      mutate(model_type = "Neural Network Model")
  ) %>%
  filter(mae < 1.05, mse < 1.65) %>%
  magrittr::set_colnames(
    c(
      "MAE",
      "Validation MAE",
      "MSE",
      "Validation MSE",
      "model"
    )
  ) %>%
  ggparcoord(
    columns = 1:4,
    groupColumn = 5,
    scale = "uniminmax",
    alphaLines = 0.5
  ) +
  theme_bw() +
  scale_color_viridis_d() +
  coord_cartesian(expand = FALSE, clip = "off") +
  theme(
    panel.grid.major.x = element_line(
      color = "#000000",
      size = 1
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(25, 25, 10, 25),
    axis.line.x.bottom = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.text.x = element_text(
      family = "Times New Roman",
      size = 12,
      hjust = 0.8
    ),
    axis.title.y = element_text(
      family = "Times New Roman",
      size = 12
    ),
    legend.title = element_blank(),
    legend.text = element_text(
      family = "Times New Roman",
      size = 10
    ),
    legend.position = "top",
    plot.title = element_text(
      family = "Times New Roman",
      hjust = 0.5
    ),
  ) +
  ylab("Scaled Value") +
  ggtitle("Relation between MAE and MSE")


#### Visualize the Time ----

all_times <- gb_log_sev %>%
  select(time_to_create, mae_valid) %>%
  mutate(
    model = "Gradient Boosted Model",
    response = "log(Severity)"
  ) %>%
  bind_rows(
    gb_sev %>%
      select(time_to_create, mae_valid) %>%
      mutate(
        model = "Gradient Boosted Model",
        response = "Severity"
      )
  ) %>%
  bind_rows(
    rf_sev %>%
      select(time_to_create, mae_valid) %>%
      mutate(
        model = "Random Forest Model",
        response = "Severity"
      )
  ) %>%
  bind_rows(
    rf_log_sev %>%
      select(time_to_create, mae_valid) %>%
      mutate(
        model = "Random Forest Model",
        response = "log(Severity)"
      )
  ) %>%
  bind_rows(
    nn_log_sev %>%
      select(time_to_create, mae_valid) %>%
      mutate(
        model = "Neural Network Model",
        response = "log(Severity)"
      )
  ) %>%
  bind_rows(
    nn_sev %>%
      select(time_to_create, mae_valid) %>%
      mutate(
        model = "Neural Network Model",
        response = "Severity"
      )
  )
# A simple donut chart to show which models got how much time
all_times %>%
  group_by(model) %>%
  summarize(time_to_create = sum(time_to_create)) %>%
  ungroup() %>%
  arrange(desc(model)) %>%
  mutate(
    y_max = cumsum(time_to_create),
    y_min = c(0, cumsum(time_to_create)[-3]),
    label_pos = (y_max + y_min) / 2,
    label_val = str_c(round(time_to_create / 3600, 2), " Hours")
  ) %>% 
  ggplot() +
  aes(
    x = 1, 
    y = time_to_create, 
    fill = model
  ) +
  theme_void() +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y") +
  xlim(c(-1, 1.5)) +
  scale_fill_manual(
    values = c(
      "#440154",
      "#39568C",
      "#238A8D"
    )
  ) +
  theme(
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(
      family = "Times New Roman",
      size = 12
    ),
    plot.title = element_text(
      family = "Times New Roman",
      face = "bold",
      hjust = 0.5
    ),
    plot.margin = margin(25, 25, 10, 25)
  ) +
  geom_text(
    mapping = aes(
      x = c(1,1,1),
      y = label_pos,
      label = label_val
    ),
    size = 3,
    color = "#ffffff",
    family = "Times New Roman",
    fontface = "bold"
  ) +
  ggtitle("Time Spent Training Each Type of Model")
