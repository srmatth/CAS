source("mSHAP/functions.R")
library(tictoc)
library(ggplot2)
library(dplyr)

eqns <- list(
  y1s = c(
    "x1 + x2 + x3",
    "2*x1 + 2*x2 + 3*x3",
    "x1 * x2 * x3",
    "x1^2 * x2^3 * x3^4",
    "(x1 + x2) / (x1 + x2 + x3)",
    "x1 * x2 / (x1 + x1 * x2 + x1^2 * x3^2)",
    "exp(x1 + x2 + x3)",
    "abs(x1 + x2 + x3)",
    "abs(x1 - x2) + abs(x2 - x3)",
    "(exp(x1) * abs(x2) + x1 * x2) / (x3^2 + abs(x1 - x2))",
    "(x1 + x2) / (x2 + x3) + (x2 - x3) / (x1 + x3)"
  ),
  y2s = c(
    "x1 + x2 + x3",
    "2*x1 + 2*x2 + 3*x3",
    "x1 * x2 * x3",
    "x1^2 * x2^3 * x3^4",
    "(x1 + x2) / (x1 + x2 + x3)",
    "x1 * x2 / (x1 + x1 * x2 + x1^2 * x3^2)",
    "exp(x1 + x2 + x3)",
    "abs(x1 + x2 + x3)",
    "abs(x1 - x2) + abs(x2 - x3)",
    "(exp(x1) * abs(x2) + x1 * x2) / (x3^2 + abs(x1 - x2))",
    "(x1 + x2) / (x2 + x3) + (x2 - x3) / (x1 + x3)"
  )
) %>%
  expand.grid(stringsAsFactors = FALSE)


all_eqns <- purrr::map2_dfr(
  .x = eqns$y1s,
  .y = eqns$y2s,
  .f = test_multiplicative_shap,
  sample = 200L,
  theta1 = 0.9,
  theta2 = 7
)

tic("shorter version")
all_eqns2 <- purrr::map2_dfr(
  .x = eqns$y1s[1:11],
  .y = eqns$y2s[1:11],
  .f = test_multiplicative_shap,
  sample = 100L,
  theta1 = 10,
  theta2 = 10
)
toc() # 554.594 seconds


readr::write_csv(all_eqns, "Multiplicative SHAP/test_1_results.csv")

all_eqns <- readr::read_csv("Multiplicative SHAP/test_1_results.csv")

all_eqns %>%
  dplyr::group_by(method, variable) %>%
  summarize(
    pct_smae_sign = mean(pct_same_sign),
    pct_same_rank = mean(pct_same_rank),
    direction_contrib = mean(direction_contrib),
    relative_mag_contrib = mean(relative_mag_contrib),
    rank_contrib = mean(rank_contrib),
    score = mean(score)
  ) %>% View()

all_eqns2 %>%
  dplyr::group_by(method, variable, theta1, theta2, sample) %>%
  summarize(
    pct_same_sign = mean(pct_same_sign),
    pct_same_rank = mean(pct_same_rank),
    direction_contrib = mean(direction_contrib),
    relative_mag_contrib = mean(relative_mag_contrib),
    rank_contrib = mean(rank_contrib),
    score = mean(score)
  ) %>% View()




############################################################
# real test

source("Multiplicative SHAP/functions.R")
library(tictoc)

eqns <- list(
  y1 = c(
    "x1 + x2 + x3",
    "2*x1 + 2*x2 + 3*x3"
  ),
  y2 = c(
    "x1 + x2 + x3",
    "2*x1 + 2*x2 + 3*x3",
    "x1 * x2 * x3",
    "x1^2 * x2^3 * x3^4",
    "(x1 + x2) / (x1 + x2 + x3)",
    "x1 * x2 / (x1 + x1 * x2 + x1^2 * x3^2)"
  ),
  theta1 = seq(0.5, 20.5, by = 1),
  theta2 = seq(1, 50, by = 5)
) %>%
  expand.grid(stringsAsFactors = FALSE)

tic("big one")
all_tests <- purrr::pmap_dfr(
  .l = eqns,
  .f = test_multiplicative_shap,
  sample = 100L
)
toc() # 11021.87 seconds (~3 hours)

readr::write_csv(all_tests, "Multiplicative SHAP/all_tests_results.csv")

all_tests <- readr::read_csv("Multiplicative SHAP/all_tests_results.csv")

## We can do groupings in several different ways

# Start with various metrics, which way of distributing alpha is the best?
all_tests %>%
  group_by(method) %>%
  summarise(mean_score = mean(score)) %>%
  ungroup() %>%
  arrange(desc(mean_score)) # weighted_abs is the best for the score

all_tests %>%
  group_by(method) %>%
  summarise(
    mean_score = mean(score),
    mean_dir_con = mean(direction_contrib),
    mean_rel_val_con = mean(relative_mag_contrib),
    mean_rank_con = mean(rank_contrib),
    pct_same_sign = mean(pct_same_sign),
    pct_same_rank = mean(pct_same_rank)
  ) %>%
  ungroup() %>%
  arrange(desc(mean_score))

all_tests %>%
  group_by(method) %>%
  summarise(mean_dir_con = mean(direction_contrib)) %>%
  ungroup() %>%
  arrange(desc(mean_dir_con)) # weighted abs is the best for the direction

all_tests %>%
  group_by(method) %>%
  summarise(mean_rel_val_con = mean(relative_mag_contrib)) %>%
  ungroup() %>%
  arrange(desc(mean_rel_val_con)) # weighted abs is the best for the relative magnitude

all_tests %>%
  group_by(method) %>%
  summarise(mean_rank_con = mean(rank_contrib)) %>%
  ungroup() %>%
  arrange(desc(mean_rank_con)) # weighted abs is the best for the rank contribution

all_tests %>%
  group_by(method) %>%
  summarise(mean_mae = mean(mae)) %>%
  ungroup() %>%
  arrange(mean_mae) # uniform is the best for the mean_mae

all_tests %>%
  group_by(method) %>%
  summarise(pct_same_sign = mean(pct_same_sign)) %>%
  ungroup() %>%
  arrange(desc(pct_same_sign)) # weighted_abs is the best with the pct same sign

all_tests %>%
  group_by(method) %>%
  summarise(pct_same_rank = mean(pct_same_rank)) %>%
  ungroup() %>%
  arrange(desc(pct_same_rank)) # weighted squared is the best with the pct same rank (just .0005 ahead of weighted abs)

# Weighted abs all but swept the trophy stand, so I think we have a pretty clear overall winner
# now we have to make sure that it isn't biased towards one type of equation or anything

all_tests %>%
  group_by(y1, y2, method) %>%
  summarise(score = mean(score), .groups = "drop_last") %>%
  arrange(desc(score)) %>%
  filter(method == first(method)) %>%
  pull(method) %>%
  `==`("weighted_abs") %>%
  mean()

all_tests %>%
  group_by(y1, y2, method) %>%
  summarise(direction_contrib = mean(direction_contrib), .groups = "drop_last") %>%
  arrange(desc(direction_contrib)) %>%
  filter(method == first(method)) %>%
  pull(method) %>%
  `==`("weighted_abs") %>%
  mean()

all_tests %>%
  group_by(y1, y2, method) %>%
  summarise(relative_mag_contrib = mean(relative_mag_contrib), .groups = "drop_last") %>%
  arrange(desc(relative_mag_contrib)) %>%
  filter(method == first(method)) %>%
  pull(method) %>%
  `==`("weighted_abs") %>%
  mean()

all_tests %>%
  group_by(y1, y2, method) %>%
  summarise(rank_contrib = mean(rank_contrib), .groups = "drop_last") %>%
  arrange(desc(rank_contrib)) %>%
  filter(method == first(method)) %>%
  pull(method) %>%
  `==`("weighted_abs") %>%
  mean()

## Create some plots

all_tests %>% 
  filter(
    y1 == "x1 + x2 + x3",
    y2 == "x1 + x2 + x3"
  ) %>%
  group_by(method, y1, y2, theta1, theta2) %>%
  summarize(score = mean(score)) %>%
  ungroup() %>%
  mutate(
    method = case_when(
      method == "uniform" ~ "Uniformly Distributed",
      method == "weighted_abs" ~ "Weighted by Absolute Values",
      method == "weighted_raw" ~ "Weighted by Raw Values",
      method == "weighted_squared" ~ "Weighted by Squared Values"
    )
  ) %>%
  ggplot() +
  aes(x = theta1, y = score, color = as.factor(theta2)) +
  facet_wrap(~method) + 
  theme_bw() + 
  xlab(expression(paste("Value of ", theta[1]))) +
  ylab("Average Score") +
  ggtitle(
    expression(paste("Effect of ", theta[1],  " on Score")), 
    expression(paste("Shown by Method and ", theta[2]))
  ) +
  labs(color = expression(theta[2])) +
  scale_color_manual(
    values = c(
      "#335C67",
      "#66827A",
      "#99A88C",
      "#CCCE9E",
      "#FFF3B0",
      "#F0C977",
      "#E09F3E",
      "#BF6535",
      "#9E2A2B",
      "#540B0E"
    )
  ) +
  geom_smooth(se = FALSE, method = "lm") +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

all_tests %>% 
  filter(
    y1 == "x1 + x2 + x3",
    y2 == "x1 + x2 + x3"
  ) %>%
  group_by(method, y1, y2, theta1, theta2) %>%
  summarize(score = mean(score)) %>%
  ungroup() %>%
  mutate(
    method = case_when(
      method == "uniform" ~ "Uniformly Distributed",
      method == "weighted_abs" ~ "Weighted by Absolute Values",
      method == "weighted_raw" ~ "Weighted by Raw Values",
      method == "weighted_squared" ~ "Weighted by Squared Values"
    )
  ) %>%
  ggplot() +
  aes(x = theta2, y = score) +
  # geom_point() +
  facet_wrap(~method) + 
  theme_bw() +
  xlab(expression(paste("Value of ", theta[2]))) +
  ylab("Average Score") +
  ggtitle(
    expression(paste("Effect of ", theta[2], " on Score")), 
    "Shown by Method"
  ) +
  # labs(color = "Theta 1") +
  geom_smooth(se = FALSE, color = "#540B0E")  +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  )

all_tests %>% 
  filter(
    y1 == "x1 + x2 + x3",
    y2 == "x1 + x2 + x3"
  )  %>%
  ggplot() +
  aes(x = direction_contrib, y = relative_mag_contrib, color = theta1, text = stringr::str_c(y1, ", ", y2)) +
  geom_point()

all_tests %>% 
  filter(
    y1 == "x1 + x2 + x3",
    y2 == "(x1 + x2) / (x1 + x2 + x3)"
  )  %>%
  ggplot() +
  aes(x = direction_contrib, y = relative_mag_contrib, color = theta2, text = stringr::str_c(y1, ", ", y2)) +
  geom_point()


params <- list(
  sample = c(100L, 200L, 500L, 1000L, 5000L, 10000L), # 10000L, 50000L),
  # get rid of 10000 and 50000, taking too much time 
  # 10000 took 262 seconds and 50000 took 1314.5 seconds with only 2 variables
  vars = c(2L) #5L, 10L, 20L, 46L)
) %>% expand.grid()

all_times_2_vars <- purrr::map2_dfr(
  .x = params$sample,
  .y = params$vars,
  .f = compare_times
)

readr::write_csv(all_times_2_vars, "Multiplicative SHAP/all_times_2_vars.csv")
all_times_2_vars <- readr::read_csv("Multiplicative SHAP/all_times_2_vars.csv")

all_times_2_vars %>%
  tidyr::pivot_longer(
    cols = c("kernel_time", "multiplicative_time"),
    names_to = "method",
    values_to = "time"
  ) %>%
  mutate(method = ifelse(method == "kernel_time", "KernelSHAP", "Multiplicative TreeSHAP")) %>%
  ggplot() +
  aes(x = smaple_size, y = time, color = method) +
  geom_line(lwd = 1) +
  theme_classic() +
  scale_color_manual(values = c("#E09F3E", "#335C67")) +
  xlab("Sample Size") +
  ylab("Time (seconds)") +
  ggtitle("Comparison of Time by Method", "Number of Variables Fixed at 2") +
  labs(color = "Method") +
  theme(
    legend.position = c(0.8, 0.4),
    legend.box.background = element_rect(color = "#99A88C"),
    plot.title = element_text(hjust = 0.5, vjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5)
  )

params <- list(
  sample = c(100L),
  vars = c(2:20, 25, 30, 35, 40, 45, 50, 60, 70, 80, 90, 100)
) %>% expand.grid()

all_times_100_sample <- purrr::map2_dfr(
  .x = params$sample,
  .y = params$vars,
  .f = compare_times
)

readr::write_csv(all_times_100_sample, "Multiplicative SHAP/all_times_100_sample.csv")
all_times_100_sample <- readr::read_csv("Multiplicative SHAP/all_times_100_sample.csv")

all_times_100_sample %>%
  tidyr::pivot_longer(
    cols = c("kernel_time", "multiplicative_time"),
    names_to = "method",
    values_to = "time"
  ) %>%
  mutate(method = ifelse(method == "kernel_time", "KernelSHAP", "Multiplicative TreeSHAP")) %>%
  ggplot() +
  aes(x = num_variables, y = time, color = method) +
  geom_line(lwd = 1) +
  theme_classic() +
  ylab("Time (seconds)") +
  xlab("Number of Variables")+
  scale_color_manual(values = c("#E09F3E", "#335C67")) +
  ggtitle("Comparison of Time by Method", "Sample Size Fixed at 100") +
  labs(color = "Method") +
  theme(
    legend.position = c(0.8, 0.4),
    legend.box.background = element_rect(color = "#99A88C"),
    plot.title = element_text(hjust = 0.5, vjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5)
  )


# test individual timing plots
all_times_100_sample %>%
  ggplot() +
  aes(x = num_variables, y = multiplicative_time)+
  geom_point()

all_times_2_vars %>%
  ggplot() +
  aes(x = smaple_size, y = multiplicative_time)+
  geom_point()

all_times_100_sample %>%
  mutate(kernel_time_per_iteration = kernel_time / 100) %>%
  View()
# with 45 variables, it is at 2.268 seconds per iteration
# Assuming 100 background samples would be enough, we have 5,000,000
# observations in out test data set

(5000000 * 2.268) / (3600 * 24) # 131.25 days

# It would take over 130 days to get explanations for all of our test set

# meanwhile, the multiplicative method takes .175 seconds to do all 100
# or in other words, .00175 per iteration

(5000000 * .00175) / (3600)

# Meanwhile, it would take a little less than three hours to compute
# the explanations for multiplicative SHAP on my personal laptop

# Literally almost 1300 times faster!

