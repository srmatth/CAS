#### Create the lambda Function Plots ####

# This file will create the plots used to visualize and understand the lambda functions
# used in the scoring equation for the methods of distributing alpha

#### Create the Data ----

x <- seq(-10, 10, by = 0.05)
y <- seq(-10, 10, by = 0.05)

lambdas <- list(x = x, y = y) %>% 
  expand.grid() %>%
  mutate(
    lambda1 = ifelse(
      x * y > 0,
      1,
      ifelse(
        (2 * 3) / (abs(x) + abs(y) + 3) > 1,
        1,
        (2 * 3) / (abs(x) + abs(y) + 3)
      )
    ),
    lambda2 = ifelse(
      (1 + 3) / (abs(x - y) + 1) > 1,
      1,
      (1 + 3) / (abs(x - y) + 1)
    )
  )

#### Lambda 1 ----

lambdas %>%
  ggplot() +
  aes(x = as.factor(x), y = as.factor(y), fill = lambda1) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme(
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    legend.position = "none"
  ) +
  xlab(expression(s[zij])) +
  ylab(expression(k[zij]))

#### Lambda 2 ----

lambdas %>%
  ggplot() +
  aes(x = as.factor(x), y = as.factor(y), fill = lambda2) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme(
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    legend.position = "none"
  ) +
  xlab(expression(s[zij])) +
  ylab(expression(k[zij]))
