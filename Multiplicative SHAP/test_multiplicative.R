library(readr)
library(dplyr)
shap1 <- read_csv("Multiplicative SHAP/shap1.csv", col_names = c("x1", "x2"))
shap2 <- read_csv("Multiplicative SHAP/shap2.csv", col_names = c("x1", "x2"))
shap3 <- read_csv("Multiplicative SHAP/shap3.csv", col_names = c("x1", "x2"))

shap1_ex <- 0.9916429
shap2_ex <- 1.9689213
shap3_ex <- 2.302656

new_shap = data.frame(
  x1_new = (shap1$x1 * (shap2$x1 + shap1$x2 + shap2_ex) + 
    shap2$x1 * (shap1$x1 + shap1$x2 + shap1_ex)) / 2,
  x2_new = (shap1$x2 * (shap2$x1 + shap2$x2 + shap2_ex) + 
    shap2$x2 * (shap1$x1 + shap1$x2 + shap1_ex)) / 2,
  expec_new = (shap1_ex * (shap2$x1 + shap2$x2 + shap2_ex) + 
    shap2_ex * (shap1$x1 + shap1$x2 + shap1_ex)) / 2
)

new_shap2 <- data.frame(
  x1_more_new = shap1$x1 * (shap2$x1 + shap2$x2) + shap2$x1 * (shap1$x1 + shap1$x2),
  x2_more_new = shap1$x2 * (shap2$x1 + shap2$x2) + shap2$x2 * (shap1$x1 + shap1$x2)
)

num_3_preds <- shap3 %>%
  cbind(new_shap) %>%
  cbind(new_shap2) %>%
  mutate(
    expec = shap3_ex,
    predictions = x1 + x2 + expec,
    predictions_new = x1_new + x2_new + expec_new,
    predictions_diff = predictions - predictions_new
  )


shaps <- shap1 %>% slice(1) %>%
  as.matrix()

x1s <- data.frame(
  x11 = shap1$x1,
  x12 = shap1$x2,
  x21 = shap2$x1,
  x22 = shap2$x2,
  x31 = shap3$x1
) %>%
  mutate(
    all_sign = (x11 < 0 & x21 < 0 & x31 < 0) | (x11 > 0 & x21 > 0 & x31 > 0)
  )

lm <- lm(x31 ~ x11 + x12 + x21 + x22 + x11:x21 + x11:x22 + x21:x12, data = x1s)
summary(lm)

x2s <- data.frame(
  x11 = shap1$x1,
  x12 = shap1$x2,
  x21 = shap2$x1,
  x22 = shap2$x2,
  x32 = shap3$x2
)

lm2 <- lm(x32 ~ x11 + x12 + x21 + x22 + x12:x21 + x12:x22 + x22:x11, data = x2s)
summary(lm2)
