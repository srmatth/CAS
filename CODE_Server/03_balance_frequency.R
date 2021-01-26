# Use Over and Under-Sampling Techniques to Balance Classes for Frequency Modeling

#### User Inputs ----

# 1. Choose the data set that you wish to use, "bi", "pd", or "coll"
# you can also do all three at once
data <- c("bi", "pd", "coll")

# 2. set the relative directory of the data and the output (with forward slash at the end)
data_loc <- "data/"
output_loc <- NULL

#### Setup ----

usethis::ui_info("Loading libraries")
library(caret)
library(usethis)
library(dplyr)
library(data.table)
library(stringr)

for (i in data) {
  
  ui_info("Working on {i} data...")
  
  #### Data Loading ----
  
  ui_info("Reading in the data...")
  df <- fread(str_c(data_loc, i, ".csv"), stringsAsFactors = TRUE) %>%
    mutate(
      severity = ifelse(ULTIMATE_CLAIM_COUNT > 0, ULTIMATE_AMOUNT / ULTIMATE_CLAIM_COUNT, 0),
      log_severity = ifelse(ULTIMATE_CLAIM_COUNT > 0, log(severity), 0),
      ULTIMATE_CLAIM_COUNT = ifelse(ULTIMATE_CLAIM_COUNT > 3, 3, ULTIMATE_CLAIM_COUNT)
    )
  
  ui_info("Creating training, validation, and test data sets")
  train_index <- createDataPartition(as.factor(df$ULTIMATE_CLAIM_COUNT), p = 0.5, list = FALSE)
  
  train_unbalanced <- df[train_index,]
  test_valid <- df[-train_index]
  
  test_index <- createDataPartition(as.factor(test_valid$ULTIMATE_CLAIM_COUNT), p = 0.5, list = FALSE)
  test <- test_valid[test_index,]
  validate <- test_valid[-test_index,]
  
  #### Over and Under Sampling ----
  
  ui_info("Balancing the classes in the train data set")
  
  train_0 <- train_unbalanced %>%
    filter(ULTIMATE_CLAIM_COUNT == 0) %>%
    sample_n(3000000, replace = TRUE)
  train_1 <- train_unbalanced %>%
    filter(ULTIMATE_CLAIM_COUNT == 1) %>%
    sample_n(3000000, replace = TRUE)
  train_2 <- train_unbalanced %>%
    filter(ULTIMATE_CLAIM_COUNT == 2) %>%
    sample_n(3000000, replace = TRUE)
  train_3 <- train_unbalanced %>%
    filter(ULTIMATE_CLAIM_COUNT == 3) %>%
    sample_n(3000000, replace = TRUE)
  
  train <- train_0 %>%
    rbind(train_1) %>%
    rbind(train_2) %>%
    rbind(train_3)
  
  #### Save the Data ----
  
  ui_info("Saving the train/validate/test data sets")
  fwrite(train, stringr::str_c(output_loc, i, "_train.csv"))
  fwrite(validate, stringr::str_c(output_loc, i, "_validate.csv"))
  fwrite(test, stringr::str_c(output_loc, i, "_test.csv"))
  
  ui_done("Finished with {i} data!")
}

#### Quit R ---- 
q(save = "no")