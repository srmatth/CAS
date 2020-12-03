# Use Over and Under-Sampling Techniques to Balance Classes for Frequency Modeling

#### User Inputs ----

# 1. Choose the data set that you wish to use, "bi", "pd", or "coll"
data <- "bi"

# 2. set the relative directory of the data and the output (with forward slash at the end)
data_loc <- "data/"
output_loc <- NULL

#### Setup ----

usethis::ui_info("Loading libraries")
library(caret)
library(usethis)
library(dplyr)
library(data.table)

#### Data Loading ----

ui_info("Reading in the data...")
df <- fread(str_c(data_loc, data, ".csv"), stringsAsFactors = TRUE)

ui_info("Creating training, validation, and test data sets")
train_index <- createDataPartition(as.factor(df$ULTIMATE_CLAIM_COUNT), p = 0.5, list = FALSE)

train_unbalanced <- df[train_index,]
test_valid <- df[-train_index]

test_index <- createDataPartition(as.factor(test_valid$ULTIMATE_CALIM_COUNT), p = 0.5, list = FALSE)
test <- test_valid[test_index,]
validate <- test_valid[-test_index,]

#### Over and Under Sampling ----

ui_info("Getting the number of levels")
levels <- df %>%
  group_by(ULTIMATE_CLAIM_COUNT) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  filter(n > 5) %>% 
  pull(ULTIMATE_CLAIM_COUNT)

ui_info("Balancing the classes in the train data set")


