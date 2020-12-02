# Analyze/Visualize the Summarized Data

# Note that this is an Interactive File, not meant to be run on a server

#### User Inputs ----

# 1. Choose the data set that you wish to use, "bi", "pd", or "coll"
data <- "bi"

# 2. set the relative directory of the summaries (with forward slash at the end)
path_to_summaries <- "inst/summaries/"

#### Setup ----

# Load libraries
library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)

# Read in the summaries

fac_sums <- fread(str_c(path_to_summaries, data, "_fac_sum.csv"))
num_sums <- fread(str_c(path_to_summaries, data, "_num_sum.csv"))

#### Factor Summaries ----

# We are going to look at X_VAR9 specifically, as an example of what can be done

# First, look at the data
fac_sums %>%
  filter(variable == "X_VAR9") %>%
  arrange(as.numeric(level)) %>%
  View()

sum_9 <- fac_sums %>%
  filter(variable == "X_VAR9") %>%
  arrange(as.numeric(level))


#### Numerical Summaries ----

View(num_sums)
