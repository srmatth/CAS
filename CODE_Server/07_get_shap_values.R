# Compute the SHAP values for all rows of the Test Data Set

# please note that this requires the model to be a random forest or gradient boosted currently
# also still not sure if this will work for the multinomial model...

## UPDATE This file will not run currently

#### User Inputs ----

# 1. Choose the data set that you wish to use, "bi", "pd", or "coll"
data <- "bi"

# 2. set the relative directory of the data and the output (with forward slash at the end)
# Note that in this case, the output_loc should be the same as in 06_get_best_model.R
data_loc <- "data/"
output_loc <- "output/"

#### Setup ----

usethis::ui_info("Loading libraries")
library(dplyr)
library(h2o)
library(stringr)
library(usethis)
library(data.table)

ui_info("Starting H2O cluster...")
h2o::h2o.init(enable_assertions = FALSE, max_mem_size = "50G")

ui_info("Reading in test data...")
test <- fread(str_c(data_loc, data, "_test.csv"), stringsAsFactors = TRUE) %>%
  mutate(ULTIMATE_CLAIM_COUNT = as.factor(ULTIMATE_CLAIM_COUNT)) %>%
  as.h2o()
ui_done("Test data read in!")

#### Get SHAP Values for Severity ----

ui_info("Loading best Severity Model for {data} data.")
sev_mod <- h2o.loadModel(str_c(output_loc, data, "_best_severity_mod"))

ui_info("Getting Severity SHAP values for {data} data...")
shap <- h2o.predict_contributions(sev_mod, test) %>%
  as.data.frame()

ui_info("Writing Severity SHAP values for {data} data...")
fwrite(shap, str_c(output_loc, data, "_severity_shap.csv"))

ui_done("Severity Shap Values computed and saved for {data} test data frame!")

#### Get SHAP Values for Frequency ----

ui_info("Loading best Frequency Model for {data} data.")
freq_mod <- h2o.loadModel(str_c(output_loc, data, "_best_frequency_mod"))

ui_info("Getting Frequency SHAP values for {data} data...")
shap <- h2o.predict_contributions(freq_mod, test) %>%
  as.data.frame()

ui_info("Writing Frequency SHAP values for {data} data...")
fwrite(shap, str_c(output_loc, data, "_severity_shap.csv"))

ui_done("Frequency Shap Values computed and saved for {data} test data frame!")

#### Clean Up ----
h2o.shutdown(prompt = FALSE)
q(save = "no")