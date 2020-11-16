# Convert from raw to clean data

# Please note that this script needs at least 10G of RAM to run

#### Setup and initialization ----

# Load necessary libraries
library(dplyr)
library(purrr)
library(stringr)
library(usethis)
library(data.table)
library(fs)

# Set the directory where the raw data is stored and initialize the column names
data_dir <- "~/CAS/data/Data"
output_dir <- "data"
colnames <- c(
  "EARNED_EXPOSURE",
  "ULTIMATE_AMOUNT",
  "ULTIMATE_CLAIM_COUNT",
  str_c("X_VAR", 1:46)
)

#### BI Data ----

ui_info("Working on the BI data")
bi <- map_dfr(
  .x = dir_ls(data_dir) %>%
    str_subset("CAS_RAW_1"),
  .f = ~{
    ui_info("Reading file: {.x}")
    fread(.x, col.names = colnames, stringsAsFactors = FALSE) %>%
      mutate(form = str_remove(.x, "CAS_RAW_"))
  }
) %>%
  filter(EARNED_EXPOSURE > 0) %>%
  mutate(ULTIMATE_CLAIM_COUNT = round(ULTIMATE_CLAIM_COUNT))
output_file <- str_c(output_dir, "/bi.csv")
ui_info("Writing clean BI data to file {output_file}")
fwrite(bi, output_file)
rm(bi)
ui_done("Finished Writing {output_file}!")

#### PD Data ----

ui_info("Working on the PD data")
pd <- map_dfr(
  .x = dir_ls(data_dir) %>%
    str_subset("CAS_RAW_2"),
  .f = ~{
    ui_info("Reading file: {.x}")
    fread(.x, col.names = colnames, stringsAsFactors = FALSE) %>%
      mutate(form = str_remove(.x, "CAS_RAW_"))
  }
) %>%
  filter(EARNED_EXPOSURE > 0) %>%
  mutate(ULTIMATE_CLAIM_COUNT = round(ULTIMATE_CLAIM_COUNT))
output_file <- str_c(output_dir, "/pd.csv")
ui_info("Writing clean PD data to file {output_file}")
fwrite(pd, output_file)
rm(pd)
ui_done("Finished Writing {output_file}!")

#### COLL Data ----

ui_info("Working on the COLL data")
coll <- map_dfr(
  .x = dir_ls(data_dir) %>%
    str_subset("CAS_RAW_3"),
  .f = ~{
    ui_info("Reading file: {.x}")
    fread(.x, col.names = colnames, stringsAsFactors = FALSE) %>%
      mutate(form = str_remove(.x, "CAS_RAW_"))
  }
) %>%
  filter(EARNED_EXPOSURE > 0) %>%
  mutate(ULTIMATE_CLAIM_COUNT = round(ULTIMATE_CLAIM_COUNT))
output_file <- str_c(output_dir, "/coll.csv")
ui_info("Writing clean COLL data to file {output_file}")
fwrite(coll, output_file)
rm(coll)
ui_done("Finished Writing {output_file}!")

#### Clean-Up ----

# Quit R on the server
q(save = "no")
