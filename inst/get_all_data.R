# A script to write all the Files to the data file, as .rds files

library(dplyr)

# location of all the files on my local computer
files <- fs::dir_ls("/Users/spencer.matthews/Documents/Research/Hartman/Data")

# colnames for each of the data tables
colnames <- c(
  "EARNED_EXPOSURE",
  "ULTIMATE_AMOUNT",
  "ULTIMATE_CLAIM_COUNT",
  stringr::str_c("X_VAR", 1:46)
)

# read in all these files and write them to the data folder as compressed .rds files
purrr::map(
  .x = files,
  .f = ~{
    file <- fs::path_file(.x) %>%
      stringr::str_remove("\\.txt")
    usethis::ui_info("Loading and writing {file}")
    tmp_data <- data.table::fread(
      .x, 
      stringsAsFactors = TRUE, 
      col.names = colnames
    )
    readr::write_rds(
      x = tmp_data,
      path = stringr::str_c("data/", file, ".rds"),
      compress = "gz"
    )
    rm(tmp_data)
  }
)

# test that we can read them back in
# test <- data.table::fread("data/CAS_RAW_1_1.rds")
# fread does not work

# what about readr
tictoc::tic()
test <- readr::read_rds("data/CAS_RAW_1_1.rds")
tictoc::toc() # 4.147 seconds

# Let's comapre that to fread on the original file
tictoc::tic()
test <- data.table::fread(
  files[1],
  stringsAsFactors = TRUE, 
  col.names = colnames
)
tictoc::toc() # 15.534 seconds
