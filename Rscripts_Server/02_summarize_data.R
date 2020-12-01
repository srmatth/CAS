# Get summaries by variable and level for a selected data set

#### User Inputs ----

# 1. Choose the data set that you wish to use, "bi", "pd", or "coll"
data <- "bi"

# 2. set the relative directory of the data and the output (with forward slash at the end)
data_loc <- "data/"
output_loc <- NULL

# 3. set the configuration for the spark cluster
conf <- list()
conf$`sparklyr.cores.local` <- 4
conf$`sparklyr.shell.driver-memory` <- "4G"
conf$spark.memory.fraction <- 0.9

#### Setup ----

usethis::ui_info("Loading Libraries...")

# Load libraries
library(dplyr)
library(stringr)
library(data.table)
library(tidyr)
library(ggplot2)
library(usethis)
library(sparklyr)
library(tictoc)
library(BBmisc)

ui_done("Finished Loading Libraries.")

ui_info("Starting Spark")

sc <- spark_connect(master = "local", version = "2.4.5", config = conf)

ui_info("Initializing values and reading in the data...")

df <- fread(str_c(data_loc, data, ".csv"))

ui_done("Data in!")

#### Factor Summarizing ----

# write a gather function since pivot_wider does not work in Spark
sdf_gather <- function(data, key = "key", value = "value", ...) {
  cols <- list(...) %>% unlist()
  
  # Explode with map (same as stack) requires multiple aliases so
  # dplyr mutate won't work for us here.
  expr <- list(paste(
    "explode(map(",
    paste("'", cols, "',`",  cols, "`", sep = "", collapse = ","),
    ")) as (", key, ",", value, ")", sep = ""))
  
  keys <- data %>% colnames() %>% setdiff(cols) %>% as.list()
  
  data %>%
    spark_dataframe() %>% 
    sparklyr::invoke("selectExpr", c(keys, expr)) %>% 
    sdf_register()
}

ui_info("Transferring Data to Spark Cluster")
df_s <- spark_read_csv(sc = sc, name = "df_s", path = str_c(data_loc, data, ".csv"))

ui_info("Beginning to summarize Factor Variables...")
fac_sum <- df_s %>%
  dplyr::mutate(
    EARNED_EXPOSURE = as.numeric(EARNED_EXPOSURE),
    ULTIMATE_AMOUNT = as.numeric(ULTIMATE_AMOUNT),
    ULTIMATE_CLAIM_COUNT = as.numeric(ULTIMATE_CLAIM_COUNT)
  ) %>%
  mutate_at(str_c("X_VAR", 1:46), as.character) %>%
  sdf_gather("variable", "level", str_c("X_VAR", 1:46)) %>%
  group_by(variable, level) %>%
  summarize(
    count = dplyr::n(),
    min_earned_exposure = min(EARNED_EXPOSURE),
    q1_earned_exposure = percentile_approx(EARNED_EXPOSURE, .25),
    med_earned_exposure = percentile_approx(EARNED_EXPOSURE, .5),
    avg_earned_exposure = mean(EARNED_EXPOSURE),
    q3_earned_exposure = percentile_approx(EARNED_EXPOSURE, .75),
    max_earned_exposure = max(EARNED_EXPOSURE),
    tot_earned_exposure = sum(EARNED_EXPOSURE),
    min_ultimate_amount = min(ULTIMATE_AMOUNT),
    q1_ultimate_amount = percentile_approx(ULTIMATE_AMOUNT, .25),
    med_ultimate_amount = percentile_approx(ULTIMATE_AMOUNT, .5),
    avg_ultimate_amount = mean(ULTIMATE_AMOUNT),
    q3_ultimate_amount = percentile_approx(ULTIMATE_AMOUNT, .75),
    max_ultimate_amount = max(ULTIMATE_AMOUNT),
    tot_ultimate_amount = sum(ULTIMATE_AMOUNT),
    min_ultimate_count = min(ULTIMATE_CLAIM_COUNT),
    q1_ultimate_count = percentile_approx(ULTIMATE_CLAIM_COUNT, .25),
    med_ultimate_count = percentile_approx(ULTIMATE_CLAIM_COUNT, .5),
    avg_ultimate_count = mean(ULTIMATE_CLAIM_COUNT),
    q3_ultimate_count = percentile_approx(ULTIMATE_CLAIM_COUNT, .75),
    max_ultimate_count = max(ULTIMATE_CLAIM_COUNT),
    tot_ultimate_count = sum(ULTIMATE_CLAIM_COUNT)
  ) %>%
  ungroup() %>%
  arrange(variable, level) %>%
  collect()

# Save the summary
ui_info("Saving factor summaries...")
fwrite(fac_sum, str_c(output_loc, data, "_fac_sum.csv"))
ui_done("Finished factor summaries!")

#### Numerical Summarizing ----

ui_info("computing summary statistics for numeric variables...")
num_sum <- summary(df$EARNED_EXPOSURE) %>%
  rbind(
    summary(df$ULTIMATE_CLAIM_COUNT)
  ) %>%
  rbind(
    summary(df$ULTIMATE_AMOUNT)
  ) %>% 
  magrittr::set_rownames(1:nrow(.)) %>%
  as_tibble() %>%
  mutate(variable = c("EARNED_EXPOSURE", "ULTIMATE_CLAIM_COUNT", "ULTIMATE_AMOUNT")) %>%
  select(variable, everything())

# save the summary
ui_info("saving numeric variable summaries...")
fwrite(num_sum, str_c(output_loc, data, "_num_sum.csv"))
ui_done("Done with the numeric summaries and data saved!")

#### Rows with Claims ----

ui_info("Beginning summaries for filtered data...")

# filter the data and create the new column of severity
raw_data <- df %>%
  filter(ULTIMATE_CLAIM_COUNT > 0) %>%
  mutate(severity = ULTIMATE_AMOUNT / ULTIMATE_CLAIM_COUNT)

ui_info("Beginning Violin Plots")
# open the pdf for editing
pdf(str_c(data, "_severity_violin_plots.pdf"))

# note that variables 19, 34, and 46 have too many levels to plot
for (i in stringr::str_c("X_VAR", c(1:18, 20:33, 35:45))) {
  summary <- count(raw_data, !!rlang::sym(i))
  
  p <- ggplot(raw_data) +
    geom_violin(aes(x = as.factor(!!rlang::sym(i)), y = severity)) +
    xlab(i) +
    ylab("Severity") +
    theme_classic() 
  if (nrow(summary) < 13) {
    p <- p +
      ggtitle(stringr::str_c("Severity vs. ",i), "(Counts for Each Level in Red)") +
      geom_text(
        data = summary, 
        aes(x = as.factor(!!rlang::sym(i)), 
            y = 8e05,
            label = n),
        colour = "red"
      )
  } else {
    p <- p +
      ggtitle(stringr::str_c("Severity vs. ",i))
  }
  
  print(p)
}

dev.off()
ui_done("Vioilin Plots done and images saved!")

#### Quit R and Spark ----

spark_disconnect(sc)
q(save = "no")
