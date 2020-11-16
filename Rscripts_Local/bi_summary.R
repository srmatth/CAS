library(sparklyr)
library(dplyr)
library(tictoc)
library(BBmisc)
conf <- list()
conf$`sparklyr.cores.local` <- 4
conf$`sparklyr.shell.driver-memory` <- "4G"
conf$spark.memory.fraction <- 0.9
sc <- spark_connect(master = "local", version = "2.4.5", config = conf)

colnames <- c(
  "EARNED_EXPOSURE",
  "ULTIMATE_AMOUNT",
  "ULTIMATE_CLAIM_COUNT",
  paste("VAR", 1:46, sep = "_")
)
bi_1 <- spark_read_csv(sc = sc, name = "BI_1", path = "/Users/spencer.matthews/Documents/Research/Hartman/Data/CAS_RAW_1_1.txt", delimiter = "|", columns = colnames)
bi_2 <- spark_read_csv(sc = sc, name = "BI_2", path = "/Users/spencer.matthews/Documents/Research/Hartman/Data/CAS_RAW_1_2.txt", delimiter = "|", columns = colnames)
bi_3 <- spark_read_csv(sc = sc, name = "BI_3", path = "/Users/spencer.matthews/Documents/Research/Hartman/Data/CAS_RAW_1_3.txt", delimiter = "|", columns = colnames)
bi_4 <- spark_read_csv(sc = sc, name = "BI_4", path = "/Users/spencer.matthews/Documents/Research/Hartman/Data/CAS_RAW_1_4.txt", delimiter = "|", columns = colnames)
bi_5 <- spark_read_csv(sc = sc, name = "BI_5", path = "/Users/spencer.matthews/Documents/Research/Hartman/Data/CAS_RAW_1_5.txt", delimiter = "|", columns = colnames)
bi_6 <- spark_read_csv(sc = sc, name = "BI_6", path = "/Users/spencer.matthews/Documents/Research/Hartman/Data/CAS_RAW_1_6.txt", delimiter = "|", columns = colnames)
bi_7 <- spark_read_csv(sc = sc, name = "BI_7", path = "/Users/spencer.matthews/Documents/Research/Hartman/Data/CAS_RAW_1_7.txt", delimiter = "|", columns = colnames)
bi_8 <- spark_read_csv(sc = sc, name = "BI_8", path = "/Users/spencer.matthews/Documents/Research/Hartman/Data/CAS_RAW_1_8.txt", delimiter = "|", columns = colnames)

all_bi <- bi_1 %>%
  rbind(bi_2) %>%
  rbind(bi_3) %>%
  rbind(bi_4) %>%
  rbind(bi_5) %>%
  rbind(bi_6) %>%
  rbind(bi_7) %>%
  rbind(bi_8) %>%
  filter(EARNED_EXPOSURE > 0) %>%
  mutate(ULTIMATE_CLAIM_COUNT = round(ULTIMATE_CLAIM_COUNT))

beepr::beep(9)

# write a gather function since pivot_wider does not work in 
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

var_sum <- all_bi %>%
  dplyr::mutate(
    EARNED_EXPOSURE = as.numeric(EARNED_EXPOSURE),
    ULTIMATE_AMOUNT = as.numeric(ULTIMATE_AMOUNT)
  ) %>%
  sdf_gather("variable", "level", paste("VAR", 1:46, sep = "_")) %>%
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

readr::write_csv(
  var_sum,
  "/Users/spencer.matthews/Documents/Research/Hartman/Results/summaries_bi_more_robust.csv"
)
