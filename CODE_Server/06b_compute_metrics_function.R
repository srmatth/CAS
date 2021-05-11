compute_metrics <- function(sev_file, freq_file, test) {
  logger::log_info("Reading in Severity Predictions: {sev_file}")
  severity_preds <- data.table::fread(sev_file) %>%
    dplyr::mutate(file = sev_file)
  if (stringr::str_detect(sev_file, "log")) {
    severity_preds <- severity_preds %>%
      dplyr::mutate(predict = exp(predict))
  }
  
  logger::log_info("Reading in Frequency Predictions: {freq_file}")
  frequency_preds <- data.table::fread(freq_file) %>%
    dplyr::mutate(file = freq_file)
  
  logger::log_info("Getting all possible combinations of models")
  mod_list <- list(
    sev_mod = severity_preds$mod_num %>% unique(), 
    freq_mod = frequency_preds$mod_num %>% unique()
  ) %>%
    expand.grid(stringsAsFactors = FALSE)
  
  logger::log_info("Setting up parallel plan")
  n_cores <- 6 # availableCores() - 1
  future::plan(future::multicore, workers = n_cores)
  
  logger::log_info("Beginning parallel map computation of model metrics")
  d <- furrr::future_map2_dfr(
    .x = mod_list$sev_mod,
    .y = mod_list$freq_mod,
    .f = ~{
      tryCatch({
        sev_mod_preds <- severity_preds %>%
          dplyr::filter(mod_num == .x)
        if (nrow(sev_mod_preds) > nrow(test)) sev_mod_preds <- sev_mod_preds[1,nrow(test),]
        freq_mod_preds <- frequency_preds %>%
          dplyr::filter(mod_num == .y)
        if (nrow(freq_mod_preds) > nrow(test)) freq_mod_preds <- freq_mod_preds[1,nrow(test),]
        
        combined_preds <- freq_mod_preds %>%
          as.data.frame() %>%
          dplyr::select(-predict) %>%
          dplyr::left_join(sev_mod_preds, by = "row_num") %>%
          dplyr::mutate(
            amount_1_claim = predict,
            amount_2_claim = 2 * predict,
            amount_3_claim = 3 * predict,
            expected_ultimate_amount = amount_1_claim * p1 +
              amount_2_claim * p2 +
              amount_3_claim * p3,
            actual_ultimate_amount = test$ULTIMATE_AMOUNT
          )
        logger::log_info("Predictions calculated and combined for sev model {.x} and freq model {.y}")
        
        data.frame(
          sev_mod_file = sev_file,
          sev_mod_num = .x,
          freq_mod_file = freq_file,
          freq_mod_num = .y,
          mae = mean(abs(combined_preds$actual_ultimate_amount - combined_preds$expected_ultimate_amount)),
          mse = mean((combined_preds$actual_ultimate_amount - combined_preds$expected_ultimate_amount)^2),
          stringsAsFactors = FALSE
        )
      },
      error = function(e) {
        logger::log_error("Predictions FAILED for sev model {.x} and freq model {.y}: {e}")
        return(NULL)
      })
    }
  )
  plan(future::sequential)
  return(d)
}