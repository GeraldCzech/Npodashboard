combine_posterior <- function(fragebogen) {
  library(dplyr)
  
  # Alle IDs als numeric setzen
  fragebogen$start01$CASE <- as.numeric(fragebogen$start01$CASE)
  fragebogen$qnr1$fallnr <- as.numeric(fragebogen$qnr1$fallnr)
  fragebogen$qnr2$fallnr <- as.numeric(fragebogen$qnr2$fallnr)
  fragebogen$qnr4$fallnr <- as.numeric(fragebogen$qnr4$fallnr)
  fragebogen$qnr5$fallnr <- as.numeric(fragebogen$qnr5$fallnr)
  
  # Start01 prob_valid
  valid_start <- fragebogen$start01 %>%
    select(CASE, prob_valid) %>%
    rename(source = prob_valid) %>%
    mutate(source_type = "start01")
  
  # Andere Quellen
  valid_qnr1 <- fragebogen$qnr1 %>%
    select(fallnr, prob_valid) %>%
    rename(CASE = fallnr, source = prob_valid) %>%
    mutate(source_type = "qnr1")
  
  valid_qnr2 <- fragebogen$qnr2 %>%
    select(fallnr, prob_valid) %>%
    rename(CASE = fallnr, source = prob_valid) %>%
    mutate(source_type = "qnr2")
  
  valid_qnr4 <- fragebogen$qnr4 %>%
    select(fallnr, prob_valid) %>%
    rename(CASE = fallnr, source = prob_valid) %>%
    mutate(source_type = "qnr4")
  
  valid_qnr5 <- fragebogen$qnr5 %>%
    select(fallnr, prob_valid) %>%
    rename(CASE = fallnr, source = prob_valid) %>%
    mutate(source_type = "qnr5")
  
  # Kombinieren aller Quellen
  valid_all <- bind_rows(valid_start, valid_qnr1, valid_qnr2, valid_qnr4, valid_qnr5) %>%
    filter(!is.na(source))  # Nur gültige Wahrscheinlichkeiten
  
  # Bayessche Kombination
  posterior_combined <- valid_all %>%
    group_by(CASE) %>%
    summarise(
      n_sources = n(),
      product = prod(source),
      product_not = prod(1 - source),
      posterior = product / (product + product_not),
      .groups = "drop"
    )
  
  return(posterior_combined)
}
