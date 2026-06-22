#' @title extract_start_awareness
#' @description Extrahiert Awareness-Daten aus Startfragebogen
#' @param data Dataframe mit QUESTNNR == "Start01"
#' @param org_synonyme Synonymliste mit org_id und extended_synonyms (Liste)
#' @return Dataframe mit Spontane_Awareness und Awareness-Spalten pro Zielorg

extract_start_awareness <- function(data, org_synonyme) {
  stopifnot("CASE" %in% names(data))
  
  # Matchfunktion lokal definieren
  match_org_code <- function(eingabe) {
    eingabe <- tolower(trimws(eingabe))
    for (i in seq_len(nrow(org_synonyme))) {
      if (eingabe %in% org_synonyme$extended_synonyms[[i]]) {
        return(org_synonyme$org_id[i])
      }
    }
    return(NA_character_)
  }
  
  # Fuzzy-Matching der drei BA01-Nennungen
  n1 <- vapply(data$BA01_01, match_org_code, character(1))
  n2 <- vapply(data$BA01_02, match_org_code, character(1))
  n3 <- vapply(data$BA01_03, match_org_code, character(1))
  
  # Spontane Awareness als Liste (für Analyse)
  spontane_awareness <- mapply(function(a, b, c) {
    na.omit(c(a, b, c))
  }, n1, n2, n3, SIMPLIFY = FALSE)
  
  # Zielorgs
  org1 <- sprintf("%02d", as.numeric(data$BA03_01))
  org2 <- sprintf("%02d", as.numeric(data$BA03_02))
  
  # TOM/SAW-Logik
  Org1_TOM <- n1 == org1
  Org2_TOM <- n1 == org2
  Org1_SAW <- mapply(function(a, b, c, target) target %in% c(a, b, c), n1, n2, n3, org1)
  Org2_SAW <- mapply(function(a, b, c, target) target %in% c(a, b, c), n1, n2, n3, org2)
  
  # BA_A = gestützte Bekanntheit (BA02_XX), BA_T = Vertrautheit (BA04_XX)
  Org1_BA_A <- vapply(seq_len(nrow(data)), function(i) {
    get_value_or_na(data, i, paste0("BA02_", org1[i]))
  }, numeric(1))
  
  Org2_BA_A <- vapply(seq_len(nrow(data)), function(i) {
    get_value_or_na(data, i, paste0("BA02_", org2[i]))
  }, numeric(1))
  
  Org1_BA_T <- vapply(seq_len(nrow(data)), function(i) {
    get_value_or_na(data, i, paste0("BA04_", org1[i]))
  }, numeric(1))
  
  Org2_BA_T <- vapply(seq_len(nrow(data)), function(i) {
    get_value_or_na(data, i, paste0("BA04_", org2[i]))
  }, numeric(1))
  
  # Zusammenführen
  out <- data %>%
    dplyr::mutate(
      Spontane_Awareness = spontane_awareness,
      Org1_TOM = Org1_TOM,
      Org2_TOM = Org2_TOM,
      Org1_SAW = Org1_SAW,
      Org2_SAW = Org2_SAW,
      Org1_BA_A = Org1_BA_A,
      Org2_BA_A = Org2_BA_A,
      Org1_BA_T = Org1_BA_T,
      Org2_BA_T = Org2_BA_T
    )
  
  return(out)
}
