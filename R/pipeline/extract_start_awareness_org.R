extract_start_awareness <- function(data, org_synonyme) {
  stopifnot("CASE" %in% names(data))
  
  # 🔍 Robustere Fuzzy-Funktion – findet Teilstrings
  fuzzy_match <- function(eingabe) {
    eingabe <- tolower(trimws(eingabe))
    for (i in seq_len(nrow(org_synonyme))) {
      syns <- tolower(org_synonyme$extended_synonyms[[i]])
      if (any(sapply(syns, function(s) grepl(s, eingabe, fixed = FALSE)))) {
        return(org_synonyme$org_id[i])
      }
    }
    return(NA_character_)
  }
  
  # 🎯 Zielorgs
  org1 <- sprintf("%02d", as.numeric(data$BA03_01))
  org2 <- sprintf("%02d", as.numeric(data$BA03_02))
  
  # 📝 Spontane Felder und Matches
  ba01_1 <- data$BA01_01
  ba01_2 <- data$BA01_02
  ba01_3 <- data$BA01_03
  
  n1 <- vapply(ba01_1, fuzzy_match, character(1))
  n2 <- vapply(ba01_2, fuzzy_match, character(1))
  n3 <- vapply(ba01_3, fuzzy_match, character(1))
  
  # 📋 Awareness-Liste
  spontane_awareness <- Map(function(a, b, c) na.omit(c(a, b, c)), n1, n2, n3)
  
  # 🧮 Statistik und Ausgabe
  alle_texte <- c(ba01_1, ba01_2, ba01_3)
  matched    <- c(n1, n2, n3)
  unmatched  <- unique(tolower(trimws(alle_texte[is.na(matched)])))
  matched_pct <- round(100 * sum(!is.na(matched)) / length(matched), 1)
  
  message(glue::glue("✅ Zuordnungsrate: {sum(!is.na(matched))} von {length(matched)} Freitexten ({matched_pct}%)"))
  message("🧐 Nicht zuordenbare Begriffe (einmalig):")
  print(unmatched)
  
  # 🧠 Awareness-Spalten berechnen
  get_tom <- function(match_code, org_code) {
    if (is.na(match_code)) return(FALSE)
    return(match_code == org_code)
  }
  
  get_saw <- function(n_list, org_code) {
    if (length(n_list) == 0 || is.na(org_code)) return(FALSE)
    return(org_code %in% n_list)
  }
  
  get_value_or_na <- function(i, varname) {
    if (varname %in% names(data)) {
      val <- data[[varname]][i]
      return(as.numeric(val))
    } else {
      return(NA_real_)
    }
  }
  
  out <- data.frame(
    CASE = data$CASE,
    Spontane_Awareness = I(spontane_awareness),
    Org1_TOM  = vapply(seq_along(n1), function(i) get_tom(n1[i], org1[i]), logical(1)),
    Org2_TOM  = vapply(seq_along(n1), function(i) get_tom(n1[i], org2[i]), logical(1)),
    Org1_SAW  = vapply(seq_along(n1), function(i) get_saw(spontane_awareness[[i]], org1[i]), logical(1)),
    Org2_SAW  = vapply(seq_along(n1), function(i) get_saw(spontane_awareness[[i]], org2[i]), logical(1)),
    Org1_BA_A = vapply(seq_along(n1), function(i) get_value_or_na(i, paste0("BA02_", org1[i])), numeric(1)),
    Org2_BA_A = vapply(seq_along(n1), function(i) get_value_or_na(i, paste0("BA02_", org2[i])), numeric(1)),
    Org1_BA_T = vapply(seq_along(n1), function(i) get_value_or_na(i, paste0("BA04_", org1[i])), numeric(1)),
    Org2_BA_T = vapply(seq_along(n1), function(i) get_value_or_na(i, paste0("BA04_", org2[i])), numeric(1))
  )
  
  return(out)
}
#' @title get_start_awareness_data
#' @description Holt Awareness-Daten aus Start01 für Organisation 1 oder 2
#' @param start_df Dataframe mit Start01-Daten
#' @param case_ref Referenz-ID (z. B. qnr1$REF)
#' @param org_num Nummer der Organisation (1 oder 2)
#' @return Dataframe mit einer Zeile und CASE

get_start_awareness_data <- function(start_df, case_ref, org_num) {
  # CASE-Wert sicherheitshalber in character konvertieren
  case_ref <- as.character(case_ref)
  
  # Eine Zeile extrahieren
  basis_daten <- start_df %>%
    dplyr::filter(as.character(CASE) == case_ref) %>%
    dplyr::slice(1)
  
  if (nrow(basis_daten) == 0) {
    return(tibble(CASE = case_ref))  # Rückgabe einer leeren Zeile mit CASE
  }
  
  vars <- c(
    "CASE",
    paste0("TOM_Org", org_num),
    paste0("SAW_Org", org_num),
    paste0("BA_A_Org", org_num),
    paste0("BA_F_Org", org_num)
  )
  out <- basis_daten %>% dplyr::select(any_of(vars))
  out$CASE <- as.character(out$CASE)  # Sicherstellen, dass CASE immer character ist
  return(out)
  
}
