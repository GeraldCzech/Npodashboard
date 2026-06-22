#' @title split_validated_data
#' @description Teilt den validierten Datensatz nach Fragebogentypen anhand QUESTNNR
#' @param data Dataframe (z. B. clean oder validated), muss QUESTNNR-Spalte enthalten
#' @param fields Eine Liste benannter Feldvektoren, z. B. fields$start01
#' @return Liste von Dataframes (ein Element je Fragebogentyp)

split_validated_data <- function(data, fields) {
  stopifnot("QUESTNNR" %in% names(data))
  
  # Kleinbuchstaben für robuste Vergleiche
  data <- data %>% dplyr::mutate(QUESTNNR = tolower(QUESTNNR))
  
  fragebogen <- list()
  
  for (typ in names(fields)) {
    fragebogen[[typ]] <- data %>%
      dplyr::filter(QUESTNNR == typ) %>%
      dplyr::select(dplyr::any_of(fields[[typ]]))
    
    n_typ <- nrow(fragebogen[[typ]])
    if (n_typ == 0) {
      warning(glue::glue("⚠️ Keine Fälle für '{typ}' gefunden."))
    } else {
      message(glue::glue("📄 '{typ}': {n_typ} Fälle extrahiert."))
    }
  }
  
  # Hilfsfunktion: Einheitliche Typisierung aller gemeinsamen Felder
  normalize_columns <- function(df, var_types) {
    for (v in names(var_types)) {
      if (v %in% names(df)) {
        df[[v]] <- var_types[[v]](df[[v]])
      }
    }
    return(df)
  }
  
  # Typkonventionen für alle Fragebögen
  var_types <- list(
    fallnr = as.numeric,
    org = as.numeric,
    org_1 = as.numeric,
    org_2 = as.numeric,
    CASE = as.numeric,
    REF = as.numeric,
    STARTED = as.character,
    FINISHED = as.character
  )
  
  # Ergänze numerische Indizes für Start01
  if ("start01" %in% names(fragebogen)) {
    fragebogen$start01 <- fragebogen$start01 %>%
      dplyr::mutate(
        fallnr = as.numeric(as.character(CASE)),
        org_1 = as.numeric(as.character(BA03_01)),
        org_2 = as.numeric(as.character(BA03_02))
      )
    fragebogen$start01 <- normalize_columns(fragebogen$start01, var_types)
  }
  
  # Für alle anderen Fragebögen
  for (typ in c("qnr1", "qnr2", "qnr4", "qnr5")) {
    if (typ %in% names(fragebogen)) {
      fragebogen[[typ]] <- fragebogen[[typ]] %>%
        dplyr::mutate(
          fallnr = as.numeric(as.character(REF)),
          org = as.numeric(as.character(BA03_01))
        )
      fragebogen[[typ]] <- normalize_columns(fragebogen[[typ]], var_types)
    }
  }
  
  return(fragebogen)
}
