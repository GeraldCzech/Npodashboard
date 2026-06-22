#' @title combine_main_questionnaires_with_supplements
#' @description Kombiniert Hauptfragebögen (qnr1, qnr2) mit Folgefragebögen (qnr4, qnr5)
#'              basierend auf fallnr und org. Fehlende Variablen aus qnr1/qnr2
#'              werden zu qnr4/qnr5 ergänzt.
#'
#' @param fragebogen Liste mit Fragebogendaten (enthält qnr1, qnr2, qnr4, qnr5)
#' @return Die Liste fragebogen mit zwei neuen Komponenten: FC_BO und RO
# To Do: Ergänze OF_ und Awarenessdaten aus qnr2 (für qnr4) und qnr1 (fürqnr5)!
combine_main_questionnaires_with_supplements <- function(fragebogen) {
  message("🔁 Kombiniere qnr1 + qnr4 → FC_BO, qnr2 + qnr5 → RO...")
  
  # Hilfsfunktion: merge und ergänze
  combine_pair <- function(main_df, supplement_df, name_main, name_supplement) {
    join_keys <- c("fallnr", "org")
    
    # Bestimme fehlende Felder im Supplement
    missing_vars <- setdiff(names(main_df), names(supplement_df))
    missing_vars <- setdiff(missing_vars, join_keys)
    
    # Wähle diese aus main_df aus
    zusatzdaten <- main_df %>%
      dplyr::select(all_of(c(join_keys, missing_vars)))
    
    # Ergänze supplement_df mit Zusatzdaten
    supplement_extended <- supplement_df %>%
      dplyr::left_join(zusatzdaten, by = join_keys)
    
    # Jetzt zusammenfügen (ergänzte + Original)
    result <- dplyr::bind_rows(main_df, supplement_extended)
    return(result)
  }
  
  # Kombiniere qnr1 + qnr4
  fragebogen$FC_BO <- combine_pair(fragebogen$qnr1, fragebogen$qnr4, "qnr1", "qnr4")
  
  # Kombiniere qnr2 + qnr5
  fragebogen$RO <- combine_pair(fragebogen$qnr2, fragebogen$qnr5, "qnr2", "qnr5")
  
  message(glue::glue("✅ Kombinierte Fragebögen erstellt: FC_BO = {nrow(fragebogen$FC_BO)}, RO = {nrow(fragebogen$RO)}"))
  
  return(fragebogen)
}
