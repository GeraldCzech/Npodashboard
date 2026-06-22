#' @title recode_reversed_items
#' @description Berechnet gespiegelte Items basierend auf Variablennamen mit `_rev` am Ende.
#' @param df Dataframe mit Items
#' @param scale_min Minimaler Skalenwert (Default: 1)
#' @param scale_max Maximaler Skalenwert (Default: 7)
#' @return Dataframe mit neu berechneten reversed Items (z. B. FC02_10_rev)
recode_reversed_items <- function(df, scale_min = 1, scale_max = 7) {
  rev_items <- grep("_rev$", names(df), value = TRUE)
  
  for (rev_name in rev_items) {
    orig_name <- gsub("_rev$", "", rev_name)
    
    if (orig_name %in% names(df)) {
      df[[rev_name]] <- scale_max + scale_min - df[[orig_name]]
    } else {
      warning(glue::glue("⚠️ Kein Originalitem gefunden für {rev_name} (erwartet: {orig_name})"))
    }
  }
  return(df)
}

#' @title reverse_specific_items
#' @description Revertiert gezielt angegebene Items und erstellt neue Variablen mit `_rev`.
#' @param df Ein Dataframe mit Items
#' @param items Character-Vektor der zu spiegelnden Variablen (z. B. c("FC02_10", "FC02_12"))
#' @param scale_min Minimalwert der Skala (Default: 1)
#' @param scale_max Maximalwert der Skala (Default: 7)
#' @return Dataframe mit neuen Variablen wie FC02_10_rev
reverse_specific_items <- function(df, items, scale_min = 1, scale_max = 7) {
  for (item in items) {
    if (item %in% names(df)) {
      df[[paste0(item, "_rev")]] <- scale_max + scale_min - df[[item]]
    } else {
      warning(glue::glue("⚠️ Item {item} nicht im Dataframe enthalten."))
    }
  }
  return(df)
}
drop_avector <- function(df, strict = TRUE) {
  # df: data.frame / tibble
  # strict = TRUE -> stoppt, wenn nach der Bereinigung noch irgendwo avector steckt
  
  df <- as.data.frame(df)
  
  for (nm in names(df)) {
    if (inherits(df[[nm]], "avector")) {
      class(df[[nm]]) <- setdiff(class(df[[nm]]), "avector")
    }
  }
  
  if (strict) {
    still <- names(df)[sapply(df, inherits, what = "avector")]
    if (length(still) > 0) {
      stop(
        "drop_avector(): 'avector' konnte nicht vollständig entfernt werden in: ",
        paste(still, collapse = ", ")
      )
    }
  }
  
  df
}

