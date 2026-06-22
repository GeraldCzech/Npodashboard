#' @title compare_column_types
#' @description Vergleicht die Spaltentypen zweier Dataframes
#' @param df1 Erstes Dataframe
#' @param df2 Zweites Dataframe
#' @return Dataframe mit Konflikten, falls vorhanden
compare_column_types <- function(df1, df2) {
  cols <- intersect(names(df1), names(df2))
  
  out <- tibble::tibble(
    column = cols,
    type_df1 = vapply(df1[cols], typeof, character(1)),
    type_df2 = vapply(df2[cols], typeof, character(1))
  ) %>%
    dplyr::mutate(identical = type_df1 == type_df2) %>%
    dplyr::filter(!identical)
  
  if (nrow(out) == 0) {
    message("✅ Keine Typkonflikte zwischen den Spalten.")
  }
  
  return(out)
}

#' @title harmonize_df
#' @description Harmonisiert zwei Dataframes so, dass sie zusammengefügt werden können (z. B. mit bind_rows).
#' @param df Das Dataframe, das angepasst werden soll.
#' @param reference Das Referenz-Dataframe, dessen Struktur übernommen werden soll.
#' @return Harmonisiertes Dataframe mit denselben Spaltennamen und -typen wie das Referenz-Dataframe.

harmonize_df <- function(df, reference) {
  # Ziel: Gemeinsame Spaltennamen aufbauen
  ref_names <- names(reference)
  
  # Fehlende Spalten im df auffüllen
  for (col in ref_names) {
    if (!col %in% names(df)) {
      df[[col]] <- NA
    }
  }
  
  # Überzählige Spalten entfernen (nicht in Referenz enthalten)
  df <- df[, ref_names, drop = FALSE]
  
  # Typkonvertierung mit Fallback
  for (col in ref_names) {
    ref_class <- class(reference[[col]])[1]
    current_class <- class(df[[col]])[1]
    
    if (ref_class != current_class) {
      df[[col]] <- tryCatch({
        if (ref_class == "character") {
          as.character(df[[col]])
        } else if (ref_class == "numeric") {
          suppressWarnings(as.numeric(as.character(df[[col]])))
        } else if (ref_class == "integer") {
          suppressWarnings(as.integer(as.character(df[[col]])))
        } else if (ref_class == "logical") {
          as.logical(df[[col]])
        } else {
          df[[col]]  # fallback ohne Konvertierung
        }
      }, error = function(e) {
        warning(glue::glue("⚠️ Typkonvertierung für Spalte '{col}' fehlgeschlagen – Fallback auf character."))
        as.character(df[[col]])
      })
    }
  }
  
  return(df)
}

#' @title join_followup_cross_questionnaires
#' @description Verknüpft qnr1 mit qnr5 und qnr2 mit qnr4 über `fallnr` und `org`.
#'              Nutzt nur Spalten aus dem ersten Fragebogen bei Konflikten.
#' @param fragebogen Liste von Dataframes aus `split_validated_data()`.
#' @return `fragebogen`-Liste mit neuem Element `$cross` (vereinte Haupt-Followup-Fragebögen).
join_followup_cross_questionnaires <- function(fragebogen) {
  message("🔗 Verknüpfe qnr1+qnr5 und qnr2+qnr4 über fallnr und org...")
  
  # 1. Inner Joins auf Fallnummer + Organisation
  joint_a <- dplyr::inner_join(
    fragebogen$qnr1,
    fragebogen$qnr5,
    by = c("fallnr", "org"),
    suffix = c("", ".drop")
  )
  
  joint_b <- dplyr::inner_join(
    fragebogen$qnr2,
    fragebogen$qnr4,
    by = c("fallnr", "org"),
    suffix = c("", ".drop")
  )
  
  # 2. Entferne doppelte Spalten aus dem zweiten Fragebogen (.drop)
  joint_a <- joint_a[, !grepl("\\.drop$", names(joint_a)), drop = FALSE]
  joint_b <- joint_b[, !grepl("\\.drop$", names(joint_b)), drop = FALSE]
  
  # 3. Typprüfung
  type_issues <- compare_column_types(joint_a, joint_b)
  
  if (nrow(type_issues) > 0) {
    stop("❌ Typkonflikte gefunden bei folgenden Spalten:\n",
         paste0(type_issues$column, collapse = ", "))
  }
  
  # 4. Einheitliche Spaltenreihenfolge
  all_cols <- union(names(joint_a), names(joint_b))
  joint_a <- joint_a[, all_cols, drop = FALSE]
  joint_b <- joint_b[, all_cols, drop = FALSE]
  
  # 5. Zusammenführen
  fragebogen$cross <- dplyr::bind_rows(joint_a, joint_b)
  
  message(glue::glue("✅ Cross-Joins abgeschlossen: {nrow(fragebogen$cross)} kombinierte Zeilen."))
  
  return(fragebogen)
}
