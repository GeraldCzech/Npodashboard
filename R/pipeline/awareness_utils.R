# ─────────────────────────────────────────────
# Funktionen zur Berechnung von Awareness-Indikatoren
# ─────────────────────────────────────────────

#' @title get_tom
#' @description Prüft, ob die erste Nennung (BA01_01) der Zielorganisation entspricht
#' @param nennung_code Zeichenkette (gematcht, z. B. "05")
#' @param org_code Zeichenkette der Zielorganisation (z. B. "05")
#' @return TRUE, FALSE oder NA
get_tom <- function(nennung_code, org_code) {
  if (is.na(nennung_code) || is.na(org_code)) return(NA)
  return(nennung_code == org_code)
}

#' @title get_saw
#' @description Prüft, ob eine der drei spontanen Nennungen zur Zielorganisation passt
#' @param n1,n2,n3 drei gematchte Codes aus BA01_01–03
#' @param org_code Zeichenkette (z. B. "03")
#' @return TRUE, FALSE oder NA
get_saw <- function(n1, n2, n3, org_code) {
  if (all(is.na(c(n1, n2, n3))) || is.na(org_code)) return(NA)
  return(org_code %in% c(n1, n2, n3))
}

#' @title get_value_or_na
#' @description Holt numerischen Wert aus einer benannten Spalte in einer gegebenen Datenzeile
#' @param data data.frame (z. B. Startfragebogen)
#' @param i Zeilenindex
#' @param varname Spaltenname als Zeichenkette
#' @return numerischer Wert oder NA
get_value_or_na <- function(data, i, varname) {
  if (!varname %in% names(data)) return(NA_real_)
  val <- suppressWarnings(as.numeric(data[[varname]][i]))
  return(ifelse(is.na(val), NA_real_, val))
}
# scripts/modules/add_awareness.R

add_awareness <- function(validated, org_synonyme) {
  start01_data <- validated[validated$QUESTNNR == "Start01", ]
  
  if (nrow(start01_data) == 0) {
    warning("⚠️ Keine Daten für QUESTNNR = 'Start01' gefunden – Awareness wird übersprungen.")
    return(validated)
  }
  
  start_awareness <- extract_start_awareness(start01_data, org_synonyme)
  
  # Prüfen, ob etwas zurückgegeben wurde
  if (is.null(start_awareness) || nrow(start_awareness) == 0) {
    warning("⚠️ Awareness-Extraktion liefert keine Daten – Merge übersprungen.")
    return(validated)
  }
  
  validated <- validated %>%
    dplyr::left_join(
      start_awareness %>%
        dplyr::select(
          CASE,
          Spontane_Awareness,
          Org1_TOM, Org2_TOM,
          Org1_SAW, Org2_SAW,
          Org1_BA_A, Org2_BA_A,
          Org1_BA_T, Org2_BA_T
        ),
      by = "CASE"
    )
  
  return(validated)
}
lookup_org <- function(ref_id, org_synonyme) {
  ref_num <- as.numeric(ref_id)
  
  org_synonyme <- org_synonyme %>%
    dplyr::mutate(org_id_num = as.numeric(org_id))
  
  match <- org_synonyme %>%
    dplyr::filter(org_id_num == ref_num) %>%
    dplyr::pull(org_name)
  
  if (length(match) == 1) {
    return(match)
  } else {
    return(NA_character_)
  }
}
library(dplyr)
library(gt)

create_at03rv4_summary_table <- function(start01_df) {
  start01_df %>%
    filter(!is.na(AT03_RV4), AT03_RV4 != "") %>%
    count(AT03_RV4, name = "Anzahl") %>%
    arrange(desc(Anzahl)) %>%
    gt() %>%
    tab_header(
      title = "Verteilung der externen Datenquellen",
      subtitle = "Nur Fälle mit REF == 0"
    ) %>%
    cols_label(
      AT03_RV4 = "Quelle (AT03_RV4)",
      Anzahl = "Fallzahl"
    ) %>%
    fmt_number(columns = Anzahl, sep_mark = ".", decimals = 0)
}
# Erzeugt dreistufige Awareness:
# 2 = TOM==1, 1 = SAW==1 (aber TOM!=1), 0 = sonst
make_BA_S_vec <- function(TOM, SAW, ordered = TRUE) {
  # robust auf 0/1
  to01 <- function(x) {
    if (is.logical(x)) return(as.integer(x))
    if (inherits(x, c("haven_labelled", "labelled", "avector", "vctrs_vctr"))) {
      x <- unclass(x); attributes(x) <- NULL
    }
    if (is.factor(x)) x <- as.character(x)
    if (is.character(x)) {
      x <- trimws(tolower(x))
      x <- ifelse(x %in% c("1","true","t","yes","ja"), 1L,
                  ifelse(x %in% c("0","false","f","no","nein"), 0L, NA_integer_))
      return(as.integer(x))
    }
    suppressWarnings(as.integer(x))
  }
  
  tom01 <- to01(TOM)
  saw01 <- to01(SAW)
  
  bs <- ifelse(!is.na(tom01) & tom01 == 1L, 2L,
               ifelse(!is.na(saw01) & saw01 == 1L, 1L, 0L))
  
  if (ordered) {
    return(ordered(bs, levels = c(0L, 1L, 2L)))
  } else {
    return(bs)
  }
}