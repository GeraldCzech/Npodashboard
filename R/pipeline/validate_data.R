#' Prüft, ob ein numerischer Antwortvektor ein alternierendes Muster zeigt
#'
#' Alternierend heißt: abwechselnde Werte wie 1–2–1–2–1…
#'
#' @param x Ein numerischer Vektor (Antworten einer Person)
#' @param tolerance wie viele Positionen Abweichung erlaubt sind (default: 0)
#' @return TRUE, wenn Muster erkannt, sonst FALSE
detect_alternating <- function(x, tolerance = 0) {
  # Versuche sicher in numerisch zu konvertieren
  x <- suppressWarnings(as.numeric(as.character(x)))
  x <- x[!is.na(x)]  # entferne NAs
  
  n <- length(x)
  if (n < 4) return(FALSE)  # zu kurz für Muster
  
  # Wenn alles konstant, kein Muster
  if (sd(x) == 0) return(FALSE)
  
  # Differenzen prüfen
  diffs <- diff(x)
  if (any(is.na(diffs)) || length(diffs) < 2) return(FALSE)
  
  alt_pattern <- rep(c(1, -1), length.out = length(diffs))
  dev <- sum(abs(sign(diffs) - alt_pattern) > 0, na.rm = TRUE)
  
  return(dev <= tolerance)
}
#' Validiere Daten anhand konfigurierter Schwellen pro Fragebogen
#'
#' Bewertet jeden Fall auf:
#' - Anteil gültiger Antworten (valid_ratio_score)
#' - Bearbeitungsdauer (duration_score)
#' - Fragebogennummer vorhanden (q_score)
#' - Alternierendes Antwortmuster (alt_score)
#'
#' Gibt vollständigen Datensatz zurück mit allen Scores und is_valid-Flag.
#'
#' @param data Dataframe mit den Rohdaten
#' @param config Liste mit Validierungsschwellen
#' @return Dataframe mit Zusatzfeldern zur Validierung
validate_data <- function(data, config) {
  message("🔍 Starte Validierung nach config...")
  
  # Initialisierung neuer Spalten
  data$scorer <- NA
  data$is_valid <- FALSE
  data$duration <- as.numeric(difftime(data$LASTDATA, data$STARTED, units = "mins"))
  
  data$valid_ratio_score <- NA
  data$duration_score <- NA
  data$q_score <- NA
  data$alt_score <- NA
  
  # Liste der Fragebogentypen
  qnnr_list <- unique(na.omit(data$QUESTNNR))
  
  for (qnnr in qnnr_list) {
    idx <- which(data$QUESTNNR == qnnr)
    sub <- data[idx, ]
    
    if (nrow(sub) == 0) next
    
    # 📦 Hole spezifische Konfiguration für diesen Fragetyp
    q_cfg <- config$validation$by_qnr[[qnnr]]
    threshold <- if (!is.null(q_cfg$min_valid_ratio)) q_cfg$min_valid_ratio else 0.4
    q_lo <- if (!is.null(q_cfg$min_duration_q)) q_cfg$min_duration_q else 0.1
    q_hi_mult <- config$validation$global$max_duration_multiplier
    min_score <- config$validation$global$min_score
    
    # ✅ Score: gültige Antworten (NA-Anteil)
    prefixes <- config$item_prefixes[[qnnr]]
    if (is.null(prefixes)) prefixes <- c("B", "BA", "BO", "FC", "EW", "R")  # Fallback
    
    # Erzeuge Regex dynamisch
    pattern <- paste0("^(", paste(prefixes, collapse = "|"), ")")
    item_cols <- grep(pattern, names(sub), value = TRUE)
    
    if (length(item_cols) > 0) {
      # berechne pro Zeile Anteil nicht-NA in Itemspalten
      valid_ratio <- apply(!is.na(sub[, item_cols, drop = FALSE]), 1, mean)
    } else {
      valid_ratio <- rep(NA, nrow(sub))
    }
    
    sub$valid_ratio_score <- ifelse(valid_ratio >= threshold, 1, 0)
    
    # ⏱ Score: Bearbeitungsdauer innerhalb plausibler Grenzen
    q1 <- quantile(sub$duration, q_lo, na.rm = TRUE)
    q3 <- quantile(sub$duration, 1 - q_lo, na.rm = TRUE)
    max_allowed <- q3 * q_hi_mult
    sub$duration_score <- ifelse(sub$duration >= q1 & sub$duration <= max_allowed, 1, 0)
    
    # 🔤 Score: QUESTNNR vorhanden
    sub$q_score <- ifelse(!is.na(sub$QUESTNNR), 1, 0)
    
    # 🔁 Score: Alternierendes Antwortmuster prüfen (optional)
    sub$alt_score <- 0
    item_cols <- grep("^(B|BA|FC|EW)", names(sub), value = TRUE)
    
    # nur numerische Spalten auswählen
    numeric_items <- item_cols[sapply(sub[, item_cols, drop = FALSE], is.numeric)]
    
    if (length(numeric_items) > 0) {
      sub$alt_score <- apply(sub[, numeric_items, drop = FALSE], 1, detect_alternating)
      sub$alt_score <- ifelse(sub$alt_score, 1, 0)
    } else {
      sub$alt_score <- 0
    }
    
    # 📊 Gesamtscore & Validität
    sub$scorer <- with(sub, valid_ratio_score + duration_score + q_score + alt_score)
    sub$is_valid <- sub$scorer >= min_score
    
    # 🔁 Ergebnisse zurückschreiben
    data[idx, c("valid_ratio_score", "duration_score", "q_score", "alt_score", "scorer", "is_valid")] <-
      sub[, c("valid_ratio_score", "duration_score", "q_score", "alt_score", "scorer", "is_valid")]
  }
  
  message("✅ ", sum(data$is_valid, na.rm = TRUE), " gültige Fälle von ", nrow(data), " erkannt.")
  return(data)
}
