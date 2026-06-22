berechne_skalen_rekursiv <- function(df, skalen_liste, standardisieren = FALSE, als_mittelwert = TRUE) {
  if (!requireNamespace("tibble", quietly = TRUE)) install.packages("tibble")
  library(tibble)
  
  berechnet <- list()
  fehlende <- character()
  
  # Hilfsfunktion für rekursive Berechnung
  berechne_skala <- function(skala) {
    if (!is.null(berechnet[[skala]])) return(invisible())
    
    elemente <- skalen_liste[[skala]]
    if (is.null(elemente)) {
      fehlende <<- c(fehlende, skala)
      return(invisible())
    }
    
    # Alle noch nicht berechneten Subskalen rekursiv berechnen
    for (e in elemente) {
      if (!(e %in% names(df)) && !(e %in% names(berechnet))) {
        berechne_skala(e)
      }
    }
    
    # Jetzt Daten holen – aus df oder berechnet
    daten <- lapply(elemente, function(e) {
      if (e %in% names(berechnet)) {
        berechnet[[e]]
      } else if (e %in% names(df)) {
        df[[e]]
      } else {
        warning(paste("❌ Element nicht gefunden:", e))
        return(rep(NA, nrow(df)))
      }
    })
    
    # Daten zu Matrix
    daten_mat <- do.call(cbind, daten)
    
    # Optional standardisieren
    if (standardisieren) {
      daten_mat <- scale(daten_mat)
    }
    
    # Skalenwert berechnen
    skalenwert <- if (als_mittelwert) {
      rowMeans(daten_mat, na.rm = TRUE)
    } else {
      rowSums(daten_mat, na.rm = TRUE)
    }
    
    # Ergebnis speichern
    berechnet[[skala]] <<- skalenwert
  }
  
  # Alle Skalen berechnen
  for (skala in names(skalen_liste)) {
    berechne_skala(skala)
  }
  
  # In ein tibble zusammenfassen
  skalen_df <- as_tibble(berechnet)
  
  if (length(fehlende) > 0) {
    warning("❌ Folgende Skalen konnten nicht berechnet werden:\n", paste(unique(fehlende), collapse = ", "))
  }
  
  return(skalen_df)
}
