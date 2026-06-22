#' Schätzt Validitätswahrscheinlichkeit auf Basis gewichteter Kriterien
#' @param row Ein Zeilenelement des validated-Datensatzes
#' @param weights Named numeric vector: Gewichtung pro Kriterium
#' @return Numerischer Wert (0–1): geschätzte Wahrscheinlichkeit
bayes_valid_score <- function(row, weights = c(
  valid_ratio = 0.4,
  duration = 0.3,
  q = 0.2,
  alt = 0.1
)) {
  stopifnot(abs(sum(weights) - 1) < 0.001)
  
  vr <- as.numeric(row[["valid_ratio_score"]])
  dr <- as.numeric(row[["duration_score"]])
  qr <- as.numeric(row[["q_score"]])
  ar <- ifelse(row[["alt_score"]] < 0, 0, 1)
  
  score <- weights["valid_ratio"] * vr +
    weights["duration"]    * dr +
    weights["q"]           * qr +
    weights["alt"]         * ar
  
  return(score)
}
