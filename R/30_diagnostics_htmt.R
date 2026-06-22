# ==============================================================================
# 30_diagnostics_htmt.R  —  Discriminant validity (HTMT)
# ------------------------------------------------------------------------------
# Two routes:
#  (a) htmt_semtools(): model-based HTMT via semTools (recommended, matches CFA).
#  (b) htmt_manual():   pairwise-complete Spearman HTMT, mirroring the Reviewer-2
#      appendix (mean absolute heterotrait / geometric mean of monotrait corrs).
# Interpretation thresholds: < .85 acceptable, .85-.90 borderline, > .90 problematic.
# ==============================================================================

htmt_threshold_label <- function(h) {
  dplyr::case_when(
    is.na(h)   ~ NA_character_,
    h > 0.90   ~ "problematic",
    h >= 0.85  ~ "borderline",
    TRUE       ~ "acceptable"
  )
}

# (a) semTools route -----------------------------------------------------------
# model: lavaan measurement syntax (first-order constructs only).
htmt_semtools <- function(model, data, ...) {
  stopifnot(requireNamespace("semTools", quietly = TRUE))
  m <- semTools::htmt(model = model, data = data, ...)
  # return long tidy table
  m[upper.tri(m)] <- NA
  tib <- tibble::as_tibble(as.table(m), .name_repair = "minimal")
  names(tib) <- c("construct_1", "construct_2", "HTMT")
  tib |>
    dplyr::filter(!is.na(HTMT), construct_1 != construct_2) |>
    dplyr::mutate(interpretation = htmt_threshold_label(HTMT))
}

# (b) manual Spearman route ----------------------------------------------------
# construct_items: named list, e.g. list(BO_TR = c("B101_01","B101_02","B101_03"),
#                                        BO_CO = c("B102_01","B102_02","B102_03"))
htmt_manual <- function(data, construct_items, method = "spearman") {
  cons <- names(construct_items)
  cmb  <- utils::combn(cons, 2, simplify = FALSE)

  mono_mean <- function(items) {
    if (length(items) < 2) return(NA_real_)
    cc <- suppressWarnings(stats::cor(data[, items], use = "pairwise.complete.obs",
                                      method = method))
    mean(abs(cc[upper.tri(cc)]), na.rm = TRUE)
  }

  purrr::map_dfr(cmb, function(pair) {
    i1 <- construct_items[[pair[1]]]; i2 <- construct_items[[pair[2]]]
    het <- suppressWarnings(stats::cor(data[, i1], data[, i2],
                                       use = "pairwise.complete.obs", method = method))
    het_mean <- mean(abs(het), na.rm = TRUE)
    m1 <- mono_mean(i1); m2 <- mono_mean(i2)
    htmt <- het_mean / sqrt(m1 * m2)
    tibble::tibble(
      construct_1 = pair[1], construct_2 = pair[2],
      HTMT = htmt, heterotrait_mean = het_mean,
      monotrait_1_mean = m1, monotrait_2_mean = m2,
      method = method, interpretation = htmt_threshold_label(htmt)
    )
  })
}

# Summary counts by interpretation band (Table A9 style)
htmt_summary_counts <- function(htmt_long, model_label = NA_character_) {
  htmt_long |>
    dplyr::count(interpretation, name = "n") |>
    tidyr::pivot_wider(names_from = interpretation, values_from = n, values_fill = 0) |>
    dplyr::mutate(model = model_label, .before = 1)
}
