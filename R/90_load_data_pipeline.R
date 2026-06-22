# ==============================================================================
# 90_load_data_pipeline.R  —  RAW → fragebogen → daten_standardisiert
# ==============================================================================
# Implements build_fragebogen() from the legacy main_EURNOVA.R pipeline.
# All helper scripts live under R/pipeline/.
#
# PIPELINE ORDER (mirrors main_EURNOVA.R):
#   load → validate → split → recode → spenden-extract → awareness → donation →
#   join-cross → combine-supplements → sociodemographics → scales → save
#
# CREDENTIALS: SoSciSurvey API URL read from env var SOSCI_API_URL.
#   Set via: readRenviron("R/pipeline/.env.local")
#   or:      Sys.setenv(SOSCI_API_URL = "...")
#
# STILL MISSING (supply next):
#   R/pipeline/awareness_utils.R   — add_awareness(), z_standardisieren()
#   R/pipeline/combine_posterior.R — combine_main_questionnaires...
#   R/pipeline/valid_bayes_score.R — bayes_valid_score()
#   config/settings.R              — extended config (item_prefixes etc.)
# ==============================================================================

build_fragebogen <- function(
    rebuild    = FALSE,
    use_cache  = TRUE,
    source_    = c("auto", "api", "local"),   # `source` is a base R fn, use source_
    local_path = NULL
) {
  source_ <- match.arg(source_)

  # ------------------------------------------------------------------
  # Fast path: cached fragebogen
  # ------------------------------------------------------------------
  if (!rebuild && use_cache && file.exists(FRAGEBOGEN_RDS)) {
    message("Loading cached fragebogen: ", FRAGEBOGEN_RDS)
    return(readRDS(FRAGEBOGEN_RDS))
  }

  # ------------------------------------------------------------------
  # Source all pipeline scripts in correct order
  # ------------------------------------------------------------------
  p <- function(f) here::here("R", "pipeline", f)

  # Credentials
  if (file.exists(p(".env.local"))) readRenviron(p(".env.local"))

  # Config
  source(p("settings.R"))                     # → config, output paths

  # Reference data: org_synonyme, fields, skalen, skalen_SEM, at03_labels
  source(p("external_Sources.R"))

  # Core data-handling functions
  source(p("load_data.R"))                    # → load_data()
  source(p("validate_data.R"))               # → validate_data(), detect_alternating()
  source(p("split_validated_data.R"))        # → split_validated_data()
  source(p("recode_reversed_items.R"))       # → recode_reversed_items(), reverse_specific_items(), drop_avector()
  source(p("berechne_alle_skalen.R"))        # → berechne_skalen_rekursiv()

  # Awareness: source older first, then improved fuzzy version overwrites
  source(p("extract_awareness_org.R"))        # → extract_start_awareness() v1 (exact match)
  source(p("extract_start_awareness_org.R")) # → extract_start_awareness() v2 (fuzzy, overwrites v1)
                                              # → get_start_awareness_data()
  if (file.exists(p("awareness_utils.R")))
    source(p("awareness_utils.R"))           # → add_awareness(), z_standardisieren() [pending]

  # Donation / spending
  source(p("extract_spendenbetrag.R"))       # → extract_spendenbetrag(), extract_spenden_from_columns()
                                              #   spenden_kategorien(), spenden_kategorien1()
  source(p("extract_donation_data.R"))       # → extract_donation_data()  (OF_Spender, OF_last, OF_2024)

  # Questionnaire joins
  source(p("join_followup_questionnaires.R"))# → join_followup_cross_questionnaires()
                                              #   compare_column_types(), harmonize_df()
  source(p("join_followup_fallbacks.R"))     # → combine_main_questionnaires_with_supplements()

  # Awareness merge + sociodemographics
  source(p("merge_awareness_data.R"))        # → merge_awareness_data()
  source(p("attach_sociodemographics.R"))    # → attach_sociodemographics()

  # Optional (not yet supplied)
  if (file.exists(p("combine_posterior.R")))  source(p("combine_posterior.R"))
  if (file.exists(p("valid_bayes_score.R")))  source(p("valid_bayes_score.R"))

  # ------------------------------------------------------------------
  # Determine data source
  # ------------------------------------------------------------------
  data_path <- switch(
    source_,
    api   = config$source_path,
    local = {
      if (is.null(local_path)) stop("local_path required when source_ = 'local'")
      local_path
    },
    auto  = {
      candidates <- c(
        file.path(RAW_DIR, "survey_raw.rds"),
        file.path(RAW_DIR, "survey_raw.sav"),
        file.path(RAW_DIR, "survey_raw.csv")
      )
      found <- candidates[file.exists(candidates)]
      if (length(found)) found[[1]] else config$source_path
    }
  )

  if (!nzchar(data_path %||% "")) {
    stop("No data source. Set SOSCI_API_URL or place a local file in data/raw/. ",
         "See R/pipeline/.env.local.template.")
  }
  # ------------------------------------------------------------------
  # 1. Load, validate, split
  # ------------------------------------------------------------------
  message("📥 Loading: ", data_path)
  raw_data   <- load_data(data_path)
  validated  <- validate_data(raw_data, config)

  # Donation amount extraction on full validated frame (before split)
  # extract_spenden_from_columns creates OF02_01_num, OF02_02_num, OF02_03_num
  for (col in c("OF02_01","OF02_02","OF02_03","SP02_01","SP03_01")) {
    if (col %in% names(validated))
      validated <- extract_spenden_from_columns(validated, col)
  }
  # Fill from category fallbacks (SP06 → OF02_02_num, SP05 → OF02_01_num)
  if (all(c("OF02_02_num","SP06") %in% names(validated)))
    validated$OF02_02_num <- spenden_kategorien(validated,  "OF02_02_num", "SP06")
  if (all(c("OF02_01_num","SP05") %in% names(validated)))
    validated$OF02_01_num <- spenden_kategorien1(validated, "OF02_01_num", "SP05")

  fragebogen <- split_validated_data(validated, fields)

  # ------------------------------------------------------------------
  # 2. Reversed items + avector cleanup on questionnaire sub-frames
  # ------------------------------------------------------------------
  for (typ in intersect(c("qnr1","qnr2","qnr4","qnr5"), names(fragebogen))) {
    fragebogen[[typ]] <- recode_reversed_items(fragebogen[[typ]])
    fragebogen[[typ]] <- drop_avector(fragebogen[[typ]])
  }

  # ------------------------------------------------------------------
  # 3. Awareness: extract from start01, merge into qnr1/qnr2
  # ------------------------------------------------------------------
  if ("start01" %in% names(fragebogen)) {
    message("📡 Extracting awareness from start01...")
    awareness_out <- tryCatch(
      extract_start_awareness(fragebogen$start01, org_synonyme),
      error = function(e) { warning("extract_start_awareness failed: ", e$message); NULL }
    )
    if (!is.null(awareness_out)) {
      # Join awareness columns back into start01 by CASE
      fragebogen$start01 <- fragebogen$start01 %>%
        dplyr::left_join(
          awareness_out %>% dplyr::mutate(CASE = as.character(CASE)),
          by = "CASE"
        )
    }
    # AT03 province labels
    if (exists("at03_labels") && "AT03_RV3" %in% names(fragebogen$start01)) {
      fragebogen$start01 <- fragebogen$start01 %>%
        dplyr::mutate(
          AT03_RV3 = factor(as.character(AT03_RV3),
                            levels = names(at03_labels), labels = at03_labels)
        )
    }
    # Merge TOM/SAW/BA_A/BA_T into qnr1 and qnr2
    fragebogen <- tryCatch(
      merge_awareness_data(fragebogen),
      error = function(e) { warning("merge_awareness_data failed: ", e$message); fragebogen }
    )
  }

  # ------------------------------------------------------------------
  # 4. Donation data on qnr1/qnr2: OF_Spender, OF_last, OF_2024
  # ------------------------------------------------------------------
  for (typ in intersect(c("qnr1","qnr2"), names(fragebogen))) {
    if (all(c("OF01_01","OF01_02","OF01_03","OF01_04") %in% names(fragebogen[[typ]]))) {
      fragebogen[[typ]] <- extract_donation_data(fragebogen[[typ]])
    }
  }

  # ------------------------------------------------------------------
  # 5. Sociodemographics (SD01, SD03, SD11, SD14, SD16, SD21, EW02_*)
  # ------------------------------------------------------------------
  if ("start01" %in% names(fragebogen)) {
    for (typ in intersect(c("qnr1","qnr2","qnr4"), names(fragebogen))) {
      fragebogen[[typ]] <- tryCatch(
        attach_sociodemographics(fragebogen[[typ]], fragebogen$start01),
        error = function(e) { warning("attach_sociodemographics (", typ, "): ", e$message); fragebogen[[typ]] }
      )
    }
  }

  # ------------------------------------------------------------------
  # 6. Cross-questionnaire joins
  #    join_followup_cross_questionnaires: qnr1+qnr5 / qnr2+qnr4 → fragebogen$cross
  #    combine_main_questionnaires: qnr1+qnr4 → FC_BO, qnr2+qnr5 → RO
  # ------------------------------------------------------------------
  fragebogen <- tryCatch(
    join_followup_cross_questionnaires(fragebogen),
    error = function(e) { warning("join_followup_cross_questionnaires: ", e$message); fragebogen }
  )
  fragebogen <- tryCatch(
    combine_main_questionnaires_with_supplements(fragebogen),
    error = function(e) { warning("combine_main_questionnaires_with_supplements: ", e$message); fragebogen }
  )

  # ------------------------------------------------------------------
  # 7. Scale scores via berechne_skalen_rekursiv
  # ------------------------------------------------------------------
  source(p("skalen_liste.R"))  # → skalen_liste (= skalen_SEM)
  if (exists("skalen_liste") && length(skalen_liste) > 0) {
    for (typ in intersect(c("qnr1","qnr2","qnr4"), names(fragebogen))) {
      skalen_df <- tryCatch(
        berechne_skalen_rekursiv(fragebogen[[typ]], skalen_liste),
        error = function(e) { warning("berechne_skalen_rekursiv (", typ, "): ", e$message); NULL }
      )
      if (!is.null(skalen_df))
        fragebogen[[typ]] <- dplyr::bind_cols(fragebogen[[typ]], skalen_df)
    }
  }

  # ------------------------------------------------------------------
  # 8. Build *_orig objects for 10_datasets.R
  #    Convention (from legacy):
  #      FC_BO_orig = qnr1  (Faircloth + Boenigk items)
  #      RO_orig    = qnr2  (Romero items)
  #      cross_orig = cross (inner-joined cross-model sample)
  # ------------------------------------------------------------------
  FC_BO_orig <- fragebogen[["qnr1"]]   %||% fragebogen[["FC_BO"]]
  RO_orig    <- fragebogen[["qnr2"]]   %||% fragebogen[["RO"]]
  cross_orig <- fragebogen[["cross"]]

  if (is.null(FC_BO_orig) || is.null(RO_orig)) {
    warning("FC_BO_orig or RO_orig is NULL — daten_standardisiert.RData may be incomplete.")
  }

  # ------------------------------------------------------------------
  # 9. Cache
  # ------------------------------------------------------------------
  dir.create(dirname(FRAGEBOGEN_RDS),      recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(DATEN_STANDARDISIERT), recursive = TRUE, showWarnings = FALSE)

  saveRDS(fragebogen, FRAGEBOGEN_RDS)
  message("✅ fragebogen saved: ", FRAGEBOGEN_RDS)

  save(FC_BO_orig, RO_orig, cross_orig, fragebogen,
       file = DATEN_STANDARDISIERT)
  message("✅ daten_standardisiert.RData saved: ", DATEN_STANDARDISIERT)

  invisible(fragebogen)
}

# null-coalescing helper (used above before formal definition)
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b
