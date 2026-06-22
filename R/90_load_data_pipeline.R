# ==============================================================================
# 90_load_data_pipeline.R  —  RAW → fragebogen → daten_standardisiert
# ==============================================================================
# Implements build_fragebogen() from the legacy main_EURNOVA.R pipeline using
# the scripts now under R/pipeline/.
#
# CREDENTIALS: The SoSciSurvey API URL (with private token) is read from the
# environment variable SOSCI_API_URL. Set it via:
#   readRenviron("R/pipeline/.env.local")   # local, gitignored
#   Sys.setenv(SOSCI_API_URL = "...")        # session
# Never hard-code the token in any committed file.
#
# MISSING (supply incrementally — see README 'Scripts still needed'):
#   R/pipeline/external_Sources.R         → org_synonyme, external ref tables
#   R/pipeline/fields.R (or settings.R)   → fields (QUESTNNR → column list)
#   R/pipeline/skalen_liste.R             → skalen_liste (scale definitions)
#   R/pipeline/join_followup_*.R          → multi-questionnaire joins
#   R/pipeline/extract_*.R                → donation / awareness extraction
#   R/pipeline/attach_sociodemographics.R → SES join
#   R/pipeline/merge_awareness_data.R     → awareness enrichment
#   R/pipeline/combine_posterior.R        → posterior combination
# ==============================================================================

build_fragebogen <- function(
    rebuild   = FALSE,
    use_cache = TRUE,
    source    = c("auto", "api", "local"),
    local_path = NULL   # path to local .sav/.rds/.csv if source = "local"
) {
  source <- match.arg(source)

  # ------------------------------------------------------------------
  # Fast path: return cached fragebogen if available and not forcing rebuild
  # ------------------------------------------------------------------
  if (!rebuild && use_cache && file.exists(FRAGEBOGEN_RDS)) {
    message("Loading cached fragebogen from: ", FRAGEBOGEN_RDS)
    return(readRDS(FRAGEBOGEN_RDS))
  }

  # ------------------------------------------------------------------
  # Source pipeline scripts (order matters)
  # ------------------------------------------------------------------
  p <- function(f) here::here("R", "pipeline", f)

  # 1. Config + credentials
  if (file.exists(p(".env.local"))) readRenviron(p(".env.local"))
  source(p("settings.R"))          # → config

  # 2. External sources (org_synonyme etc.) — optional until supplied
  if (file.exists(p("external_Sources.R"))) source(p("external_Sources.R"))

  # 3. Core data functions
  source(p("load_data.R"))            # → load_data()
  source(p("validate_data.R"))        # → validate_data(), detect_alternating()
  source(p("split_validated_data.R")) # → split_validated_data()
  source(p("recode_reversed_items.R"))# → recode_reversed_items(), reverse_specific_items(), drop_avector()
  source(p("berechne_alle_skalen.R")) # → berechne_skalen_rekursiv()

  # 4. Optional extract / join scripts (sourced if present)
  optional <- c(
    "join_followup_questionnaires.R",
    "extract_donation_data.R",
    "join_followup_fallbacks.R",
    "extract_spendenbetrag.R",
    "extract_start_awareness_org.R",
    "diagnostics/valid_bayes_score.R",
    "attach_sociodemographics.R",
    "merge_awareness_data.R",
    "combine_posterior.R",
    "modules/match_org_code.R",
    "modules/z_standardisieren.R",
    "modules/awareness_utils.R"
  )
  for (sc in optional) {
    fp <- p(sc)
    if (file.exists(fp)) source(fp) else message("(not yet supplied, skipping): ", sc)
  }

  # ------------------------------------------------------------------
  # fields: QUESTNNR → column list
  # Supplied by fields.R or defined inside settings extension.
  # Stub: minimal fallback so split_validated_data() can at least run.
  # Replace this block by sourcing the real fields definition.
  # ------------------------------------------------------------------
  if (!exists("fields")) {
    message("⚠️  'fields' not found — using minimal fallback. Supply R/pipeline/fields.R.")
    fields <- list(
      start01 = NULL,   # NULL → dplyr::everything()
      qnr1    = NULL,
      qnr2    = NULL,
      qnr4    = NULL,
      qnr5    = NULL
    )
  }

  # ------------------------------------------------------------------
  # Determine data source
  # ------------------------------------------------------------------
  data_path <- switch(
    source,
    api   = config$source_path,
    local = {
      if (is.null(local_path)) stop("local_path must be provided when source = 'local'")
      local_path
    },
    auto  = {
      # prefer local raw file if present, fall back to API
      candidates <- c(
        file.path(RAW_DIR, "survey_raw.rds"),
        file.path(RAW_DIR, "survey_raw.sav"),
        file.path(RAW_DIR, "survey_raw.csv")
      )
      found <- candidates[file.exists(candidates)]
      if (length(found)) found[[1]] else config$source_path
    }
  )

  if (!nzchar(data_path)) {
    stop("No data source available. Set SOSCI_API_URL or place a local file in data/raw/. ",
         "See R/pipeline/.env.local.template.")
  }

  # ------------------------------------------------------------------
  # Pipeline execution (mirrors main_EURNOVA.R main())
  # ------------------------------------------------------------------
  message("📥 Loading raw data from: ", data_path)
  raw_data  <- load_data(data_path)
  validated <- validate_data(raw_data, config)
  fragebogen <- split_validated_data(validated, fields)

  # Reversed items and scale scores on each questionnaire sub-frame
  for (typ in intersect(c("qnr1", "qnr2", "qnr4"), names(fragebogen))) {
    fragebogen[[typ]] <- recode_reversed_items(fragebogen[[typ]])
    fragebogen[[typ]] <- drop_avector(fragebogen[[typ]])
  }

  # Optional pipeline steps (only called if functions were sourced)
  if (exists("join_followup_cross_questionnaires"))
    fragebogen <- join_followup_cross_questionnaires(fragebogen)
  if (exists("combine_main_questionnaires_with_supplements"))
    fragebogen <- combine_main_questionnaires_with_supplements(fragebogen)
  if (exists("attach_sociodemographics") && "start01" %in% names(fragebogen)) {
    for (typ in intersect(c("qnr1", "qnr2", "qnr4"), names(fragebogen)))
      fragebogen[[typ]] <- attach_sociodemographics(fragebogen[[typ]], fragebogen$start01)
  }
  if (exists("extract_spenden_from_columns")) {
    for (typ in intersect(c("qnr1", "qnr2"), names(fragebogen)))
      fragebogen[[typ]] <- extract_spenden_from_columns(
        fragebogen[[typ]], c("OF02_01","OF02_02","OF02_03","SP02_01","SP03_01"))
  }
  if (exists("add_awareness") && exists("org_synonyme")) {
    fragebogen <- lapply(fragebogen, function(df)
      tryCatch(add_awareness(df, org_synonyme), error = function(e) df))
  }
  if (exists("merge_awareness_data"))
    fragebogen <- merge_awareness_data(fragebogen)

  # Scale scores (if skalen_liste is defined)
  if (exists("skalen_liste")) {
    for (typ in intersect(c("qnr1", "qnr2", "qnr4"), names(fragebogen))) {
      skalen <- berechne_skalen_rekursiv(fragebogen[[typ]], skalen_liste)
      fragebogen[[typ]] <- dplyr::bind_cols(fragebogen[[typ]], skalen)
    }
  } else {
    message("⚠️  'skalen_liste' not found — scale scores skipped. Supply R/pipeline/skalen_liste.R.")
  }

  # ------------------------------------------------------------------
  # Cache and export daten_standardisiert.RData
  # ------------------------------------------------------------------
  dir.create(dirname(FRAGEBOGEN_RDS), recursive = TRUE, showWarnings = FALSE)
  saveRDS(fragebogen, FRAGEBOGEN_RDS)
  message("✅ fragebogen saved: ", FRAGEBOGEN_RDS)

  # The *_orig objects consumed by R/10_datasets.R come from fragebogen.
  # If your pipeline builds them separately, save them into DATEN_STANDARDISIERT:
  FC_BO_orig <- fragebogen$qnr1
  RO_orig    <- fragebogen$qnr2
  cross_orig <- fragebogen$qnr4
  save(FC_BO_orig, RO_orig, cross_orig,
       file = DATEN_STANDARDISIERT)
  message("✅ daten_standardisiert.RData saved: ", DATEN_STANDARDISIERT)

  invisible(fragebogen)
}
