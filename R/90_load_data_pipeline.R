# ==============================================================================
# 90_load_data_pipeline.R  —  RAW -> fragebogen -> daten_standardisiert
# ==============================================================================
# PIPELINE ORDER (mirrors main_EURNOVA.R exactly):
#   load -> validate -> bayes_score -> add_awareness (PRE-SPLIT) ->
#   spenden-extract -> split -> recode -> donation -> sociodemographics ->
#   merge_awareness (post-split, org-specific) -> make_BA_S ->
#   cross-joins -> combine-supplements -> scales -> combine_posterior -> save
#
# WHY pre-split awareness:
#   fields$start01 lists Org1_TOM, Org2_TOM, Org1_SAW, Org2_SAW etc.
#   These must exist on `validated` BEFORE split_validated_data() so they
#   land in fragebogen$start01. merge_awareness_data() then pushes org-specific
#   TOM/SAW into qnr1/qnr2 from fragebogen$start01.
#
# validierung_uebersicht.R is a diagnostic script (not a function).
#   It is NOT sourced here — use interactively or from the supplement qmd.
# ==============================================================================

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

build_fragebogen <- function(
    rebuild    = FALSE,
    use_cache  = TRUE,
    source_    = c("auto", "api", "local"),
    local_path = NULL
) {
  source_ <- match.arg(source_)

  if (!rebuild && use_cache && file.exists(FRAGEBOGEN_RDS)) {
    message("Loading cached fragebogen: ", FRAGEBOGEN_RDS)
    return(readRDS(FRAGEBOGEN_RDS))
  }

  p <- function(f) here::here("R", "pipeline", f)
  if (file.exists(p(".env.local"))) readRenviron(p(".env.local"))

  # --- Source order matters ---
  source(p("settings.R"))
  source(p("external_Sources.R"))          # org_synonyme, fields, skalen*, at03_labels
  source(p("load_data.R"))
  source(p("valid_bayes_score.R"))         # bayes_valid_score()
  source(p("validate_data.R"))
  source(p("extract_awareness_org.R"))     # extract_start_awareness() v1 (exact)
  source(p("extract_start_awareness_org.R")) # v2 fuzzy — overwrites v1
  source(p("awareness_utils.R"))           # add_awareness(), make_BA_S_vec(), get_tom/saw()
  source(p("extract_spendenbetrag.R"))
  source(p("extract_donation_data.R"))
  source(p("split_validated_data.R"))
  source(p("recode_reversed_items.R"))
  source(p("berechne_alle_skalen.R"))
  source(p("attach_sociodemographics.R"))
  source(p("merge_awareness_data.R"))
  source(p("join_followup_questionnaires.R"))
  source(p("join_followup_fallbacks.R"))
  source(p("combine_posterior.R"))
  source(p("skalen_liste.R"))              # skalen_liste = skalen_SEM

  # --- Data source ---
  data_path <- switch(source_,
    api   = config$source_path,
    local = { if (is.null(local_path)) stop("local_path required"); local_path },
    auto  = {
      cands <- c(file.path(RAW_DIR, "survey_raw.rds"),
                 file.path(RAW_DIR, "survey_raw.sav"),
                 file.path(RAW_DIR, "survey_raw.csv"))
      found <- cands[file.exists(cands)]
      if (length(found)) found[[1]] else config$source_path
    }
  )
  if (!nzchar(data_path %||% ""))
    stop("No data source. Set SOSCI_API_URL or place a local file in data/raw/.")

  # --- 1. Load + validate ---
  message("Loading: ", data_path)
  raw_data  <- load_data(data_path)
  validated <- validate_data(raw_data, config)
  validated$prob_valid <- apply(validated, 1,
    function(row) tryCatch(bayes_valid_score(row), error = function(e) NA_real_))

  # --- 2. Awareness PRE-SPLIT (fields$start01 expects these columns) ---
  message("Extracting awareness (pre-split)...")
  validated <- tryCatch(add_awareness(validated, org_synonyme),
    error = function(e) { warning("add_awareness(): ", e$message); validated })

  # --- 3. Donation amounts PRE-SPLIT ---
  validated <- extract_spenden_from_columns(
    validated, c("OF02_01","OF02_02","OF02_03","SP02_01","SP03_01"))
  if (all(c("OF02_02_num","SP06") %in% names(validated)))
    validated$OF02_02_num <- spenden_kategorien( validated, "OF02_02_num", "SP06")
  if (all(c("OF02_01_num","SP05") %in% names(validated)))
    validated$OF02_01_num <- spenden_kategorien1(validated, "OF02_01_num", "SP05")

  # --- 4. Split ---
  fragebogen <- split_validated_data(validated, fields)

  # --- 5. Reversed items + avector cleanup ---
  for (typ in intersect(c("qnr1","qnr2","qnr4","qnr5"), names(fragebogen))) {
    fragebogen[[typ]] <- recode_reversed_items(fragebogen[[typ]])
    fragebogen[[typ]] <- drop_avector(fragebogen[[typ]])
  }

  # --- 6. Donation status per questionnaire ---
  for (typ in intersect(c("qnr1","qnr2"), names(fragebogen))) {
    if (all(c("OF01_01","OF01_02","OF01_03","OF01_04") %in% names(fragebogen[[typ]])))
      fragebogen[[typ]] <- extract_donation_data(fragebogen[[typ]])
  }

  # --- 7. Sociodemographics ---
  if ("start01" %in% names(fragebogen)) {
    for (typ in intersect(c("qnr1","qnr2","qnr4"), names(fragebogen))) {
      fragebogen[[typ]] <- tryCatch(
        attach_sociodemographics(fragebogen[[typ]], fragebogen$start01),
        error = function(e) { warning("attach_sociodemographics (", typ,"): ", e$message)
          fragebogen[[typ]] })
    }
  }

  # --- 8. Org-specific awareness merge: start01 -> qnr1/qnr2 (TOM, SAW, BA_A, BA_T) ---
  fragebogen <- tryCatch(merge_awareness_data(fragebogen),
    error = function(e) { warning("merge_awareness_data(): ", e$message); fragebogen })

  # Build 3-level BA_S: TOM=2, SAW=1, else=0
  for (typ in intersect(c("qnr1","qnr2"), names(fragebogen))) {
    df <- fragebogen[[typ]]
    if (all(c("TOM","SAW") %in% names(df))) {
      df$BA_S <- make_BA_S_vec(df$TOM, df$SAW, ordered = TRUE)
      fragebogen[[typ]] <- df
    }
  }

  # --- 9. Cross-questionnaire joins ---
  fragebogen <- tryCatch(join_followup_cross_questionnaires(fragebogen),
    error = function(e) { warning("join_followup_cross_questionnaires(): ", e$message); fragebogen })
  fragebogen <- tryCatch(combine_main_questionnaires_with_supplements(fragebogen),
    error = function(e) { warning("combine_main_questionnaires_with_supplements(): ", e$message); fragebogen })

  # --- 10. Scale scores ---
  if (exists("skalen_liste") && length(skalen_liste) > 0) {
    for (typ in intersect(c("qnr1","qnr2","qnr4"), names(fragebogen))) {
      sk <- tryCatch(berechne_skalen_rekursiv(fragebogen[[typ]], skalen_liste),
        error = function(e) { warning("berechne_skalen_rekursiv (", typ,"): ", e$message); NULL })
      if (!is.null(sk)) fragebogen[[typ]] <- dplyr::bind_cols(fragebogen[[typ]], sk)
    }
  }

  # --- 11. Bayesian posterior validity combination ---
  post <- tryCatch(combine_posterior(fragebogen),
    error = function(e) { warning("combine_posterior(): ", e$message); NULL })
  if (!is.null(post)) {
    fragebogen$posterior_combined <- post
    message(sprintf("Posterior combined: %d unique CASEs", nrow(post)))
  }

  # --- 12. Build *_orig objects for R/10_datasets.R ---
  #   FC_BO_orig = qnr1: Faircloth + Boenigk items, TOM/SAW/SES attached
  #   RO_orig    = qnr2: Romero items, TOM/SAW/SES attached
  #   cross_orig = cross: inner-joined cross-model sample
  FC_BO_orig <- fragebogen[["qnr1"]] %||% fragebogen[["FC_BO"]]
  RO_orig    <- fragebogen[["qnr2"]] %||% fragebogen[["RO"]]
  cross_orig <- fragebogen[["cross"]]
  if (is.null(FC_BO_orig) || is.null(RO_orig))
    warning("FC_BO_orig or RO_orig is NULL — check pipeline steps above.")

  # --- 13. Save ---
  dir.create(dirname(FRAGEBOGEN_RDS),       recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(DATEN_STANDARDISIERT), recursive = TRUE, showWarnings = FALSE)
  saveRDS(fragebogen, FRAGEBOGEN_RDS)
  message("fragebogen.rds saved: ", FRAGEBOGEN_RDS)
  save(FC_BO_orig, RO_orig, cross_orig, fragebogen, file = DATEN_STANDARDISIERT)
  message("daten_standardisiert.RData saved: ", DATEN_STANDARDISIERT)

  invisible(fragebogen)
}
