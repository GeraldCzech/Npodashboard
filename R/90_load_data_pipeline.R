# ==============================================================================
# 90_load_data_pipeline.R  —  RAW -> fragebogen -> daten_standardisiert
# ------------------------------------------------------------------------------
# This is the heavy data-construction pipeline from the legacy main_EURNOVA.R.
# It builds `fragebogen` (qnr1/qnr2/qnr4/start01 + FC_BO_orig/RO_orig/cross_orig)
# from the raw SoSciSurvey export, then writes daten_standardisiert.RData that
# R/10_datasets.R consumes.
#
# STATUS: stub. The ~30 helper scripts below are supplied incrementally and will
# be placed under R/pipeline/. Until then, point CBE_DATA_DIR at a folder that
# already contains daten_standardisiert.RData + fragebogen.rds and skip build.
#
# Legacy source() order (from main_EURNOVA.R) — these are the scripts still needed:
#   data/external_Sources.R
#   data/load_data.R          data/validate_data.R       data/split_validated_data.R
#   data/recode_reversed_items.R
#   extract/join_followup_questionnaires.R   extract/extract_donation_data.R
#   extract/join_followup_fallbacks.R        extract/extract_spendenbetrag.R
#   extract/extract_start_awareness_org.R
#   diagnostics/valid_bayes_score.R
#   data/attach_sociodemographics.R          data/berechne_alle_skalen.R
#   modules/match_org_code.R   modules/z_standardisieren.R   modules/awareness_utils.R
#   data/merge_awareness_data.R              data/combine_posterior.R
#   analysis/efa_analysis.R  analysis/cfa_analysis.R  modules/run_cfa.R
#   modules/cfa_model_builder.R              analysis/summarize_cfa_results.R
#   analysis/run_sem_analysis.R  analysis/run_sem_model.R  analysis/run_sem_model_template.R
#   export/save_outputs.R  modules/io_utils.R  export/export_cfa_report.R
#   config/settings.R
# ==============================================================================

build_fragebogen <- function(rebuild = FALSE) {
  if (!rebuild && file.exists(FRAGEBOGEN_RDS)) {
    message("Loading cached fragebogen: ", FRAGEBOGEN_RDS)
    return(readRDS(FRAGEBOGEN_RDS))
  }
  stop("build_fragebogen(): pipeline scripts not yet installed under R/pipeline/. ",
       "Provide them (see header) or set CBE_DATA_DIR to a folder with ",
       "daten_standardisiert.RData + fragebogen.rds.")
}
