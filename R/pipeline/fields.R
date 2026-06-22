# ==============================================================================
# R/pipeline/fields.R  —  QUESTNNR → column field definitions
# ==============================================================================
# STATUS: STUB — supply the real version.
# This file must define `fields`, a named list mapping each QUESTNNR type to
# a character vector of column names to keep in split_validated_data().
# Example structure (replace with actual column lists from SoSciSurvey export):
#
# fields <- list(
#   start01 = c("CASE", "STARTED", "FINISHED", "LASTDATA", "QUESTNNR",
#               "BA03_01", "BA03_02", <sociodemographic cols...>),
#   qnr1    = c("REF", "CASE", "STARTED", "QUESTNNR", "BA03_01",
#               "FC01_01", "FC01_02", ..., "B101_01", ..., "OF02_01", ...),
#   qnr2    = c("REF", "CASE", "STARTED", "QUESTNNR", "BA03_01",
#               "R201_01", ..., "OF02_01", ...),
#   qnr4    = c("REF", "CASE", "STARTED", "QUESTNNR", <cross-model cols...>),
#   qnr5    = c("REF", "CASE", "STARTED", "QUESTNNR", <follow-up cols...>)
# )
#
# Or use NULL per type to keep all columns (split_validated_data uses any_of()).
# ==============================================================================
message("⚠️  R/pipeline/fields.R is a stub — fields definition not yet supplied.")
if (!exists("fields")) fields <- list(start01=NULL, qnr1=NULL, qnr2=NULL, qnr4=NULL, qnr5=NULL)
