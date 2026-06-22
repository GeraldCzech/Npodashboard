# ==============================================================================
# R/pipeline/fields.R  —  QUESTNNR → column field definitions
# ==============================================================================
# fields is defined in external_Sources.R (lines 55-80) and sourced from there
# in build_fragebogen(). This file just ensures fields is available if someone
# sources fields.R directly without going through the full pipeline.
# ==============================================================================
if (!exists("fields")) {
  source(here::here("R", "pipeline", "external_Sources.R"))
}
stopifnot(is.list(fields), all(c("start01","qnr1","qnr2","qnr4","qnr5") %in% names(fields)))
message("fields loaded: ", paste(names(fields), collapse=", "),
        " (", sum(sapply(fields, length)), " total columns defined)")
