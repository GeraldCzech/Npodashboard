# ==============================================================================
# R/pipeline/skalen_liste.R  —  Scale definitions
# ==============================================================================
# skalen and skalen_SEM are defined in external_Sources.R (lines 97-141).
# This file exposes them under the name expected by berechne_skalen_rekursiv().
# ==============================================================================
if (!exists("skalen")) {
  source(here::here("R", "pipeline", "external_Sources.R"))
}
# berechne_skalen_rekursiv() uses skalen_liste as its argument.
# Use skalen_SEM (includes meta-scales / higher-order composites).
skalen_liste <- skalen_SEM
message("skalen_liste ready: ", length(skalen_liste), " scales defined (",
        paste(names(skalen_liste), collapse=", "), ")")
