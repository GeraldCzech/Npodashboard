#skript zur Übersicht über die Validierungseinstellungen
library(dplyr)
library(tibble)
library(tidyr)

# ⚙️ Hilfsfunktion: Hole Konfiguration pro Fragebogentyp
get_qnr_thresholds <- function(qnr, config) {
  q_cfg <- config$validation$by_qnr[[qnr]]
  list(
    min_valid_ratio = if (!is.null(q_cfg$min_valid_ratio)) q_cfg$min_valid_ratio else NA,
    min_duration_q = if (!is.null(q_cfg$min_duration_q)) q_cfg$min_duration_q else NA,
    max_duration_multiplier = config$validation$global$max_duration_multiplier,
    min_score = config$validation$global$min_score
  )
}

# 🔍 Prüfen der Ergebnisse nach Fragebogentyp
summary_table <- validated %>%
  group_by(QUESTNNR) %>%
  summarise(
    n = n(),
    Anteil_gueltig = mean(is_valid, na.rm = TRUE),
    avg_scorer = mean(scorer, na.rm = TRUE),
    Anteil_valid_ratio_OK = mean(valid_ratio_score, na.rm = TRUE),
    Anteil_duration_OK = mean(duration_score, na.rm = TRUE),
    Anteil_alt_flag = mean(alt_score < 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rowwise() %>%
  mutate(thresholds = list(get_qnr_thresholds(QUESTNNR, config))) %>%
  unnest_wider(thresholds)

# Anzeigen
print(summary_table)
print(config$validation)
library(ggplot2)

ggplot(validated, aes(x = scorer, fill = QUESTNNR)) +
  geom_histogram(binwidth = 1, position = "dodge") +
  facet_wrap(~ QUESTNNR, scales = "free_y") +
  theme_minimal() +
  labs(title = "Scorer-Verteilung pro Fragebogen", x = "Scorer", y = "Fälle")
