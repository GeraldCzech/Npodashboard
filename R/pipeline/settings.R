config <- list(
  # API URL with access token: set via environment variable (never commit the token).
  # Local setup: create a file R/pipeline/.env.local containing:
  #   SOSCI_API_URL=https://soscisurvey.wu.ac.at/SpendenOrganisationen/?act=<TOKEN>&vQuality&rScript
  # or call: Sys.setenv(SOSCI_API_URL = "...")  before running build_fragebogen().
  source_path = Sys.getenv("SOSCI_API_URL", unset = ""),
  # 📂 Standard-Ausgabeverzeichnis
  output_path = here::here("outputs"),
  output_dir  = here::here("outputs"),
  # ✅ Validierungseinstellungen
  validation = list(
    global = list(
      max_duration_multiplier = 2.5,   # Dauergrenze = Median * Faktor
      min_score = 1                    # Mindestpunktwert für valide Fälle
    ),
    by_qnr = list(
      qnr1 = list(min_valid_ratio = 0.2, min_duration_q = 0.1),
      qnr2 = list(min_valid_ratio = 0.2, min_duration_q = 0.1),
      qnr4 = list(min_valid_ratio = 0.2, min_duration_q = 0.1),
      qnr5 = list(min_valid_ratio = 0.2, min_duration_q = 0.05),
      Start01 = list(min_valid_ratio = 0.4, min_duration_q = 0.1)
    ),
    
    # 📊 Klassifikation auf Basis von Bayes-Scores
    bayes_breaks = c(0.5, 0.7, 0.9),  # Schwellenwerte für Kategorien
    bayes_labels = c("fragwürdig", "unsicher", "gut", "sehr gut")
  ),
  
  # 📈 Explorative Faktorenanalyse
  efa = list(
    default_n_factors = 5,           # Fallback, falls keine Empfehlung durch Parallel Analysis
    min_complete_cases_factor = 2,   # Fälle ≥ 2 * #Items für EFA
    report_template = "scripts/report_templates/efa_report_template.Rmd"
  ),
  
  # 🎨 Diagrammoptionen
  plots = list(
    width = 10,
    height = 8,
    dpi = 300
  ),
  
  # 📦 Projektinfo
  project = list(
    name = "SpendenOrganisationen",
    description = "Auswirkungen der Marke auf die Spende an Nonprofit-Organisationen in AT",
    date = Sys.Date(),
    author = "GeraldCzech"
  )
)
config$analyse <- list(
  run_orga_details = FALSE,  # FALSE deaktiviert EFA pro Organisation
  run_cfa = TRUE ,       # TRUE aktiviert CFA
  run_sem = TRUE         # TRUE aktiviert SEM-Analyse
)