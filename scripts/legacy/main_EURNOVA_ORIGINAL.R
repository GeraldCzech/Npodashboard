  # Hauptskript für Dissertation-Projekt
  # Strang für Auswertungen EURNOVA
  # Gerald Czech
  # ====================================
  
  # 1. Pakete und Konfiguration ---------------------------------------
  #conflicts_prefer(dplyr::union)
  source("scripts/packages.R")
  library(here)
  
  # Globale Konfigurationen und Hilfsdaten
  source(here("config", "settings.R"))
  source(here("scripts/data/external_Sources.R"))
  
  # 2. Funktionsbausteine --------------------------------------------
  
  # Daten & Verarbeitung
  source(here("scripts/data/load_data.R"))
  source(here("scripts/data/validate_data.R"))
  source(here("scripts/data/split_validated_data.R"))
  source(here("scripts/data/recode_reversed_items.R"))
  source(here("scripts/extract/join_followup_questionnaires.R"))
  source(here("scripts/extract/extract_donation_data.R"))
  source(here("scripts/extract/join_followup_fallbacks.R"))
  source(here("scripts/diagnostics/valid_bayes_score.R"))
  source(here("scripts/data/attach_sociodemographics.R"))
  source(here("scripts/extract/extract_spendenbetrag.R"))
  source(here("scripts/data/berechne_alle_skalen.R"))
  # Awareness
  source(here("scripts/modules/match_org_code.R"))
  source(here("scripts/modules/z_standardisieren.R"))
  source(here("scripts/modules/awareness_utils.R"))
  source(here("scripts/extract/extract_start_awareness_org.R"))
  source(here("scripts/data/merge_awareness_data.R"))
  source(here("scripts/data/combine_posterior.R"))
  # Analyse
  source(here("scripts/analysis/efa_analysis.R"))
  source(here("scripts/analysis/efa_per_org.R"))
  source(here("scripts/analysis/crosstab_organisationen.R"))
  source(here("scripts/analysis/summarize_data_sources.R"))
  
  # CFA & Modelle
  source(here("scripts/analysis/cfa_analysis.R"))
  source(here("scripts/modules/run_cfa.R"))
  source(here("scripts/modules/cfa_model_builder.R"))
  source(here("scripts/export/plot_cfa_model.R"))
  source(here("scripts/analysis/plot_cfa_fit_indices.R"))
  source(here("scripts/analysis/summarize_cfa_results.R"))
  
  #SEM
  source(here("scripts/analysis/run_sem_analysis.R"))
  source(here("scripts/analysis/run_sem_model.R"))
  source(here("scripts/analysis/run_sem_model_template.R"))  
  # Visualisierung & Export
  source(here("scripts/diagnostics/plot_validation_heatmap.R"))
  source(here("scripts/diagnostics/plot_analysis_kmo.R"))
  source(here("scripts/export/save_outputs.R"))
  source(here("scripts/modules/io_utils.R"))
  source(here("scripts/modules/analysis_container.R"))
  source(here("scripts/export/export_cfa_report.R"))

  # 3. Hauptfunktion -------------------------------------------------
  
  main <- function() {
    message("🚀 Starte Analyse-Pipeline...")
    
    # 📥 Daten laden
    raw_data    <- load_data(config$source_path)
    #validation after processing
    validated <- raw_data
    validated   <- validate_data(raw_data, config)
    
    clean_data  <- validated[validated$is_valid, ]
    
    # Bayes-Scores berechnen later
    validated$prob_valid <- apply(validated, 1, bayes_valid_score)
    
    # Klassifikation
    #breaks <- c(-Inf, config$validation$bayes_breaks, Inf)
    #labels <- config$validation$bayes_labels
    #validated$valid_class <- cut(validated$prob_valid, breaks, labels, right = TRUE)
    
    saveRDS(validated, file = file.path(config$output_path, "validated_data.rds"))
    
    # Heatmap
    #try(plot_validation_heatmap(validated), silent = TRUE)
    
    # Awareness vorbereiten
    validated <- add_awareness(validated, org_synonyme)
    # 📊 Tabellen zu Referenzquellen erzeugen
    try({
      ref_table <- create_org_ref_summary_table(validated, org_synonyme)
      print(ref_table)
      gtsave(ref_table, filename = "output/ref_summary_table.html")
    }, silent = TRUE)
    
    try({
      ext_table <- create_external_source_table(validated)
      print(ext_table)
      gtsave(ext_table, filename = "output/external_source_table.html")
    }, silent = TRUE)
    # spendeninput normalisieren
    validated <- extract_spenden_from_columns(validated,c("OF02_01","OF02_02", "OF02_03","SP02_01","SP03_01"))
    print(paste("Anzahl Werte in OF02_02_num:", nrow(validated)-sum(is.na(validated$OF02_02_num))))
    print(paste("Anzahl Werte in OF02_01_num:", nrow(validated)-sum(is.na(validated$OF02_01_num))))
    validated$OF02_02_num  <- spenden_kategorien(validated,"OF02_02_num","SP06")
    validated$OF02_01_num  <- spenden_kategorien1(validated,"OF02_01_num","SP05")
    print(paste("Anzahl Werte in OF02_02_num nach Kat:", nrow(validated)-sum(is.na(validated$OF02_02_num))))
    print(paste("Anzahl Werte in OF02_01_num nach Kat:", nrow(validated)-sum(is.na(validated$OF02_01_num))))
     # Fragebögen trennen & Awareness/Spenden anreichern
    fragebogen <- split_validated_data(validated, fields)
    fragebogen <- merge_awareness_data(fragebogen)
    fragebogen$qnr1 <- extract_donation_data(fragebogen$qnr1)
    fragebogen$qnr2 <- extract_donation_data(fragebogen$qnr2)
    fragebogen <- join_followup_cross_questionnaires(fragebogen)
    fragebogen <- combine_main_questionnaires_with_supplements(fragebogen)
    #soziodemographie einkopieren
    fragebogen$qnr1 <- attach_sociodemographics(fragebogen$qnr1, fragebogen$start01)
    fragebogen$qnr2 <- attach_sociodemographics(fragebogen$qnr2, fragebogen$start01)
    fragebogen$qnr4 <- attach_sociodemographics(fragebogen$qnr4, fragebogen$start01)
    fragebogen$qnr5 <- attach_sociodemographics(fragebogen$qnr5, fragebogen$start01)
    fragebogen$cross <- attach_sociodemographics(fragebogen$cross, fragebogen$start01)
    # Umkodieren
    fragebogen$FC_BO <- reverse_specific_items(fragebogen$FC_BO, c("FC02_10", "FC02_12"))
    fragebogen$cross <- reverse_specific_items(fragebogen$cross, c("FC02_10", "FC02_12"))
    message("Starte umcodierung...")
    
    # Umkodieren der Bayes Logik
    #fragebogen$posterior <- combine_posterior(fragebogen)
    # z-standardisieren und -9, -1 als NA vor den Faktorenanalysen und SEM
    fragebogen$FC_BO     <- replace_negatives_with_na(fragebogen$FC_BO)
    fragebogen$RO        <- replace_negatives_with_na(fragebogen$RO)
    fragebogen$cross     <- replace_negatives_with_na(fragebogen$cross)
    fragebogen$FC_BO_orig <- fragebogen$FC_BO
    fragebogen$RO_orig   <- fragebogen$RO
    fragebogen$cross_orig <- fragebogen$cross
    felder_FC_BO <- grep("^FC|^B1|^BA_T",names(fragebogen$FC_BO),value =TRUE)
    felder_RO <- grep("^R|^BA_T",names(fragebogen$RO),value =TRUE)
    fragebogen$FC_BO <- z_standardisiere_felder(fragebogen$FC_BO_orig, felder_FC_BO)
    fragebogen$RO <- z_standardisiere_felder(fragebogen$RO_orig, felder_RO)
    fragebogen$cross <- z_standardisiere_felder(fragebogen$cross_orig, c(felder_FC_BO,felder_RO))
    
    message("?&%$§ Fragebogen umcodiert...")
    # 
    # Skalen berechnen für weitere Analysen
    fragebogen$scales <- berechne_skalen_rekursiv(fragebogen$cross, skalen_SEM, standardisieren = TRUE)
    # Analyse-Container vorbereiten
    analysis_results <- init_analysis_container()
    
    # 🔍 Crosstab
    org_summary <- create_org_crosstab(fragebogen, org_synonyme)
    print(org_summary)
    print("Jetzt bin ich hier")
    print(summary(fragebogen))
    saveRDS(fragebogen, file = file.path(config$output_path, "fragebogen.rds"))
    # EurNova spezifisch Datenaufbereitung gemeinsame Datensätze
    source(here("scripts/EURNOVA/Datenaufbereitung.R"))
    # EFA Analysen
    source(here("scripts/EURNOVA/EFA_psy_easy.R"))
    # CFA Analysen
    source(here("scripts/EURNOVA/crossCFA_lavaan_new.R"))
    # SEM Analysen
    source(here("scripts/EURNOVA/update_lavaan-klassik.R"))
    # SEM Bayes für später
    #source(here("scripts/EURNOVA/update_blavaan.R"))
    #source(here("scripts/EURNOVA/bayes_fit.R"))
    #source(here("scripts/EURNOVA/Bayes_forest-Plot.R"))
    # Speichern
    save_outputs(
      list(
        raw        = raw_data,
        validated  = validated,
        #clean      = clean_data,
        fragebogen = fragebogen,
        analysis   = analysis_results
        #cfa        = cfa_summary
        #sqources    = ref_table
      ),
      output_dir = config$output_path
    )
   
    
    git_backup()
  }
  
  # 4. Aufruf --------------------------------------------------------
  main()
  analysis <- readRDS("~/10787172/output/analysis.rds")
  fragebogen <- readRDS("~/10787172/output/fragebogen.rds")
  analysis <- readRDS("~/10787172/output/analysis.rds")
  