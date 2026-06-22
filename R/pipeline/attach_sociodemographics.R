attach_sociodemographics <- function(org_table, start01_df = fragebogen$start01) {
  # List of sociodemographic and preference variables
  # SD1_Geschlecht 1 w, 2 m, 3 d, 4 NA
  # SD3_Alter Kategorien
  # SD11_Bildungsabschluss Kategorien
  # SD14_Beschäftigung
  # SD16_Haushaltseinkommen Kategorien
  # SD21 Bundesland
  
  socio_vars <- c("SD01", "SD03", "SD11", "SD14", "SD16", "SD21")
  pref_vars <- paste0("EW02_0", 1:5)  # EW02_01 to EW02_05
  vars_to_select <- c("CASE", socio_vars, pref_vars)
  
  # Prepare sociodata with CASE as character
  sociodata <- start01_df %>%
    mutate(CASE = as.character(CASE)) %>%
    select(all_of(vars_to_select)) %>%
    rename(fallnr = CASE)
  
  # Ensure fallnr in org_table is also character
  org_table <- org_table %>%
    mutate(fallnr = as.character(fallnr))
  
  # Join and return enriched table
  org_table %>%
    left_join(sociodata, by = "fallnr")
}