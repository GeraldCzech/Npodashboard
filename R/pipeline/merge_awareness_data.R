#' @title merge_awareness_data
#' @description Überträgt Awareness-Daten aus dem Start01-Fragebogen in qnr1 und qnr2, basierend auf fallnr und Organisationsnummer (org).
#' @param fragebogen Eine Liste mit den Elementen start01, qnr1 und qnr2
#' @return Die Liste fragebogen, bei der qnr1 und qnr2 um die Variablen TOM, SAW, BA_A und BA_T ergänzt wurden

merge_awareness_data <- function(fragebogen) {
  # Sicherstellen, dass beide Join-Keys numerisch sind
  fragebogen$start01 <- fragebogen$start01 %>%
    mutate(REF = as.numeric(REF))
  
  org_synonyme <- org_synonyme %>%
    mutate(org_id = as.numeric(org_id))
  
  # Join durchführen und REF_name ergänzen
  fragebogen$start01 <- fragebogen$start01 %>%
    left_join(org_synonyme %>%
                select(org_id, org_name),
              by = c("REF" = "org_id")) %>%
    rename(REF_name = org_name)
  # Labels für AT03_RV3
  fragebogen$start01 <- fragebogen$start01 %>%
    mutate(
      AT03_RV3 = factor(as.character(AT03_RV3), levels = names(at03_labels), labels = at03_labels)
    )
  
  # Join für qnr1 mit Start01 - org1
  qnr1_join1 <- fragebogen$qnr1 %>%
    dplyr::left_join(
      fragebogen$start01 %>%
        dplyr::select(fallnr, org_1, Org1_TOM, Org1_SAW, Org1_BA_A, Org1_BA_T) %>%
        dplyr::rename(org = org_1),
      by = c("fallnr", "org")
    ) %>%
    dplyr::mutate(
      TOM  = Org1_TOM,
      SAW  = Org1_SAW,
      BA_A = Org1_BA_A,
      BA_T = Org1_BA_T
    ) %>%
    dplyr::select(-Org1_TOM, -Org1_SAW, -Org1_BA_A, -Org1_BA_T)
  
  # Join für qnr1 mit Start01 - org2
  qnr1_final <- qnr1_join1 %>%
    dplyr::left_join(
      fragebogen$start01 %>%
        dplyr::select(fallnr, org_2, Org2_TOM, Org2_SAW, Org2_BA_A, Org2_BA_T) %>%
        dplyr::rename(org = org_2),
      by = c("fallnr", "org")
    ) %>%
    dplyr::mutate(
      TOM  = dplyr::coalesce(TOM, Org2_TOM),
      SAW  = dplyr::coalesce(SAW, Org2_SAW),
      BA_A = dplyr::coalesce(BA_A, Org2_BA_A),
      BA_T = dplyr::coalesce(BA_T, Org2_BA_T)
    ) %>%
    dplyr::select(-Org2_TOM, -Org2_SAW, -Org2_BA_A, -Org2_BA_T)
  
  fragebogen$qnr1 <- qnr1_final
  
  # Join für qnr2 mit Start01 - org1
  qnr2_join1 <- fragebogen$qnr2 %>%
    dplyr::left_join(
      fragebogen$start01 %>%
        dplyr::select(fallnr, org_1, Org1_TOM, Org1_SAW, Org1_BA_A, Org1_BA_T) %>%
        dplyr::rename(org = org_1),
      by = c("fallnr", "org")
    ) %>%
    dplyr::mutate(
      TOM  = Org1_TOM,
      SAW  = Org1_SAW,
      BA_A = Org1_BA_A,
      BA_T = Org1_BA_T
    ) %>%
    dplyr::select(-Org1_TOM, -Org1_SAW, -Org1_BA_A, -Org1_BA_T)
  
  # Join für qnr2 mit Start01 - org2
  qnr2_final <- qnr2_join1 %>%
    dplyr::left_join(
      fragebogen$start01 %>%
        dplyr::select(fallnr, org_2, Org2_TOM, Org2_SAW, Org2_BA_A, Org2_BA_T) %>%
        dplyr::rename(org = org_2),
      by = c("fallnr", "org")
    ) %>%
    dplyr::mutate(
      TOM  = dplyr::coalesce(TOM, Org2_TOM),
      SAW  = dplyr::coalesce(SAW, Org2_SAW),
      BA_A = dplyr::coalesce(BA_A, Org2_BA_A),
      BA_T = dplyr::coalesce(BA_T, Org2_BA_T)
    ) %>%
    dplyr::select(-Org2_TOM, -Org2_SAW, -Org2_BA_A, -Org2_BA_T)
  
  fragebogen$qnr2 <- qnr2_final
  
  return(fragebogen)
}
