#' @title extract_donation_data
#' @description Berechnet Spendenstatus und -summen aus OF01* und OF02* Feldern
#' @param df Ein Fragebogen-Datensatz mit OF01_01 bis OF01_04 sowie OF02_01 und OF02_02
#' @return Ein Dataframe mit zusätzlichen Spendenvariablen
library(stringr)
extract_donation_data <- function(df) {
  df <- df %>%
    mutate(
      OF_Spender = OF01_01 | OF01_02 | OF01_03 | OF01_04,
      OF_last = str_replace_all(OF02_01, ",", "."),
      OF_last = str_remove_all(OF_last, "€|EUR|Euro|euro|\\s"),
      OF_last = as.numeric(OF_last),
      OF_2024 = str_replace_all(OF02_02, ",", "."),
      OF_2024 = str_remove_all(OF_2024, "€|EUR|Euro|euro|\\s"),
      OF_2024 = as.numeric(OF_2024)
    )
  
  # Labels setzen
  attr(df$OF_Spender, "label") <- "Spender:in (einmalig oder regelmäßig)"
  attr(df$OF_last,    "label") <- "Höhe der letzten Spende"
  attr(df$OF_2024,    "label") <- "Spende: Summe 2024"
  
  return(df)
}
