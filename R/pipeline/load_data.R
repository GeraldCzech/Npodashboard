#' Läd Daten lokal oder über die SoSciSurvey-API
#'
#' @param path Zeichenkette: Dateipfad (.sav, .rds, .csv) oder API-Link
#' @return Dataframe (tibble)
#'
#' @examples
#' load_data("data/survey_raw.sav")
#' load_data("https://...rScript")
load_data <- function(path) {
  message("📥 Lade Datenquelle...")
  
  # Pakete bei Bedarf installieren
  for (pkg in c("haven", "readr", "tibble")) {
    if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  }
  
  # Prüfe auf SoSciSurvey-API
  is_api_url <- grepl("^https?://", path) && grepl("rScript", path)
  
  if (is_api_url) {
    message("🌐 Lade Daten via SoSciSurvey-API...")
    
    expr <- sprintf('eval(parse("%s", encoding="UTF-8"))', path)
    eval(parse(text = expr))  # erwartet: erzeugt Objekt ds
    
    if (!exists("ds")) stop("❌ API-Abruf fehlgeschlagen – Objekt 'ds' nicht vorhanden.")
    
    df <- tibble::as_tibble(ds)
    message("✅ Daten erfolgreich von API geladen: ", nrow(df), " Zeilen.")
    return(df)
  }
  
  # Lokale Datei
  if (!file.exists(path)) stop("❌ Datei nicht gefunden: ", path)
  
  extension <- tolower(tools::file_ext(path))
  df <- switch(extension,
               sav = haven::read_sav(path),
               rds = readRDS(path),
               csv = readr::read_csv(path, show_col_types = FALSE),
               stop("❌ Nicht unterstützter Dateityp: ", extension)
  )
  
  df <- tibble::as_tibble(df)
  message("✅ Datei erfolgreich geladen: ", nrow(df), " Zeilen.")
  return(df)
}
