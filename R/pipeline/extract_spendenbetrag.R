extract_spendenbetrag <- function(dftext) {
  parse_text <- function(txt) {
    if (is.na(txt) || trimws(txt) == "") return(NA_real_)
    
    txt <- tolower(txt)
    txt <- gsub(",", ".", txt)
    txt <- gsub("\\s+", " ", txt)
    txt <- gsub("â‚¬", "€", txt)
    txt <- gsub("5o", "50", txt)
    txt <- gsub("x", "*", txt)
    txt <- gsub("=", " ", txt)
    txt <- gsub("€|euro|eur", "", txt)     # Währungszeichen entfernen
    txt <- trimws(txt)
    
    if (grepl("priesterpatin|priester", txt)) return(720)
    
    # Nur Jahreszahlen entfernen, wenn es NICHT die einzige Zahl ist
    if (length(regmatches(txt, gregexpr("\\d+", txt))[[1]]) > 1) {
      txt <- gsub("\\b(19|20)\\d{2}\\b", "", txt)
    }
    
    
    if (grepl("\\d+\\s*\\*\\s*\\d+", txt)) {
      parts <- unlist(strsplit(txt, "\\*"))
      nums <- as.numeric(parts)
      if (length(nums) == 2 && all(!is.na(nums))) return(prod(nums))
    }
    
    is_monatlich <- grepl("monatlich|pro monat|jeden monat|mtl|monatl|monat", txt)
    
    matches <- regmatches(txt, gregexpr("\\d+\\.?\\d*", txt))
    zahlen <- matches[[1]]
    
    zahlen <- sapply(zahlen, function(z) {
      if (nchar(z) %% 2 == 0 && substr(z, 1, nchar(z)/2) == substr(z, nchar(z)/2 + 1, nchar(z))) {
        return(substr(z, 1, nchar(z)/2))
      }
      return(z)
    })
    
    zahlen <- as.numeric(zahlen)
    zahlen <- zahlen[!is.na(zahlen)]
    
    if (length(zahlen) == 0) return(NA_real_)
    
    num <- zahlen[1]
    if (is_monatlich) num <- num * 12
    
    return(num)
  }
  
  sapply(dftext, parse_text)
}

extract_spenden_from_columns <- function(title, namen) {
  for (spaltenname in namen) {
    if (!spaltenname %in% names(title)) {
      warning(paste("Spalte", spaltenname, "nicht im Data Frame vorhanden."))
      next
    }
    neue_spalte <- paste0(spaltenname, "_num")
    title[[neue_spalte]] <- extract_spendenbetrag(title[[spaltenname]])
  }
  return(title)
}
spenden_kategorien <- function(validated, zielname, quellname_kat) {
  # Mapping-Tabelle
  kat_map <- c(
    `1` = 0,
    `2` = 10,
    `3` = 25,
    `4` = 50,
    `5` = 75,
    `6` = 100,
    `7` = 500,
    `8` = 1000,
    `9` = 2000,
    `-9` = NA
  )
  
  # Hole die Ziel- und Quellspalten
  ziel <- validated[[zielname]]
  quelle <- as.integer(validated[[quellname_kat]])
  
  # Finde Positionen, wo Zielwert NA ist und Quelle gültig
  ergänzen <- is.na(ziel) & !is.na(quelle) & as.character(quelle) %in% names(kat_map)
  
  # Kategorien umwandeln (nur dort, wo ergänzt werden soll)
  ziel[ergänzen] <- as.numeric(kat_map[as.character(quelle[ergänzen])])
  
  return(ziel)
}
spenden_kategorien1 <- function(validated, zielname, quellname_kat) {
  # Mapping-Tabelle
  kat_map <- c(
    `1` = 0,
    `2` = 5,
    `3` = 10,
    `4` = 25,
    `5` = 50,
    `6` = 100,
    `7` = 500,
    `8` = 1000,
    `9` = 2000,
    `-9` = NA
  )
  
  # Hole die Ziel- und Quellspalten
  ziel <- validated[[zielname]]
  quelle <- as.integer(validated[[quellname_kat]])
  
  # Finde Positionen, wo Zielwert NA ist und Quelle gültig
  ergänzen <- is.na(ziel) & !is.na(quelle) & as.character(quelle) %in% names(kat_map)
  
  # Kategorien umwandeln (nur dort, wo ergänzt werden soll)
  ziel[ergänzen] <- as.numeric(kat_map[as.character(quelle[ergänzen])])
  
  return(ziel)
}