# Laden der externen INfos
# Awareness aus anderen Umfragen
# Reihenfolgen
# Spendenstände
# ─────────────────────────────────────────────
# 1. Organisationen + Synonyme definieren
# ─────────────────────────────────────────────
#source("~/Diss_zwischenstand/scripts/Items_fragebogen_mit_Gruppen.R")
org_names <- c(
  "Caritas", "SOS Kinderdorf", "Ärzte ohne Grenzen", "Licht ins Dunkel",
  "Nachbar in Not", "Diakonie - Brot für die Welt", "WWF Osterreich", "CONCORDIA Sozialprojekte",
  "Freiwillige Feuerwehr", "Volkshilfe", "Hilfswerk", "Samariterbund", "Bergrettungsdienst",
  "Greenpeace", "Dreikönigsaktion", "Missio - Papstliche Missionswerke in Osterreich", "Vier Pfoten",
  "Rote Nasen Clowndoctors", "St. Anna Kinderkrebsforschung", "Licht für die Welt",
  "UNICEF", "Amnesty International", "Care", "Debra", "World Vision", "Rotes Kreuz"
)

org_synonyme <- data.frame(
  org_id = sprintf("%02d", 1:26),
  org_name = org_names,
  stringsAsFactors = FALSE
)

org_synonyme$extended_synonyms <- list(
  c("caritas","karitas" , "charitas" ,"caritas österreich", "caritas oesterreich", "caritas", "carit"),
  c("sos kinderdorf","kinderdörfer","österr. kinderdörfer", "sos-kinderdorf", "sos", "kinderdorf", "sos kinderd.", "sos kinderdorf österreich"),
  c("ärzte ohne grenzen","ärtzte ohne grenzen", "ärzte ohne grenzeu","ärzteohne grenzern","ärztinnen ohne grenzen", "ärzte ohne granzen","aerzte ohne grenzen", "msf", "rzte ohne grenzen", "arzte ohne grenzen", "ärtze ohne grenzen","ärzte o g", "ärzte ohne genzen", "ärzte ohne grenmzen", "ärzte ohne grenuen", "ätzte ohne grenzen","doctors without borders","medecins sans frontières"),
  c("licht ins dunkel", "lucht ins dunkel", "licht ins funkel", "lichts ins dunkel","licht in dunkel", "licht ins dunkle", "licht ins dunkl", "lid"),
  c("nachbar in not","nachbarn in not", "nachbar in", "nachbar in nor"),
  c("diakonie", "brot für die welt", "dakonie"),
  c("wwf", "world wildlife fund", "world wild fund for nature", "wwf österreich", "wwf oesterreich","world wildlife foundd"),
  c("concordia"),
  c("freiwillige feuerwehr", "feuerwehr", "ff", "ffw", "feierwehr", "freiwillige feuerwehren","feuerweh"),
  c("volkshilfe"),
  c("hilfswerk"),
  c("samariterbund","sameriterbund", "samariter", "asb", "asbö","samaritabund", "samarithabund"),
  c("bergrettung", "bergrettungsdienst"),
  c("greenpeace","greenprace","greepeace","grennpeace", "geen peace", "green peace", "greenpace", "greeanpeace", "green p.","grean peace", "greenpece"),
  c("dreikönigsaktion","heilige 3 könige", "sternsinger", "heilige drei könige", "hl. 3 könige", "heiligen 3 könige","hlg drei könige"),
  c("missio", "misio ö","misso österreich"),
  c("vier pfoten","4 poten", "vier pfote","vier pforen","vier  pfoten", "4 pfoten", "vierpfoten", "4pfoten","vier pfotn", "vier photen","4-pfoten"),
  c("rote nasen", "rote nase", "klinikclowns", "clowndoctors", "rote nasen clowns", "rote nasen clown","roten nasen", "rote nsen"),
  c("st. anna", "st.anna","st.anna kinder kh", "st anna", "kinderkrebshilfe", "krebshilfe", "kinderkrebsforschung","st.anna krebsforschung"),
  c("licht für die welt","licht f.die welt", "lichtfürdiewelt","lich für die welt", "licht f d welt","licht f.d. welt", "licht fuer die welt","licht für die wwelt", "licht der welt","licht in der welt"),
  c("unicef", "unicf", "uicef"),
  c("amnesty","anmesty international", "amnesty international", "amnesti international", "emnesty", "ammesty international","amnasty international"),
  c("care"),
  c("debra", "debra austria", "schmetterlingskinder"),
  c("world vision", "worldvision", "world vision österreich"),
  c("rotes kreuz", "Rote  Kreuz","Rotes  Kreuz","rk", "örk", "oerk", "rotkreuz", "rote kreuz", "österreichisches rotes kreuz", "das rote kreuz", "red cross", "rot kreuz",
    "rote kreusz", "rotes kreiz", "rotes kreu", "rotes kreur",
    "rotes kreutz", "rotes kreuu", "rotes kruz", "rotes.kreuz",
    "roteskreuz", "rots kreuz","notarzt","rotea kreuz","rotes kteuz","rotes lreuz")
)
start01 <- c(
  "CASE", "REF", "STARTED", "AT03_RV3","AT03_RV4", "AT06_01", "AT07_CP", "AT07", "AT09_CP", "AT09", "AT08", "AT10_01", "AT10_02", "AT10_03", "AT10_04", "BA01_01", "BA01_02", "BA01_03", "BA02", "BA02_01", "BA02_02", "BA02_03", "BA02_04", "BA02_05", "BA02_06", "BA02_07", "BA02_08", "BA02_09", "BA02_10", "BA02_11", "BA02_12", "BA02_13", "BA02_14", "BA02_15", "BA02_16", "BA02_17", "BA02_18", "BA02_19", "BA02_20", "BA02_21", "BA02_22", "BA02_23", "BA02_24", "BA02_25", "BA02_26", "BA04_01", "BA04_02", "BA04_03", "BA04_04", "BA04_05", "BA04_06", "BA04_07", "BA04_08", "BA04_09", "BA04_10", "BA04_11", "BA04_12", "BA04_13", "BA04_14", "BA04_15", "BA04_16", "BA04_17", "BA04_18", "BA04_19", "BA04_20", "BA04_21", "BA04_22", "BA04_23", "BA04_24", "BA04_25", "BA04_26", "BA03_01", "BA03_02", "BA03_03", "BA03_04", "EW01_01", "EW01_02", "EW01_03", "EW01_04", "EW01_05", "EW01_06", "EW01_07", "EW01_08", "EW01_09", "EW01_10", "EW01_11", "EW01_12", "EW01_13", "EW01_14", "EW01_15", "EW01_16", "EW01_17", "EW01_18", "EW01_19", "EW01_20", "EW01_21", "EW02_01", "EW02_02", "EW02_03", "EW02_04", "EW02_05", "FC06_01", "FC06_02", "FC06_03", "SD01", "SD03", "SD11", "SD14", "SD14_01", "SD14_02", "SD14_03", "SD14_12", "SD14_04", "SD14_09", "SD14_05", "SD14_11", "SD14_06", "SD14_07", "SD14_08", "SD14_08a", "SD16", "SD21", "SP01", "SP01_01", "SP01_02", "SP01_03", "SP01_04", "SP01_05", "SP01_06", "SP01_07", "SP01_08", "SP01_09", "SP01_10", "SP01_11", "SP01_12", "SP01_13", "SP01_14", "SP01_15", "SP01_16", "SP01_17", "SP01_18", "SP01_19", "SP01_20", "SP01_21", "SP01_22", "SP01_23", "SP01_24", "SP01_25", "SP01_26", "SP01_27", "SP01_28", "SP01_29", "SP01_30", "SP01_31", "SP01_32", "SP01_33", "SP01_34", "SP02_01", "SP03_01", "SP04", "SP04x01", "SP04x02", "SP04x03", "SP04x04", "SP05", "SP06", "TIME001", "TIME002", "TIME003", "TIME004", "TIME005", "TIME008", "TIME009", "TIME010", "TIME011", "TIME012", "TIME013", "TIME014", "TIME015", "TIME_SUM", "LASTDATA", "FINISHED", "LASTPAGE", "MAXPAGE", "MISSING", "MISSREL", "TIME_RSI", "scorer","is_valid","duration","valid_ratio_score","q_score","alt_score","prob_valid","valid_class","Spontane_Awareness", "Org1_TOM", "Org2_TOM", "Org1_SAW", "Org2_SAW", "Org1_BA_A","Org2_BA_A","Org1_BA_T", "Org2_BA_T","alt_ok","SP02_01_num","SP03_01_num"
)

qnr1 <- c(
  "CASE", "REF", "STARTED", "AT03_RV3", "AT06_01", "AT07", "AT09", "B101_01", "B101_02", "B101_03", "B102_01", "B102_02", "B102_03", "BA03_01", "BA03_02", "BA03_03", "FC01_01", "FC01_02", "FC01_03", "FC01_04", "FC01_05", "FC01_06", "FC02_01", "FC02_02", "FC02_03", "FC02_04", "FC02_05", "FC02_06", "FC02_07", "FC02_08", "FC02_09", "FC02_10", "FC02_11", "FC02_12", "FC03_02", "FC03_03", "FC03_01", "FC04", "OF01", "OF01_01", "OF01_02", "OF01_03", "OF01_04", "OF01_05", "OF01_06", "OF01_07", "OF02_01", "OF02_02", "OF02_03", "SP05", "TIME001", "TIME002", "TIME003", "TIME_SUM", "LASTDATA", "FINISHED", "LASTPAGE", "MAXPAGE", "MISSING", "MISSREL", "TIME_RSI","scorer","is_valid","duration","valid_ratio_score","q_score","alt_score","prob_valid","valid_class","OF02_01_num","OF02_02_num","OF02_03_num"
)

qnr2 <- c(
  "CASE", "REF", "STARTED", "AT03_RV3", "AT06_01", "AT07", "AT09", "BA03_01", "BA03_02", "BA03_03", "OF01", "OF01_01", "OF01_02", "OF01_03", "OF01_04", "OF01_05", "OF01_06", "OF01_07", "OF02_01", "OF02_02", "OF02_03", "R201_01", "R201_02", "R201_03", "R201_04", "R201_05", "R201_06", "R201_07", "R202_01", "R202_02", "R202_03", "R202_04", "R202_05", "R202_06", "R202_07", "R202_08", "R203_01", "R203_02", "R203_03", "R203_04", "R203_05", "R203_06", "R203_07", "R203_08", "R203_09", "R204_01", "R204_02", "R204_03", "R204_04", "R204_05", "R204_06", "R204_07", "R204_08", "R204_09", "R205_01", "R205_02", "R205_03", "R205_04", "R205_05", "R205_06", "R205_07", "SP05", "TIME001", "TIME002", "TIME003", "TIME_SUM", "LASTDATA", "FINISHED", "LASTPAGE", "MAXPAGE", "MISSING", "MISSREL", "TIME_RSI","scorer","is_valid","duration","valid_ratio_score","q_score","alt_score","prob_valid","valid_class","OF02_01_num","OF02_02_num","OF02_03_num"
)

qnr4 <- c(
  "CASE", "REF", "STARTED", "AT03_RV3", "AT06_01", "AT07", "AT09", "B101_01", "B101_02", "B101_03", "B102_01", "B102_02", "B102_03", "BA03_01", "BA03_02", "BA03_03", "FC01_01", "FC01_02", "FC01_03", "FC01_04", "FC01_05", "FC01_06", "FC02_01", "FC02_02", "FC02_03", "FC02_04", "FC02_05", "FC02_06", "FC02_07", "FC02_08", "FC02_09", "FC02_10", "FC02_11", "FC02_12", "FC03_02", "FC03_03", "FC03_01", "FC04", "TIME001", "TIME_SUM", "LASTDATA", "MISSING", "MISSREL", "TIME_RSI","scorer","is_valid","duration","valid_ratio_score","q_score","alt_score","prob_valid","valid_class","scorer","is_valid","duration","valid_ratio_score","q_score","alt_score","prob_valid","valid_class"
)

qnr5 <- c(
  "CASE", "REF", "STARTED", "AT03_RV3", "AT06_01", "AT07", "AT09", "BA03_01", "BA03_02", "BA03_03", "R201_01", "R201_02", "R201_03", "R201_04", "R201_05", "R201_06", "R201_07", "R202_01", "R202_02", "R202_03", "R202_04", "R202_05", "R202_06", "R202_07", "R202_08", "R203_01", "R203_02", "R203_03", "R203_04", "R203_05", "R203_06", "R203_07", "R203_08", "R203_09", "R204_01", "R204_02", "R204_03", "R204_04", "R204_05", "R204_06", "R204_07", "R204_08", "R204_09", "R205_01", "R205_02", "R205_03", "R205_04", "R205_05", "R205_06", "R205_07", "TIME001", "TIME_SUM", "LASTDATA", "MISSING", "MISSREL", "TIME_RSI","scorer","is_valid","duration","valid_ratio_score","q_score","alt_score","prob_valid","valid_class"
)
fields <- list(
  start01 = start01,
  qnr1 = qnr1,
  qnr2 = qnr2,
  qnr4 = qnr4,
  qnr5 = qnr5
)

zielvariablen <- c(
  "REF", "BA03_01",
  "B101_01", "B101_02", "B101_03", "B102_01", "B102_02", "B102_03",
  "FC01_01", "FC01_02", "FC01_03", "FC01_04", "FC01_05", "FC01_06",
  "FC02_01", "FC02_02", "FC02_03", "FC02_04", "FC02_05", "FC02_06",
  "FC02_07", "FC02_08", "FC02_09", "FC02_10", "FC02_11", "FC02_12",
  "FC03_02", "FC03_03", "FC03_01", "FC04",
  "OF01", "OF01_01", "OF01_02", "OF01_03", "OF01_04", "OF01_05", "OF01_06", "OF01_07",
  "OF02_01", "OF02_02", "OF02_03", "SP05",
  "R201_01", "R201_02", "R201_03", "R201_04", "R201_05", "R201_06", "R201_07",
  "R202_01", "R202_02", "R202_03", "R202_04", "R202_05", "R202_06", "R202_07", "R202_08",
  "R203_01", "R203_02", "R203_03", "R203_04", "R203_05", "R203_06", "R203_07", "R203_08", "R203_09",
  "R204_01", "R204_02", "R204_03", "R204_04", "R204_05", "R204_06", "R204_07", "R204_08", "R204_09",
  "R205_01", "R205_02", "R205_03", "R205_04", "R205_05", "R205_06", "R205_07"
)
skalen <- list(
  # Faircloth
  FC_BP = c("FC01_01", "FC01_02", "FC01_03", "FC01_04", "FC01_05", "FC01_06"),
  FC_BI = c("FC02_01", "FC02_02", "FC02_03", "FC02_04", "FC02_05", "FC02_06",
            "FC02_07", "FC02_08", "FC02_09", "FC02_10", "FC02_11", "FC02_12"),
  FC_BF = c("FC03_01", "FC03_02", "FC03_03"),
  FC_RC = c("TOM", "SAW"),
  
  # Boenigk
  BO_TR = c("B101_01", "B101_02", "B101_03"),
  BO_CO = c("B102_01", "B102_02", "B102_03"),
  
  # Romero
  RO_BF = c("R201_01", "R201_02", "R201_03", "R201_04"),
  RO_BS = c("R201_05", "R201_06", "R201_07"),
  RO_BI = c("R202_01", "R202_02", "R202_03", "R202_04"),
  RO_BA = c("R202_05", "R202_06", "R202_07", "R202_08"),
  RO_BD = c("R203_01", "R203_02", "R203_03", "R203_04", "R203_05"),
  RO_BR = c("R203_06", "R203_07", "R203_08", "R203_09"),
  RO_AC = c("R204_01", "R204_02", "R204_03", "R204_04"),
  RO_EC = c("R204_05", "R204_06", "R204_07", "R204_08", "R204_09"),
  RO_ID = c("R205_01", "R205_02", "R205_03", "R205_04", "R205_05", "R205_06", "R205_07")
  
)
skalen_SEM <- c(skalen, list (
  #FC Metaskalen
  FC_BP = c("FC_BR","FC_BD"),
  FC_BI = c("FC_BC","FC_BS"),
  FC_BA = c("FC_RC","FC_BF"),
  FC_BE = c("FC_BP","FC_BI","FC_BA"),
  # BO Metaskalen
  BO_BE = c("BO_TR","BO_CO","FC_BA"),
  #RO Metaskalen
  # Brand Commitment
  RO_BC = c("RO_AC","RO_EC"),
  #Brand awareness
  RO_AW = c("RO_BF","RO_BS"),
  # Brand Personality
  RO_BP = c("RO_BD","RO_BA","RO_BR"),
    #Brand Equity
  RO_BE = c("RO_BC","RO_AW","RO_BP","RO_BI"),
  # Outcome
  #Idention to donate
  RO_ID = c("R205_01","R205_02","R205_03","R205_04","R205_05")
))
                

source_links <- data.frame(
  Name = c(
    "RK NÖ", "RK Sbg", "RK Bgld", "RK GenSek|Spender", "RK GenSek|Newsletter",
    "RK Tirol", "RK Stmk", "Caritas", "Hilfswerk", "SOS", "Nachbar in Not", "WWF",
    "Licht für die Welt", "Bergrettungsdienst", "Missio", "marketmind", "market",
    "marketagent", "Facebook/Meta", "VierPfoten", "RK Wien", "RK Vbg",
    "Facebook/Meta", "Facebook/Meta", "Facebook/Meta", "Facebook/Meta", "Facebook/Meta",
    "Linkedin", "Surveycircle"
  ),
  REF = c(
    26, 26, 26, 26, 26,
    26, 26, 1, 11, 2, 5, 7,
    20, 13, 16, NA, NA,
    NA, NA, 17, 26, 26,
    NA, NA, NA, NA, NA,
    NA, NA
  ),
  AT03_RV3 = c(
    3, 5, 1, 10, 11,
    7, 6, NA, 0, 0, 0, 0,
    0, NA, 0, NA, NA,
    NA, NA, NA, 9, 8,
    1, 2, 3, 4, 5,
    1, NA
  ),
  AT03_RV4 = c(
    0, 0, 0, NA, 1,
    0, NA, 0, 0, 0, 0, 0,
    NA, NA, NA, 1, 2,
    3, 4, NA, 0, 0,
    4, 4, 4, 4, 4,
    5, 6
  )
)
at03_labels <- c(
  "1" = "Burgenland",
  "2" = "Kärnten",
  "3" = "Niederösterreich",
  "4" = "Oberösterreich",
  "5" = "Salzburg",
  "6" = "Steiermark",
  "7" = "Tirol",
  "8" = "Vorarlberg",
  "9" = "Wien"
)
