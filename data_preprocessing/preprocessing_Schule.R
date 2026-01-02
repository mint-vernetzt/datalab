# Akronym übergeben für Datensatz-Pfad in Onedrive

#akro <- "kbr"

################################################################################
#
# Data Lab
# Vorbereitung Datensatz: Schule
# Author: Katharina Brunner, Januar 2023
# Quelle: C:\Users\kbr\OneDrive - Stifterverband\MINTvernetzt (SV)\MINTv_SV_AP7 MINT-DataLab\02 Datenmaterial\02_Prozess\Datenaufbereitung 2023\Schule
#
################################################################################


# Erstellt "kurse" --------------------------------------------------------

## Rohdatensatz einlesen ------------------------------------------------------
library(dplyr)
library(tidyr)
library(readxl)
library(stringr)

# wd <- getwd()
# setwd(wd)
# akro <- "kab"
#  akro <- "kbr"
# setwd(paste0("C:/Users/", akro,
      # "/OneDrive - Stifterverband/MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/02 Datenmaterial/01_Rohdaten/02_Alle Daten"))
# wd <- getwd() #für Datensatz einlesen später nötig

# Funktion zum Daten einlesen (Quelle: Skript aus 2022 - Spezifische_Kurse_kab)
read_data <- function(file, BL, year, course_level) {

  data <- readxl::read_excel(
    file,
    sheet = BL,
    range = ifelse(course_level == "Grundkurse", "A8:L34",
                   ifelse(course_level == "Leistungskurse", "A38:L64")),
    col_names = FALSE )

  data <- data[ , c(1, 4, 5, 6)]

  # add headlines
  header <- c("fachbereich", "gesamt","frauen", "männer")
  colnames(data) <- header

  data$frauen <- as.double(data$frauen)
  data$männer <- as.double(data$männer)

  data$indikator <- course_level
  data$Jahr <- year
  data$region = BL
  data$bereich = "Schule"
  data$quelle = "Sekretariat der Ständigen Konferenz der Kultusminister der Länder in der Bundesrepublik Deutschland (KMK), 2021: Statistik IVC"
  data$hinweise = "Abweichungen möglich, Daten nicht offiziell geprüft"

  # ins long-Format bringen
  data <- data %>%
    tidyr::pivot_longer(cols = gesamt:männer, values_to = "wert")
  data <- data %>%
    dplyr::rename(anzeige_geschlecht = name)

  return(data)
}

# Datentabellen einlesen
# wd2 <- paste0(wd, "/raw")
# setwd(wd2)
akro <- "kbr"
pfad <- paste0("C:/Users/", akro,
               "/OneDrive - Stifterverband/MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/02 Datenmaterial/01_Rohdaten/02_Alle Daten/")

#pfad_kek <- "C:/Users/kab/OneDrive - Stifterverband/AP7 MINT-DataLab/02 Datenmaterial/01_Rohdaten/02_Alle Daten/"

#pfad <- pfad_kek

### 2021-2023 ----
data_LK_D  <- read_data(paste0(pfad,"KMK023_Aus_Kurse_2021_Werte.xlsx"), "D" , 2021, "Leistungskurse")
data_LK_BW <- read_data(paste0(pfad,"KMK023_Aus_Kurse_2021_Werte.xlsx"), "BW", 2021, "Leistungskurse")
data_LK_BY <- read_data(paste0(pfad,"KMK023_Aus_Kurse_2021_Werte.xlsx"), "BY", 2021, "Leistungskurse")
data_LK_BE <- read_data(paste0(pfad,"KMK023_Aus_Kurse_2021_Werte.xlsx"), "BE", 2021, "Leistungskurse")
data_LK_BB <- read_data(paste0(pfad,"KMK023_Aus_Kurse_2021_Werte.xlsx"), "BB", 2021, "Leistungskurse")
data_LK_HB <- read_data(paste0(pfad,"KMK023_Aus_Kurse_2021_Werte.xlsx"), "HB", 2021, "Leistungskurse")
data_LK_HH <- read_data(paste0(pfad,"KMK023_Aus_Kurse_2021_Werte.xlsx"), "HH", 2021, "Leistungskurse")
data_LK_HE <- read_data(paste0(pfad,"KMK023_Aus_Kurse_2021_Werte.xlsx"), "HE", 2021, "Leistungskurse")
data_LK_MV <- read_data(paste0(pfad,"KMK023_Aus_Kurse_2021_Werte.xlsx"), "MV", 2021, "Leistungskurse")
data_LK_NI <- read_data(paste0(pfad,"KMK023_Aus_Kurse_2021_Werte.xlsx"), "NI", 2021, "Leistungskurse")
data_LK_NW <- read_data(paste0(pfad,"KMK023_Aus_Kurse_2021_Werte.xlsx"), "NW", 2021, "Leistungskurse")
data_LK_RP <- read_data(paste0(pfad,"KMK023_Aus_Kurse_2021_Werte.xlsx"), "RP", 2021, "Leistungskurse")
data_LK_SL <- read_data(paste0(pfad,"KMK023_Aus_Kurse_2021_Werte.xlsx"), "SL", 2021, "Leistungskurse")
data_LK_SN <- read_data(paste0(pfad,"KMK023_Aus_Kurse_2021_Werte.xlsx"), "SN", 2021, "Leistungskurse")
data_LK_ST <- read_data(paste0(pfad,"KMK023_Aus_Kurse_2021_Werte.xlsx"), "ST", 2021, "Leistungskurse")
data_LK_SH <- read_data(paste0(pfad,"KMK023_Aus_Kurse_2021_Werte.xlsx"), "SH", 2021, "Leistungskurse")
data_LK_TH <- read_data(paste0(pfad,"KMK023_Aus_Kurse_2021_Werte.xlsx"), "TH", 2021, "Leistungskurse")

data_GK_D  <- read_data(paste0(pfad, "KMK023_Aus_Kurse_2021_Werte.xlsx"), "D" , 2021, "Grundkurse")
data_GK_BW <- read_data(paste0(pfad, "KMK023_Aus_Kurse_2021_Werte.xlsx"), "BW", 2021, "Grundkurse")
data_GK_BY <- read_data(paste0(pfad, "KMK023_Aus_Kurse_2021_Werte.xlsx"), "BY", 2021, "Grundkurse")
data_GK_BE <- read_data(paste0(pfad, "KMK023_Aus_Kurse_2021_Werte.xlsx"), "BE", 2021, "Grundkurse")
data_GK_BB <- read_data(paste0(pfad, "KMK023_Aus_Kurse_2021_Werte.xlsx"), "BB", 2021, "Grundkurse")
data_GK_HB <- read_data(paste0(pfad, "KMK023_Aus_Kurse_2021_Werte.xlsx"), "HB", 2021, "Grundkurse")
data_GK_HH <- read_data(paste0(pfad, "KMK023_Aus_Kurse_2021_Werte.xlsx"), "HH", 2021, "Grundkurse")
data_GK_HE <- read_data(paste0(pfad, "KMK023_Aus_Kurse_2021_Werte.xlsx"), "HE", 2021, "Grundkurse")
data_GK_MV <- read_data(paste0(pfad, "KMK023_Aus_Kurse_2021_Werte.xlsx"), "MV", 2021, "Grundkurse")
data_GK_NI <- read_data(paste0(pfad, "KMK023_Aus_Kurse_2021_Werte.xlsx"), "NI", 2021, "Grundkurse")
data_GK_NW <- read_data(paste0(pfad, "KMK023_Aus_Kurse_2021_Werte.xlsx"), "NW", 2021, "Grundkurse")
data_GK_RP <- read_data(paste0(pfad, "KMK023_Aus_Kurse_2021_Werte.xlsx"), "RP", 2021, "Grundkurse")
data_GK_SL <- read_data(paste0(pfad, "KMK023_Aus_Kurse_2021_Werte.xlsx"), "SL", 2021, "Grundkurse")
data_GK_SN <- read_data(paste0(pfad, "KMK023_Aus_Kurse_2021_Werte.xlsx"), "SN", 2021, "Grundkurse")
data_GK_ST <- read_data(paste0(pfad, "KMK023_Aus_Kurse_2021_Werte.xlsx"), "ST", 2021, "Grundkurse")
data_GK_SH <- read_data(paste0(pfad, "KMK023_Aus_Kurse_2021_Werte.xlsx"), "SH", 2021, "Grundkurse")
data_GK_TH <- read_data(paste0(pfad, "KMK023_Aus_Kurse_2021_Werte.xlsx"), "TH", 2021, "Grundkurse")

# daten für 2022

data22_LK_D  <- read_data(paste0(pfad,"KMK024_Aus_Kurse_2022.xlsx"), "D" , 2022, "Leistungskurse")
data22_LK_BW <- read_data(paste0(pfad,"KMK024_Aus_Kurse_2022.xlsx"), "BW", 2022, "Leistungskurse")
data22_LK_BY <- read_data(paste0(pfad,"KMK024_Aus_Kurse_2022.xlsx"), "BY", 2022, "Leistungskurse")
data22_LK_BE <- read_data(paste0(pfad,"KMK024_Aus_Kurse_2022.xlsx"), "BE", 2022, "Leistungskurse")
data22_LK_BB <- read_data(paste0(pfad,"KMK024_Aus_Kurse_2022.xlsx"), "BB", 2022, "Leistungskurse")
data22_LK_HB <- read_data(paste0(pfad,"KMK024_Aus_Kurse_2022.xlsx"), "HB", 2022, "Leistungskurse")
data22_LK_HH <- read_data(paste0(pfad,"KMK024_Aus_Kurse_2022.xlsx"), "HH", 2022, "Leistungskurse")
data22_LK_HE <- read_data(paste0(pfad,"KMK024_Aus_Kurse_2022.xlsx"), "HE", 2022, "Leistungskurse")
data22_LK_MV <- read_data(paste0(pfad,"KMK024_Aus_Kurse_2022.xlsx"), "MV", 2022, "Leistungskurse")
data22_LK_NI <- read_data(paste0(pfad,"KMK024_Aus_Kurse_2022.xlsx"), "NI", 2022, "Leistungskurse")
data22_LK_NW <- read_data(paste0(pfad,"KMK024_Aus_Kurse_2022.xlsx"), "NW", 2022, "Leistungskurse")
data22_LK_RP <- read_data(paste0(pfad,"KMK024_Aus_Kurse_2022.xlsx"), "RP", 2022, "Leistungskurse")
data22_LK_SL <- read_data(paste0(pfad,"KMK024_Aus_Kurse_2022.xlsx"), "SL", 2022, "Leistungskurse")
data22_LK_SN <- read_data(paste0(pfad,"KMK024_Aus_Kurse_2022.xlsx"), "SN", 2022, "Leistungskurse")
data22_LK_ST <- read_data(paste0(pfad,"KMK024_Aus_Kurse_2022.xlsx"), "ST", 2022, "Leistungskurse")
data22_LK_SH <- read_data(paste0(pfad,"KMK024_Aus_Kurse_2022.xlsx"), "SH", 2022, "Leistungskurse")
data22_LK_TH <- read_data(paste0(pfad,"KMK024_Aus_Kurse_2022.xlsx"), "TH", 2022, "Leistungskurse")

data22_GK_D  <- read_data(paste0(pfad, "KMK024_Aus_Kurse_2022.xlsx"), "D" , 2022, "Grundkurse")
data22_GK_BW <- read_data(paste0(pfad, "KMK024_Aus_Kurse_2022.xlsx"), "BW", 2022, "Grundkurse")
data22_GK_BY <- read_data(paste0(pfad, "KMK024_Aus_Kurse_2022.xlsx"), "BY", 2022, "Grundkurse")
data22_GK_BE <- read_data(paste0(pfad, "KMK024_Aus_Kurse_2022.xlsx"), "BE", 2022, "Grundkurse")
data22_GK_BB <- read_data(paste0(pfad, "KMK024_Aus_Kurse_2022.xlsx"), "BB", 2022, "Grundkurse")
data22_GK_HB <- read_data(paste0(pfad, "KMK024_Aus_Kurse_2022.xlsx"), "HB", 2022, "Grundkurse")
data22_GK_HH <- read_data(paste0(pfad, "KMK024_Aus_Kurse_2022.xlsx"), "HH", 2022, "Grundkurse")
data22_GK_HE <- read_data(paste0(pfad, "KMK024_Aus_Kurse_2022.xlsx"), "HE", 2022, "Grundkurse")
data22_GK_MV <- read_data(paste0(pfad, "KMK024_Aus_Kurse_2022.xlsx"), "MV", 2022, "Grundkurse")
data22_GK_NI <- read_data(paste0(pfad, "KMK024_Aus_Kurse_2022.xlsx"), "NI", 2022, "Grundkurse")
data22_GK_NW <- read_data(paste0(pfad, "KMK024_Aus_Kurse_2022.xlsx"), "NW", 2022, "Grundkurse")
data22_GK_RP <- read_data(paste0(pfad, "KMK024_Aus_Kurse_2022.xlsx"), "RP", 2022, "Grundkurse")
data22_GK_SL <- read_data(paste0(pfad, "KMK024_Aus_Kurse_2022.xlsx"), "SL", 2022, "Grundkurse")
data22_GK_SN <- read_data(paste0(pfad, "KMK024_Aus_Kurse_2022.xlsx"), "SN", 2022, "Grundkurse")
data22_GK_ST <- read_data(paste0(pfad, "KMK024_Aus_Kurse_2022.xlsx"), "ST", 2022, "Grundkurse")
data22_GK_SH <- read_data(paste0(pfad, "KMK024_Aus_Kurse_2022.xlsx"), "SH", 2022, "Grundkurse")
data22_GK_TH <- read_data(paste0(pfad, "KMK024_Aus_Kurse_2022.xlsx"), "TH", 2022, "Grundkurse")

# daten für 2023

data23_LK_D  <- read_data(paste0(pfad,"KNK025_Aus_Kurse_2023.xlsx"), "D" , 2023, "Leistungskurse")
data23_LK_BW <- read_data(paste0(pfad,"KNK025_Aus_Kurse_2023.xlsx"), "BW", 2023, "Leistungskurse")
data23_LK_BY <- read_data(paste0(pfad,"KNK025_Aus_Kurse_2023.xlsx"), "BY", 2023, "Leistungskurse")
data23_LK_BE <- read_data(paste0(pfad,"KNK025_Aus_Kurse_2023.xlsx"), "BE", 2023, "Leistungskurse")
data23_LK_BB <- read_data(paste0(pfad,"KNK025_Aus_Kurse_2023.xlsx"), "BB", 2023, "Leistungskurse")
data23_LK_HB <- read_data(paste0(pfad,"KNK025_Aus_Kurse_2023.xlsx"), "HB", 2023, "Leistungskurse")
data23_LK_HH <- read_data(paste0(pfad,"KNK025_Aus_Kurse_2023.xlsx"), "HH", 2023, "Leistungskurse")
data23_LK_HE <- read_data(paste0(pfad,"KNK025_Aus_Kurse_2023.xlsx"), "HE", 2023, "Leistungskurse")
data23_LK_MV <- read_data(paste0(pfad,"KNK025_Aus_Kurse_2023.xlsx"), "MV", 2023, "Leistungskurse")
data23_LK_NI <- read_data(paste0(pfad,"KNK025_Aus_Kurse_2023.xlsx"), "NI", 2023, "Leistungskurse")
data23_LK_NW <- read_data(paste0(pfad,"KNK025_Aus_Kurse_2023.xlsx"), "NW", 2023, "Leistungskurse")
data23_LK_RP <- read_data(paste0(pfad,"KNK025_Aus_Kurse_2023.xlsx"), "RP", 2023, "Leistungskurse")
data23_LK_SL <- read_data(paste0(pfad,"KNK025_Aus_Kurse_2023.xlsx"), "SL", 2023, "Leistungskurse")
data23_LK_SN <- read_data(paste0(pfad,"KNK025_Aus_Kurse_2023.xlsx"), "SN", 2023, "Leistungskurse")
data23_LK_ST <- read_data(paste0(pfad,"KNK025_Aus_Kurse_2023.xlsx"), "ST", 2023, "Leistungskurse")
data23_LK_SH <- read_data(paste0(pfad,"KNK025_Aus_Kurse_2023.xlsx"), "SH", 2023, "Leistungskurse")
data23_LK_TH <- read_data(paste0(pfad,"KNK025_Aus_Kurse_2023.xlsx"), "TH", 2023, "Leistungskurse")
data23_GK_D  <- read_data(paste0(pfad, "KNK025_Aus_Kurse_2023.xlsx"), "D" , 2023, "Grundkurse")
data23_GK_BW <- read_data(paste0(pfad, "KNK025_Aus_Kurse_2023.xlsx"), "BW", 2023, "Grundkurse")
data23_GK_BY <- read_data(paste0(pfad, "KNK025_Aus_Kurse_2023.xlsx"), "BY", 2023, "Grundkurse")
data23_GK_BE <- read_data(paste0(pfad, "KNK025_Aus_Kurse_2023.xlsx"), "BE", 2023, "Grundkurse")
data23_GK_BB <- read_data(paste0(pfad, "KNK025_Aus_Kurse_2023.xlsx"), "BB", 2023, "Grundkurse")
data23_GK_HB <- read_data(paste0(pfad, "KNK025_Aus_Kurse_2023.xlsx"), "HB", 2023, "Grundkurse")
data23_GK_HH <- read_data(paste0(pfad, "KNK025_Aus_Kurse_2023.xlsx"), "HH", 2023, "Grundkurse")
data23_GK_HE <- read_data(paste0(pfad, "KNK025_Aus_Kurse_2023.xlsx"), "HE", 2023, "Grundkurse")
data23_GK_MV <- read_data(paste0(pfad, "KNK025_Aus_Kurse_2023.xlsx"), "MV", 2023, "Grundkurse")
data23_GK_NI <- read_data(paste0(pfad, "KNK025_Aus_Kurse_2023.xlsx"), "NI", 2023, "Grundkurse")
data23_GK_NW <- read_data(paste0(pfad, "KNK025_Aus_Kurse_2023.xlsx"), "NW", 2023, "Grundkurse")
data23_GK_RP <- read_data(paste0(pfad, "KNK025_Aus_Kurse_2023.xlsx"), "RP", 2023, "Grundkurse")
data23_GK_SL <- read_data(paste0(pfad, "KNK025_Aus_Kurse_2023.xlsx"), "SL", 2023, "Grundkurse")
data23_GK_SN <- read_data(paste0(pfad, "KNK025_Aus_Kurse_2023.xlsx"), "SN", 2023, "Grundkurse")
data23_GK_ST <- read_data(paste0(pfad, "KNK025_Aus_Kurse_2023.xlsx"), "ST", 2023, "Grundkurse")
data23_GK_SH <- read_data(paste0(pfad, "KNK025_Aus_Kurse_2023.xlsx"), "SH", 2023, "Grundkurse")
data23_GK_TH <- read_data(paste0(pfad, "KNK025_Aus_Kurse_2023.xlsx"), "TH", 2023, "Grundkurse")



# Zwischen-DFs zusammenfassen
data <- rbind( data_LK_D     ,
                   data_LK_BW ,
                   data_LK_BY ,
                   data_LK_BE ,
                   data_LK_BB ,
                   data_LK_HB ,
                   data_LK_HH ,
                   data_LK_HE ,
                   data_LK_MV ,
                   data_LK_NI ,
                   data_LK_NW ,
                   data_LK_RP ,
                   data_LK_SL ,
                   data_LK_SN ,
                   data_LK_ST ,
                   data_LK_SH ,
                   data_LK_TH ,
                   data_GK_D  ,
                   data_GK_BW ,
                   data_GK_BY ,
                   data_GK_BE ,
                   data_GK_BB ,
                   data_GK_HB ,
                   data_GK_HH ,
                   data_GK_HE ,
                   data_GK_MV ,
                   data_GK_NI ,
                   data_GK_NW ,
                   data_GK_RP ,
                   data_GK_SL ,
                   data_GK_SN ,
                   data_GK_ST ,
                   data_GK_SH ,
                   data_GK_TH
)

data22 <- rbind(data22_LK_D     ,
                data22_LK_BW ,
                data22_LK_BY ,
                data22_LK_BE ,
                data22_LK_BB ,
                data22_LK_HB ,
                data22_LK_HH ,
                data22_LK_HE ,
                data22_LK_MV ,
                data22_LK_NI ,
                data22_LK_NW ,
                data22_LK_RP ,
                data22_LK_SL ,
                data22_LK_SN ,
                data22_LK_ST ,
                data22_LK_SH ,
                data22_LK_TH ,
                data22_GK_D  ,
                data22_GK_BW ,
                data22_GK_BY ,
                data22_GK_BE ,
                data22_GK_BB ,
                data22_GK_HB ,
                data22_GK_HH ,
                data22_GK_HE ,
                data22_GK_MV ,
                data22_GK_NI ,
                data22_GK_NW ,
                data22_GK_RP ,
                data22_GK_SL ,
                data22_GK_SN ,
                data22_GK_ST ,
                data22_GK_SH ,
                data22_GK_TH)


data <- rbind(data, data22)

#nur neues Jahr:
data <- rbind(data23_LK_D     ,
              data23_LK_BW ,
              data23_LK_BY ,
              data23_LK_BE ,
              data23_LK_BB ,
              data23_LK_HB ,
              data23_LK_HH ,
              data23_LK_HE ,
              data23_LK_MV ,
              data23_LK_NI ,
              data23_LK_NW ,
              data23_LK_RP ,
              data23_LK_SL ,
              data23_LK_SN ,
              data23_LK_ST ,
              data23_LK_SH ,
              data23_LK_TH ,
              data23_GK_D  ,
              data23_GK_BW ,
              data23_GK_BY ,
              data23_GK_BE ,
              data23_GK_BB ,
              data23_GK_HB ,
              data23_GK_HH ,
              data23_GK_HE ,
              data23_GK_MV ,
              data23_GK_NI ,
              data23_GK_NW ,
              data23_GK_RP ,
              data23_GK_SL ,
              data23_GK_SN ,
              data23_GK_ST ,
              data23_GK_SH ,
              data23_GK_TH)


### 2024 ----

data_LK_D  <- read_data(paste0(pfad,"KMK026_Aus_Kurse_2024.xlsx"), "D" , 2024, "Leistungskurse")
data_LK_BW <- read_data(paste0(pfad,"KMK026_Aus_Kurse_2024.xlsx"), "BW", 2024, "Leistungskurse")
data_LK_BY <- read_data(paste0(pfad,"KMK026_Aus_Kurse_2024.xlsx"), "BY", 2024, "Leistungskurse")
data_LK_BE <- read_data(paste0(pfad,"KMK026_Aus_Kurse_2024.xlsx"), "BE", 2024, "Leistungskurse")
data_LK_BB <- read_data(paste0(pfad,"KMK026_Aus_Kurse_2024.xlsx"), "BB", 2024, "Leistungskurse")
data_LK_HB <- read_data(paste0(pfad,"KMK026_Aus_Kurse_2024.xlsx"), "HB", 2024, "Leistungskurse")
data_LK_HH <- read_data(paste0(pfad,"KMK026_Aus_Kurse_2024.xlsx"), "HH", 2024, "Leistungskurse")
data_LK_HE <- read_data(paste0(pfad,"KMK026_Aus_Kurse_2024.xlsx"), "HE", 2024, "Leistungskurse")
data_LK_MV <- read_data(paste0(pfad,"KMK026_Aus_Kurse_2024.xlsx"), "MV", 2024, "Leistungskurse")
data_LK_NI <- read_data(paste0(pfad,"KMK026_Aus_Kurse_2024.xlsx"), "NI", 2024, "Leistungskurse")
data_LK_NW <- read_data(paste0(pfad,"KMK026_Aus_Kurse_2024.xlsx"), "NW", 2024, "Leistungskurse")
data_LK_RP <- read_data(paste0(pfad,"KMK026_Aus_Kurse_2024.xlsx"), "RP", 2024, "Leistungskurse")
data_LK_SL <- read_data(paste0(pfad,"KMK026_Aus_Kurse_2024.xlsx"), "SL", 2024, "Leistungskurse")
data_LK_SN <- read_data(paste0(pfad,"KMK026_Aus_Kurse_2024.xlsx"), "SN", 2024, "Leistungskurse")
data_LK_ST <- read_data(paste0(pfad,"KMK026_Aus_Kurse_2024.xlsx"), "ST", 2024, "Leistungskurse")
data_LK_SH <- read_data(paste0(pfad,"KMK026_Aus_Kurse_2024.xlsx"), "SH", 2024, "Leistungskurse")
data_LK_TH <- read_data(paste0(pfad,"KMK026_Aus_Kurse_2024.xlsx"), "TH", 2024, "Leistungskurse")

data_GK_D  <- read_data(paste0(pfad, "KMK026_Aus_Kurse_2024.xlsx"), "D" , 2024, "Grundkurse")
data_GK_BW <- read_data(paste0(pfad, "KMK026_Aus_Kurse_2024.xlsx"), "BW", 2024, "Grundkurse")
data_GK_BY <- read_data(paste0(pfad, "KMK026_Aus_Kurse_2024.xlsx"), "BY", 2024, "Grundkurse")
data_GK_BE <- read_data(paste0(pfad, "KMK026_Aus_Kurse_2024.xlsx"), "BE", 2024, "Grundkurse")
data_GK_BB <- read_data(paste0(pfad, "KMK026_Aus_Kurse_2024.xlsx"), "BB", 2024, "Grundkurse")
data_GK_HB <- read_data(paste0(pfad, "KMK026_Aus_Kurse_2024.xlsx"), "HB", 2024, "Grundkurse")
data_GK_HH <- read_data(paste0(pfad, "KMK026_Aus_Kurse_2024.xlsx"), "HH", 2024, "Grundkurse")
data_GK_HE <- read_data(paste0(pfad, "KMK026_Aus_Kurse_2024.xlsx"), "HE", 2024, "Grundkurse")
data_GK_MV <- read_data(paste0(pfad, "KMK026_Aus_Kurse_2024.xlsx"), "MV", 2024, "Grundkurse")
data_GK_NI <- read_data(paste0(pfad, "KMK026_Aus_Kurse_2024.xlsx"), "NI", 2024, "Grundkurse")
data_GK_NW <- read_data(paste0(pfad, "KMK026_Aus_Kurse_2024.xlsx"), "NW", 2024, "Grundkurse")
data_GK_RP <- read_data(paste0(pfad, "KMK026_Aus_Kurse_2024.xlsx"), "RP", 2024, "Grundkurse")
data_GK_SL <- read_data(paste0(pfad, "KMK026_Aus_Kurse_2024.xlsx"), "SL", 2024, "Grundkurse")
data_GK_SN <- read_data(paste0(pfad, "KMK026_Aus_Kurse_2024.xlsx"), "SN", 2024, "Grundkurse")
data_GK_ST <- read_data(paste0(pfad, "KMK026_Aus_Kurse_2024.xlsx"), "ST", 2024, "Grundkurse")
data_GK_SH <- read_data(paste0(pfad, "KMK026_Aus_Kurse_2024.xlsx"), "SH", 2024, "Grundkurse")
data_GK_TH <- read_data(paste0(pfad, "KMK026_Aus_Kurse_2024.xlsx"), "TH", 2024, "Grundkurse")

data <- rbind( data_LK_D     ,
               data_LK_BW ,
               data_LK_BY ,
               data_LK_BE ,
               data_LK_BB ,
               data_LK_HB ,
               data_LK_HH ,
               data_LK_HE ,
               data_LK_MV ,
               data_LK_NI ,
               data_LK_NW ,
               data_LK_RP ,
               data_LK_SL ,
               data_LK_SN ,
               data_LK_ST ,
               data_LK_SH ,
               data_LK_TH ,
               data_GK_D  ,
               data_GK_BW ,
               data_GK_BY ,
               data_GK_BE ,
               data_GK_BB ,
               data_GK_HB ,
               data_GK_HH ,
               data_GK_HE ,
               data_GK_MV ,
               data_GK_NI ,
               data_GK_NW ,
               data_GK_RP ,
               data_GK_SL ,
               data_GK_SN ,
               data_GK_ST ,
               data_GK_SH ,
               data_GK_TH
)

## Datensatz in gewünschte From bringe -------------------------------------

# Bundesländer benennen
data <- data %>%
  dplyr::mutate(region=dplyr::case_when(
    region== "D"  ~   "Deutschland",
    region== "SH"  ~   "Schleswig-Holstein",
    region== "HH" ~   "Hamburg",
    region== "NI" ~   "Niedersachsen",
    region== "HB" ~   "Bremen",
    region== "NW" ~   "Nordrhein-Westfalen",
    region== "HE" ~   "Hessen",
    region== "RP" ~   "Rheinland-Pfalz",
    region== "BW" ~   "Baden-Württemberg",
    region== "BY"  ~  "Bayern",
    region== "SL" ~  "Saarland",
    region== "BE" ~ "Berlin",
    region== "BB" ~ "Brandenburg",
    region== "MV" ~ "Mecklenburg-Vorpommern",
    region== "SN" ~ "Sachsen",
    region== "ST" ~ "Sachsen-Anhalt",
    region== "TH" ~ "Thüringen",
    T  ~ "Fehler"
  ))

# fehlende Kategorien in fachbereich ergänzen

data <- data %>%
  tidyr::pivot_wider(names_from = fachbereich, values_from = wert)%>%
  dplyr::mutate("Musik/Kunst" = Musik + `Kunst/Gestaltung/Werken`,
         "Religion/Ethik" = `Religion, ev.`+ `Religion, kath.` + `Ethik/Philosophie`,
         "MINT" = Mathematik + Informatik + Biologie + Chemie + Physik+ `andere naturwiss.-technische Fächer`,
         "Nicht MINT"= `Alle Fächer` - MINT
         # "Alle Fächer" = `Alle Fächer` - `andere naturwiss.-technische Fächer`,
  ) %>%
  dplyr::select(-Englisch,- Französisch,-`andere moderne Fremdsprachen`,- `Latein und andere antike Sprachen`, -Naturwissenschaften,
         -Erdkunde, -Geschichte, -`Sozialkunde/Gesellschaftslehre/Politik`, -`Wirtschaft/Verwaltung/Recht`, -`Psychologie, Pädagogik`,
         -Musik, - `Kunst/Gestaltung/Werken`, - `Religion, ev.`,
         - `Religion, kath.`, - `Ethik/Philosophie`,- `Sonstige Fächer`)%>%
  dplyr::transmute(Jahr ,
            region,
            indikator,
            anzeige_geschlecht ,
            quelle,
            hinweise,
            bereich,
            `Alle Fächer`,
            Mathematik,
            Informatik,
            Biologie,
            Chemie,
            Physik,
            MINT,
            `Nicht MINT`,
            Deutsch,
            Fremdsprachen,
            Gesellschaftswissenschaften,
            `Musik/Kunst`,
            `Religion/Ethik`,
            Sport,
            `andere naturwiss.-technische Fächer`)

data <- data %>%
  tidyr::pivot_longer(cols = "Alle Fächer":"andere naturwiss.-technische Fächer") %>%
  dplyr::rename(fachbereich = name,
         wert = value)

#NA definieren anstelle 0 bei GK Bayern
data$wert <- ifelse(data$region=="Bayern"&data$indikator=="Leistungskurse"&data$wert==0, NA, data$wert)


#Spalten in logische Reihenfolge bringen
data <- data[,c("bereich", "hinweise", "quelle", "indikator", "fachbereich", "anzeige_geschlecht",
                    "region", "Jahr", "wert")]
data <- data %>%
  rename(jahr = Jahr)

# Oberstufenbelegungen hinzufügen
data_o <- data %>%
  group_by(bereich, hinweise, quelle, jahr, region, anzeige_geschlecht, fachbereich) %>%
  summarise(wert = sum(wert, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(indikator = "Oberstufenbelegungen",
         wert = case_when(
           wert == 0 ~ NA,
           T ~ wert
         ))
data <- rbind(data, data_o)

#exportieren
kurse <- data %>%
  janitor::clean_names() %>%
  janitor::remove_empty() %>%
  dplyr::select(-c("quelle", "hinweise")) %>%
  dplyr::filter(!fachbereich %in% c("Nicht MINT", "MINT"))

kurse <- kurse %>% dplyr::filter(jahr >= 2010)

#kurse <- kurse %>% dplyr::filter(region != "Baden-Württemberg")

kurse[kurse$region == "Rheinland-Pflaz", "region"] <- "Rheinland-Pfalz"

kurse[kurse$anzeige_geschlecht == "frauen", "anzeige_geschlecht"] <- "Frauen"
kurse[kurse$anzeige_geschlecht == "gesamt", "anzeige_geschlecht"] <- "Gesamt"
kurse[kurse$anzeige_geschlecht == "männer", "anzeige_geschlecht"] <- "Männer"

## Prep fuktionen übernehmen ----

states_east_west <- list(west = c("Baden-Württemberg", "Bayern", "Bremen", "Hamburg",
                                  "Hessen", "Niedersachsen", "Nordrhein-Westfalen",
                                  "Rheinland-Pfalz", "Saarland", "Schleswig-Holstein"),
                         east = c("Brandenburg", "Mecklenburg-Vorpommern", "Sachsen",
                                  "Sachsen-Anhalt", "Thüringen", "Berlin"))

# prep_kurse_east_west

df_incl <- kurse

# create dummy variable to indicate east or west
df_incl$dummy_west <- ifelse(df_incl$region %in% states_east_west$west & df_incl$region != "Deutschland", "Westdeutschland (o. Berlin)", NA)
df_incl$dummy_west <- ifelse(df_incl$region %in% states_east_west$east & df_incl$region != "Deutschland", "Ostdeutschland (inkl. Berlin)", df_incl$dummy_west)
df_incl <- na.omit(df_incl)# ifelse erstellt nochmal DE mit "NA" als region-Namen -->löschen

# aggregate values
df_incl <- df_incl %>% dplyr::group_by(jahr, anzeige_geschlecht, indikator, fachbereich, dummy_west, bereich) %>%
  dplyr::summarise(wert = sum(wert, na.rm = T))

names(df_incl)[5] <- "region"


  df_incl <- df_incl[, colnames(kurse)]

  kurse <- rbind(kurse, df_incl)


# objekt zum filtern, pivoten aller fächer erstellen
alle_kurse <- kurse %>%
  dplyr::select(fachbereich)%>%
  unique%>%
  unlist() %>%
  as.vector%>%
  append("MINT")%>%
  append("andere Fächer")


kurse <- kurse %>%
  tidyr::pivot_wider(names_from = fachbereich, values_from = wert) %>%
  dplyr::mutate(MINT = rowSums(select(.,"Mathematik", "Informatik",  "Biologie", "Chemie",
                                      "Physik", "andere naturwiss.-technische Fächer" ), na.rm =T ))%>%
  dplyr::mutate("andere Fächer" = `Alle Fächer`- MINT) %>%
  tidyr::pivot_longer(all_of(alle_kurse), values_to = "wert", names_to = "fachbereich")

kurse <- kurse %>%
  mutate(wert = case_when(
    wert == 0 ~NA,
    T ~ wert
  ))

## Datensatz zusammenfügen, anpassen und exportieren --------------------------------------


library(DBI)
con <- dbConnect(duckdb::duckdb(), "data/mint_db.duckdb")
data_z <- dbGetQuery(con, "SELECT * FROM kurse")
dbDisconnect(con, shutdown = TRUE)

#neue Daten anhängen
kurse <- rbind(data_z, kurse)

save(kurse, file="kurse.rda")


# Erstellt "iqb" -------------------------------------------------

#
# Data Lab
# Vorbereitung Datensatz: IQB Daten 4. Klasse (2011, 2016, 2021)
# Author: Katharina Brunner, April 2023
#

library(dplyr)


## iqb_standard Teil -------------------------------------------------------

### Daten ergänzen an db-Dataframe ----

library(DBI)
library(dplyr)

setwd("~/datalab2")
con <- dbConnect(duckdb::duckdb(), "data/mint_db.duckdb")
iqb <- dbGetQuery(con, "SELECT * FROM iqb")

akro <- "kbr"
pfad <- paste0("C:/Users/", akro,
               "/OneDrive - Stifterverband/MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/02 Datenmaterial/01_Rohdaten/02_Alle Daten/")

## Mindeststandard

#pfad
m_ms <- readxl::read_xlsx(path = paste0(pfad, "IQB022_Bildungtrend_2024_Zusatzmaterial_Tabellen_251114.xlsx"),
                  sheet = "Tab. 3.31web")
m_ms <- m_ms[,c(1,2,3,6,9)]
m_ms <- m_ms[-1*(1:4),]

m_ms <- m_ms %>%
  rename("land" = "zurück zum Inhaltsverzeichnis",
         "2012" = "...3",
         "2018" = "...6",
         "indikator" = "...2",
         "2024" = "...9") %>%
  tidyr::fill(land, .direction = "downup") %>%
  # mutate(wert = round(as.numeric(wert), 2)) %>%
  na.omit()

m_ms <- m_ms %>%
  tidyr::pivot_longer(cols = c("2012", "2018", "2024"))

m_ms <- m_ms %>%
  mutate(
    bereich = "Schule",
    typ = "standard",
    klasse = "9. Klasse",
    fach = "Mathematik",
    geschlecht = "gesamt",
    indikator = case_when(
      indikator == "MS (MSA) verfehlt" ~ "Mindeststandard nicht erreicht",
      indikator == "RS (MSA) erreicht" ~ "Regelstandard erreicht",
      indikator == "OS (MSA) erreicht" ~ "Optimalstandard erreicht"
    )
  ) %>%
  rename("region" = "land",
         "jahr" = "name",
         "wert" = "value") %>%
  mutate(wert = round(as.numeric(wert), 2)) %>%
  na.omit()

m_ms <- m_ms[, c("bereich", "typ", "klasse", "fach", "indikator", "geschlecht",
                 "region", "jahr", "wert")]

b_ms <- readxl::read_xlsx(path = paste0(pfad, "IQB022_Bildungtrend_2024_Zusatzmaterial_Tabellen_251114.xlsx"),
                          sheet = "Tab. 3.67web")
b_ms <- b_ms[,c(1,2, 3, 6,9)]
b_ms <- b_ms[-1*(1:5),]
b_ms <- b_ms %>%
  rename("land" = "zurück zum Inhaltsverzeichnis",
         "2012" = "...3",
         "2018" = "...6",
         "indikator" = "...2",
         "2024" = "...9") %>%
  tidyr::fill(land, .direction = "downup") %>%
 # mutate(wert = round(as.numeric(wert), 2)) %>%
  na.omit()

b_ms <- b_ms %>%
  tidyr::pivot_longer(cols = c("2012", "2018", "2024"))

b_ms <- b_ms %>%
  mutate(
    bereich = "Schule",
    typ = "standard",
    klasse = "9. Klasse",
    fach = "Biologie (Fachwissen)",
    geschlecht = "gesamt",
    indikator = case_when(
      indikator == "MS (MSA) verfehlt" ~ "Mindeststandard nicht erreicht",
      indikator == "RS (MSA) erreicht" ~ "Regelstandard erreicht",
      indikator == "OS (MSA) erreicht" ~ "Optimalstandard erreicht"
    )
  ) %>%
  rename("region" = "land",
         "jahr" = "name",
         "wert" = "value") %>%
  mutate(wert = round(as.numeric(wert), 2))

b_ms <- b_ms[, c("bereich", "typ", "klasse", "fach", "indikator", "geschlecht",
                 "region", "jahr", "wert")]


c_ms <- readxl::read_xlsx(path = paste0(pfad, "IQB022_Bildungtrend_2024_Zusatzmaterial_Tabellen_251114.xlsx"),
                          sheet = "Tab. 3.69web")
c_ms <- c_ms[,c(1,2, 3, 6,9)]
c_ms <- c_ms[-1*(1:5),]
c_ms <- c_ms %>%
  rename("land" = "zurück zum Inhaltsverzeichnis",
         "2012" = "...3",
         "2018" = "...6",
         "indikator" = "...2",
         "2024" = "...9") %>%
  tidyr::fill(land, .direction = "downup") %>%
  # mutate(wert = round(as.numeric(wert), 2)) %>%
  na.omit()

c_ms <- c_ms %>%
  tidyr::pivot_longer(cols = c("2012", "2018", "2024"))

c_ms <- c_ms %>%
  mutate(
    bereich = "Schule",
    typ = "standard",
    klasse = "9. Klasse",
    fach = "Chemie (Fachwissen)",
    geschlecht = "gesamt",
    indikator = case_when(
      indikator == "MS (MSA) verfehlt" ~ "Mindeststandard nicht erreicht",
      indikator == "RS (MSA) erreicht" ~ "Regelstandard erreicht",
      indikator == "OS (MSA) erreicht" ~ "Optimalstandard erreicht"
    )
  ) %>%
  rename("region" = "land",
         "jahr" = "name",
         "wert" = "value") %>%
  mutate(wert = round(as.numeric(wert), 2))

c_ms <- c_ms[, c("bereich", "typ", "klasse", "fach", "indikator", "geschlecht",
                 "region", "jahr", "wert")]


p_ms <- readxl::read_xlsx(path = paste0(pfad, "IQB022_Bildungtrend_2024_Zusatzmaterial_Tabellen_251114.xlsx"),
                          sheet = "Tab. 3.71web")
p_ms <- p_ms[,c(1,2, 3, 6,9)]
p_ms <- p_ms[-1*(1:5),]
p_ms <- p_ms %>%
  rename("land" = "zurück zum Inhaltsverzeichnis",
         "2012" = "...3",
         "2018" = "...6",
         "indikator" = "...2",
         "2024" = "...9") %>%
  tidyr::fill(land, .direction = "downup") %>%
  # mutate(wert = round(as.numeric(wert), 2)) %>%
  na.omit()

p_ms <- p_ms %>%
  tidyr::pivot_longer(cols = c("2012", "2018", "2024"))

p_ms <- p_ms %>%
  mutate(
    bereich = "Schule",
    typ = "standard",
    klasse = "9. Klasse",
    fach = "Phsik (Fachwissen)",
    geschlecht = "gesamt",
    indikator = case_when(
      indikator == "MS (MSA) verfehlt" ~ "Mindeststandard nicht erreicht",
      indikator == "RS (MSA) erreicht" ~ "Regelstandard erreicht",
      indikator == "OS (MSA) erreicht" ~ "Optimalstandard erreicht"
    )
  ) %>%
  rename("region" = "land",
         "jahr" = "name",
         "wert" = "value") %>%
  mutate(wert = round(as.numeric(wert), 2))

p_ms <- p_ms[, c("bereich", "typ", "klasse", "fach", "indikator", "geschlecht",
                 "region", "jahr", "wert")]

## Testwerte nach Geschlecht

m_gs <- readxl::read_xlsx(path = paste0(pfad, "IQB023_BT 2024 geschlecht.xlsx"),
                          sheet = "mathe")

m_gs <- m_gs %>%
  tidyr::pivot_longer(cols = c("2012", "2018", "2024"))

m_gs <- m_gs %>%
  mutate(
    bereich = "Schule",
    typ = "score",
    klasse = "9. Klasse",
    fach = "Mathematik",
    indikator = "Alle",
    geschlecht = case_when(
      geschlecht == "Jungen" ~ "Männer",
      geschlecht == "Mädchen" ~ "Frauen"
    )
  ) %>%
  rename(
         "jahr" = "name",
         "wert" = "value") %>%
  mutate(wert = round(as.numeric(wert), 2))

m_gs <- m_gs[, c("bereich", "typ", "klasse", "fach", "indikator", "geschlecht",
                 "region", "jahr", "wert")]

b_gs <- readxl::read_xlsx(path = paste0(pfad, "IQB023_BT 2024 geschlecht.xlsx"),
                          sheet = "bio")

b_gs <- b_gs %>%
  tidyr::pivot_longer(cols = c("2012", "2018", "2024"))

b_gs <- b_gs %>%
  mutate(
    bereich = "Schule",
    typ = "score",
    klasse = "9. Klasse",
    fach = "Biologie (Fachwissen)",
    indikator = "Alle",
    geschlecht = case_when(
      geschlecht == "Jungen" ~ "Männer",
      geschlecht == "Mädchen" ~ "Frauen"
    )
  ) %>%
  rename(
    "jahr" = "name",
    "wert" = "value") %>%
  mutate(wert = round(as.numeric(wert), 2))

b_gs <- b_gs[, c("bereich", "typ", "klasse", "fach", "indikator", "geschlecht",
                 "region", "jahr", "wert")]

c_gs <- readxl::read_xlsx(path = paste0(pfad, "IQB023_BT 2024 geschlecht.xlsx"),
                          sheet = "chemie")

c_gs <- c_gs %>%
  tidyr::pivot_longer(cols = c("2012", "2018", "2024"))

c_gs <- c_gs %>%
  mutate(
    bereich = "Schule",
    typ = "score",
    klasse = "9. Klasse",
    fach = "Chemie (Fachwissen)",
    indikator = "Alle",
    geschlecht = case_when(
      geschlecht == "Jungen" ~ "Männer",
      geschlecht == "Mädchen" ~ "Frauen"
    )
  ) %>%
  rename(
    "jahr" = "name",
    "wert" = "value") %>%
  mutate(wert = round(as.numeric(wert), 2))

c_gs <- c_gs[, c("bereich", "typ", "klasse", "fach", "indikator", "geschlecht",
                 "region", "jahr", "wert")]


p_gs <- readxl::read_xlsx(path = paste0(pfad, "IQB023_BT 2024 geschlecht.xlsx"),
                          sheet = "physik")

p_gs <- p_gs %>%
  tidyr::pivot_longer(cols = c("2012", "2018", "2024"))

p_gs <- p_gs %>%
  mutate(
    bereich = "Schule",
    typ = "score",
    klasse = "9. Klasse",
    fach = "Physik (Fachwissen)",
    indikator = "Alle",
    geschlecht = case_when(
      geschlecht == "Jungen" ~ "Männer",
      geschlecht == "Mädchen" ~ "Frauen"
    )
  ) %>%
  rename(
    "jahr" = "name",
    "wert" = "value") %>%
  mutate(wert = round(as.numeric(wert), 2))

p_gs <- p_gs[, c("bereich", "typ", "klasse", "fach", "indikator", "geschlecht",
                 "region", "jahr", "wert")]

# Wert nach Status und Zuwanderung

m_migra <- readxl::read_xlsx(path = paste0(pfad, "IQB022_Bildungtrend_2024_Zusatzmaterial_Tabellen_251114.xlsx"),
                          sheet = "Tab. 7.4web")
m_migra <- m_migra[,c(1,2, 3, 7, 11)]

m_migra <- m_migra %>%
  rename("region" = "zurück zum Inhaltsverzeichnis",
         "2012" = "...3",
         "2018" = "...7",
         "indikator" = "...2",
         "2024" = "...11") %>%
  tidyr::fill(region, .direction = "downup") %>%
  # mutate(wert = round(as.numeric(wert), 2)) %>%
  na.omit()

m_migra$region <- gsub("[0-9,]", "", m_migra$region)

m_migra <- m_migra %>%
  tidyr::pivot_longer(cols = c("2012", "2018", "2024"))

m_migra <- m_migra %>%
  mutate(
    bereich = "Schule",
    typ = "score",
    klasse = "9. Klasse",
    fach = "Mathematik",
    geschlecht = "gesamt",
    indikator = case_when(
      indikator == "mit Zuwanderungshintergrund" ~ "mit Zuwanderungsgeschichte",
      indikator == "ohne Zuwanderungshintergrund" ~ "ohne Zuwanderungsgeschichte"
    )
  ) %>%
  rename(
         "jahr" = "name",
         "wert" = "value") %>%
  mutate(wert = round(as.numeric(wert), 2))

m_migra <- m_migra[, c("bereich", "typ", "klasse", "fach", "indikator", "geschlecht",
                 "region", "jahr", "wert")]


b_migra <- readxl::read_xlsx(path = paste0(pfad, "IQB022_Bildungtrend_2024_Zusatzmaterial_Tabellen_251114.xlsx"),
                             sheet = "Tab. 7.5web")
b_migra <- b_migra[,c(1,2, 3, 7, 11)]

b_migra <- b_migra %>%
  rename("region" = "zurück zum Inhaltsverzeichnis",
         "2012" = "...3",
         "2018" = "...7",
         "indikator" = "...2",
         "2024" = "...11") %>%
  tidyr::fill(region, .direction = "downup") %>%
  # mutate(wert = round(as.numeric(wert), 2)) %>%
  na.omit()

b_migra$region <- gsub("[0-9,]", "", b_migra$region)

b_migra <- b_migra %>%
  tidyr::pivot_longer(cols = c("2012", "2018", "2024"))

b_migra <- b_migra %>%
  mutate(
    bereich = "Schule",
    typ = "score",
    klasse = "9. Klasse",
    fach = "Biologie (Fachwissen)",
    geschlecht = "gesamt",
    indikator = case_when(
      indikator == "mit Zuwanderungshintergrund" ~ "mit Zuwanderungsgeschichte",
      indikator == "ohne Zuwanderungshintergrund" ~ "ohne Zuwanderungsgeschichte"
    )
  ) %>%
  rename(
    "jahr" = "name",
    "wert" = "value") %>%
  mutate(wert = round(as.numeric(wert), 2))

b_migra <- b_migra[, c("bereich", "typ", "klasse", "fach", "indikator", "geschlecht",
                       "region", "jahr", "wert")]

c_migra <- readxl::read_xlsx(path = paste0(pfad, "IQB022_Bildungtrend_2024_Zusatzmaterial_Tabellen_251114.xlsx"),
                             sheet = "Tab. 7.7web")
c_migra <- c_migra[,c(1,2, 3, 7, 11)]

c_migra <- c_migra %>%
  rename("region" = "zurück zum Inhaltsverzeichnis",
         "2012" = "...3",
         "2018" = "...7",
         "indikator" = "...2",
         "2024" = "...11") %>%
  tidyr::fill(region, .direction = "downup") %>%
  # mutate(wert = round(as.numeric(wert), 2)) %>%
  na.omit()

c_migra$region <- gsub("[0-9,]", "", c_migra$region)

c_migra <- c_migra %>%
  tidyr::pivot_longer(cols = c("2012", "2018", "2024"))

c_migra <- c_migra %>%
  mutate(
    bereich = "Schule",
    typ = "score",
    klasse = "9. Klasse",
    fach = "Chemie (Fachwissen)",
    geschlecht = "gesamt",
    indikator = case_when(
      indikator == "mit Zuwanderungshintergrund" ~ "mit Zuwanderungsgeschichte",
      indikator == "ohne Zuwanderungshintergrund" ~ "ohne Zuwanderungsgeschichte"
    )
  ) %>%
  rename(
    "jahr" = "name",
    "wert" = "value") %>%
  mutate(wert = round(as.numeric(wert), 2))

c_migra <- c_migra[, c("bereich", "typ", "klasse", "fach", "indikator", "geschlecht",
                       "region", "jahr", "wert")]

p_migra <- readxl::read_xlsx(path = paste0(pfad, "IQB022_Bildungtrend_2024_Zusatzmaterial_Tabellen_251114.xlsx"),
                             sheet = "Tab. 7.9web")
p_migra <- p_migra[,c(1,2, 3, 7, 11)]

p_migra <- p_migra %>%
  rename("region" = "zurück zum Inhaltsverzeichnis",
         "2012" = "...3",
         "2018" = "...7",
         "indikator" = "...2",
         "2024" = "...11") %>%
  tidyr::fill(region, .direction = "downup") %>%
  # mutate(wert = round(as.numeric(wert), 2)) %>%
  na.omit()

p_migra$region <- gsub("[0-9,]", "", p_migra$region)

p_migra <- p_migra %>%
  tidyr::pivot_longer(cols = c("2012", "2018", "2024"))

p_migra <- p_migra %>%
  mutate(
    bereich = "Schule",
    typ = "score",
    klasse = "9. Klasse",
    fach = "Physik (Fachwissen)",
    geschlecht = "gesamt",
    indikator = case_when(
      indikator == "mit Zuwanderungshintergrund" ~ "mit Zuwanderungsgeschichte",
      indikator == "ohne Zuwanderungshintergrund" ~ "ohne Zuwanderungsgeschichte"
    )
  ) %>%
  rename(
    "jahr" = "name",
    "wert" = "value") %>%
  mutate(wert = round(as.numeric(wert), 2))

p_migra <- p_migra[, c("bereich", "typ", "klasse", "fach", "indikator", "geschlecht",
                       "region", "jahr", "wert")]

## Status
status <- readxl::read_excel(path = paste0(pfad, "IQB024_BT 2024 status.xlsx"))

status <- status %>%
  mutate(
    bereich = "Schule",
    typ = "score",
    klasse = "9. Klasse",
    geschlecht = "gesamt"
  )

status <- status[, c("bereich", "typ", "klasse", "fach", "indikator", "geschlecht",
                       "region", "jahr", "wert")]

## Selbstkonzept und Interesse

# funktion
create_selbst_int <- function(df, bereichx, fachx){
  df <- df[,c(1,2, 7, 9, 11,  13, 15,  17)]
  df <- df[-1*1:4,]

  df <- df %>%
    rename("region" = "zurück zum Inhaltsverzeichnis",
           "geschlecht" = "...2",
           "2012_M" = "...7",
           "2012_SD" = "...9",
           "2018_M" = "...11",
           "2018_SD" = "...13",
           "2024_M" = "...15",
           "2024_SD" = "...17"
    ) %>%
    tidyr::fill(region, .direction = "downup") %>%
    # mutate(wert = round(as.numeric(wert), 2)) %>%
    na.omit()

  df$region <- gsub("[0-9,]", "", df$region)

  df <- df %>%
    tidyr::pivot_longer(cols = c("2012_M", "2012_SD", "2018_M", "2018_SD", "2024_M", "2024_SD"))

  df <- df %>%
    mutate(
      bereich = "Schule",
      klasse = "9. Klasse",
      indikator = bereichx,
      fach = fachx,
      jahr = case_when(
        grepl("2012", name) ~ 2012,
        grepl("2018", name) ~ 2018,
        grepl("2024", name) ~ 2024
      ),
      typ = case_when(
        grepl("M", name) ~ "Mittelwert",
        grepl("SD", name) ~ "Standardabweichung"
      )
    ) %>%
    rename(
      "wert" = "value") %>%
    mutate(wert = round(as.numeric(wert), 2)) %>%
    select(-name)

  df <- df[, c("bereich", "typ", "klasse", "fach", "indikator", "geschlecht",
                           "region", "jahr", "wert")]
}

# Mathe - Selbstkonzept
selbst_mm <- readxl::read_excel(path = paste0(pfad, "IQB022_Bildungtrend_2024_Zusatzmaterial_Tabellen_251114.xlsx"),
                               sheet = "Tab. 8.1web")

selbst_ma <- create_selbst_int(df = selbst_mm, fachx = "Mathematik", bereichx = "Selbstkonzept")


# Mathe - Interesse
int_ma <- readxl::read_excel(path = paste0(pfad, "IQB022_Bildungtrend_2024_Zusatzmaterial_Tabellen_251114.xlsx"),
                               sheet = "Tab. 8.2web")

int_ma <- create_selbst_int(df = int_ma, fachx = "Mathematik", bereichx = "Interesse")

# Bio - Selbstkonzept
selbst_b <- readxl::read_excel(path = paste0(pfad, "IQB022_Bildungtrend_2024_Zusatzmaterial_Tabellen_251114.xlsx"),
                                sheet = "Tab. 8.3web")

selbst_b <- create_selbst_int(df = selbst_b, fachx = "Biologie", bereichx = "Selbstkonzept")


# Bio - Interesse
int_b <- readxl::read_excel(path = paste0(pfad, "IQB022_Bildungtrend_2024_Zusatzmaterial_Tabellen_251114.xlsx"),
                           sheet = "Tab. 8.4web")

int_b <- create_selbst_int(df = int_b, fachx = "Biologie", bereichx = "Interesse")

# Chemie- Selbstkonzept
selbst_c <- readxl::read_excel(path = paste0(pfad, "IQB022_Bildungtrend_2024_Zusatzmaterial_Tabellen_251114.xlsx"),
                               sheet = "Tab. 8.5web")

selbst_c <- create_selbst_int(df = selbst_c, fachx = "Chemie", bereichx = "Selbstkonzept")


# Chemie - Interesse
int_c <- readxl::read_excel(path = paste0(pfad, "IQB022_Bildungtrend_2024_Zusatzmaterial_Tabellen_251114.xlsx"),
                            sheet = "Tab. 8.6web")

int_c <- create_selbst_int(df = int_c, fachx = "Chemie", bereichx = "Interesse")

# Physik- Selbstkonzept
selbst_p <- readxl::read_excel(path = paste0(pfad, "IQB022_Bildungtrend_2024_Zusatzmaterial_Tabellen_251114.xlsx"),
                               sheet = "Tab. 8.7web")

selbst_p <- create_selbst_int(df = selbst_p, fachx = "Physik", bereichx = "Selbstkonzept")


# Physik - Interesse
int_p <- readxl::read_excel(path = paste0(pfad, "IQB022_Bildungtrend_2024_Zusatzmaterial_Tabellen_251114.xlsx"),
                            sheet = "Tab. 8.8web")

int_p <- create_selbst_int(df = int_p, fachx = "Physik", bereichx = "Interesse")



## Zusammenfügen und aktualisieren in DB ----

iqb <- rbind(iqb,m_ms, b_ms, c_ms, p_ms, m_gs, b_gs, c_gs, p_gs,
             m_migra, b_migra, c_migra, p_migra, status,
             selbst_ma, int_ma, selbst_b, int_b, selbst_c, int_c,
             selbst_p, int_p)

iqb$fach[iqb$fach == "Biologie"] <- "Biologie (Fachwissen)"
iqb$fach[iqb$fach == "Chemie"] <- "Chemie (Fachwissen)"
iqb$fach[iqb$fach == "Physik"] <- "Physik (Fachwissen)"

dbWriteTable(con, 'iqb', iqb, overwrite = TRUE, append = FALSE)
save(iqb, file="iqb.rda")
dbDisconnect(con, shutdown=TRUE)

con <- dbConnect(duckdb::duckdb(), "data/mint_db.duckdb")
iqb <- dbGetQuery(con, "SELECT * FROM iqb")

# nur noch neue Zuwanderungs-Zahlen für 9. Kl, ums zusammen darstellen zu können
iqb <- iqb %>%
  filter(!(klasse == "9. Klasse" & indikator %in% c("mit Zuwanderungsgeschichte",
                                                    "ohne Zuwanderungsgeschichte",
                                                    "mit Zuwanderungsgeschichte (beide Elternteile)",
                                                    "mit Zuwanderungsgeschichte (ein Elternteil)")))

iqb <- rbind(iqb, m_migra, b_migra, c_migra, p_migra)
dbWriteTable(con, 'iqb', iqb, overwrite = TRUE, append = FALSE)
save(iqb, file="iqb.rda")
dbDisconnect(con, shutdown=TRUE)

#### Rohdatensatz einlesen ------------------------------------------------------

wd <- getwd()
setwd(wd)
#setwd("C:/Users/kab/Downloads/datalab/datalab")

## 4. Klasse
# Sheet 2 für alle drei Jahre auswählen
data <- readxl::read_excel(paste0(pfad, "IQB015_Abb3.17&3.19 (S.71&75)_2021.xlsx"), sheet = "Abb3.19")

# Nur Spalten mit Land, Standard, und Werten auswählen
data <- data %>%
  dplyr::select("...3", "...4", "perc_2011", "perc_2016", "perc_2021")

# standard nach geschlecht
data_gen <- readxl::read_excel(paste0(pfad, "IQB021_Tab6.1,6.2&6.6 (S.128,133&146)_2021.xlsx"), sheet = "Tab6.2")

## 9. Klasse
# Einlesen
d9 <- read.csv(paste0(pfad, "IQB001_Vergleich_Mathe 2018.csv"))


#### Datensatz aufbereiten ---------------------------------------------------

## 4. Klasse
# Spalten umbenennen und BULA-Namen korrigieren
data <- data %>%
  dplyr::rename(region = ...3,
                indikator = ...4,
                "2011" = perc_2011,
                "2016" = perc_2016,
                "2021" = perc_2021) %>%
  dplyr::mutate(region = dplyr::case_when(
    stringr::str_detect(region, "Baden") ~ "Baden-Württemberg",
    stringr::str_detect(region, "Westf") ~"Nordrhein-Westfalen",
    stringr::str_detect(region, "Pfal") ~"Rheinland-Pfalz",
    stringr::str_detect(region, "Anhal")~ "Sachsen-Anhalt",
    stringr::str_detect(region, "Holst") ~"Schleswig-Holstein",
    TRUE ~ region
  ))

# Bundesland Zuweisung auffüllen
data$region <- stats::ave(data$region, cumsum(!is.na(data$region)), FUN=function(x) x[1])

# NAs/überflüssige Spalten löschen
data <- stats::na.omit(data)

# 3 Werte-Spalten in eine Spalte und Jahr als Spalte ergänzen
data <- tidyr::pivot_longer(data, "2011":"2021", names_to = "jahr", values_to = "wert")

# klassenstufe ergänzen
data$klasse <- "4. Klasse"

## 4. Klasse nach Gender
# passende spalten/zeilen auswählen
data_gen <- data_gen %>%
  dplyr::select(...1, ...2, ...3, ...7, ...8, ...12, ...13) %>%
  dplyr::rename(
    fach = ...1,
    ms_nicht_j = ...2,
    ms_nicht_m = ...3,
    regels_j = ...7,
    regels_m = ...8,
    optimals_j = ...12,
    optimals_m = ...13
  ) %>%
  dplyr::filter(fach == "Globalskala")

# long format und umbennen
data_gen <- tidyr::pivot_longer(data_gen, "ms_nicht_j":"optimals_m", names_to = "indikator",
                                values_to = "wert")
data_gen <- data_gen %>%
  dplyr::mutate(geschlecht = dplyr::case_when(
    grepl("_j", indikator) ~ "Männer",
    grepl("_m", indikator) ~ "Frauen"
  ),
  indikator = dplyr::case_when(
    grepl("ms_nicht", indikator) ~ "Mindeststandard nicht erreicht",
    grepl("regel", indikator) ~ "Regelstandards erreicht",
    grepl("optim", indikator) ~ "Optimalstandard erreicht"
  ),
  jahr = 2021,
  fach = "Mathematik",
  klasse = "4. Klasse",
  typ = "standard",
  bereich = "Schule",
  region = "Deutschland")

## 9. Klasse
# umnennen
d9 <- d9 %>%
  dplyr::rename(
    region = X.1,
    "Mindeststandard nicht erreicht" = Wie.viele.Schüler.unter.dem.Mindeststandard.bleiben,
    "Regelstandard erreicht" = Wie.viele.Schüler.mindestens.den.Regelstandard.erreichen
  )

# in long formt bringen und spalten ergänzen
d9 <- tidyr::pivot_longer(d9, "Mindeststandard nicht erreicht":"Regelstandard erreicht",
                          names_to = "indikator", values_to = "wert")
d9$jahr <- "2018"
d9$klasse <- "9. Klasse"

#### Jahre zusammenfügen und Struktur ----------------------------------------

iqb_standard <- rbind(data, d9)

# Spalten ergänzen/angleichen, um zusammenführen zu können
iqb_standard$bereich <- "Schule"
iqb_standard$typ <- "standard"
iqb_standard$geschlecht <- "gesamt"
iqb_standard$fach <- "Mathematik"

iqb_standard <- rbind(iqb_standard, data_gen)

# Spalten sortieren
# Spalten in gleiche Reihenfolge bringen
iqb_standard <- iqb_standard[,c("bereich", "typ", "klasse", "fach", "indikator", "geschlecht", "region", "jahr", "wert")]

# jahr/wert als numeric
iqb_standard$wert <- as.numeric(iqb_standard$wert)
iqb_standard$jahr <- as.numeric(iqb_standard$jahr)

## iqb_score Teil ----------------------------------------------------

# Data Lab
# Vorbereitung Datensatz: IQB Daten 4. Klasse (2011, 2016, 2021) und 9. Klasse (2012, 2018)
# Author: Katharina Brunner, April 2023

library(dplyr)


### 4. Klasse ---------------------------------------------------------------

#### Rohdatensatz einlesen ------------------------------------------------------

# wd <- getwd()
# setwd(wd)

#setwd("C:/Users/kab/Downloads/datalab/datalab")

# Mathe Mittel ings. nur DE 2021
# einlesen IQB016 - sheet "Abb4.7"
d_m_ges <- readxl::read_excel(paste0(pfad, "IQB016_Abb4.7&4.14 (S.94&107)_2021.xlsx"), sheet = "Abb4.7")

#Mathe Mittel M und J 2021 inkl. Bundesländer
# einlesen IQB017 - sheet "Abb6.2"
d_m_gen_2021 <- readxl::read_excel(paste0(pfad, "IQB017_Abb6.1&6.2 (S.131&135)_2021.xlsx"), sheet = "Abb6.2")

#Mathe Mittel M und J 2011, 2016 nur DE verfügbar
# einlesen IQB021 - sheet "Tab6.1"
d_m_gen_2011_16 <- readxl::read_excel(paste0(pfad, "IQB021_Tab6.1,6.2&6.6 (S.128,133&146)_2021.xlsx"), sheet = "Tab6.1")

# Mathe Mittel Bildung hoch/niedrig, alle Jahre, alle BULA
# einlesen IQB018 - sheet "Abb_7.11"
d_m_bildung <- readxl::read_excel(paste0(pfad, "IQB018_Abb7.11 (S.172)_2021.xlsx"), sheet = "Abb_7.11")

# Mathe Mittel Einwanderung, alle Jahre, alle BULA
# einlesen IQB019 - sheet "Abb_8.9"
d_m_migra <- readxl::read_excel(paste0(pfad, "IQB019_Abb8.9 (S.200)_2021.xlsx"), sheet = "Abb_8.9")


#### Datensätze aufbereiten --------------------------------

## Mathe Mittel ings. nur DE
# nötige Spalten auswählen
d_m_ges <- d_m_ges %>%
  dplyr::select(...3, est_mean...4) %>%
  dplyr::rename(region = ...3,
                wert = est_mean...4)

# überflüssige Zeilen löschen
d_m_ges<-d_m_ges[-1,]
d_m_ges <- na.omit(d_m_ges)

# nötige Spalten ergänzen
d_m_ges$jahr <- 2021
d_m_ges$geschlecht <- "gesamt"
d_m_ges$indikator <- "Alle"

# wert als numerisch
d_m_ges$wert <- as.numeric(d_m_ges$wert)


##Mathe Mittel M und J 2021 inkl. Bundesländer
# nötige Spalten auswählen
d_m_gen_2021 <- d_m_gen_2021 %>%
  dplyr::select(...1, ...2, Jungen, Mädchen) %>%
  dplyr::rename(region = ...1)

# Bundesland Zuweisung auffüllen
d_m_gen_2021$region <- stats::ave(d_m_gen_2021$region, cumsum(!is.na(d_m_gen_2021$region)), FUN=function(x) x[1])

# überflüssige Zeilen löschen
d_m_gen_2021 <- d_m_gen_2021 %>% dplyr::filter(...2 == "Mathematik")
d_m_gen_2021 <- na.omit(d_m_gen_2021)

# geschlecht als Spalte
d_m_gen_2021 <- tidyr::pivot_longer(d_m_gen_2021, "Jungen":"Mädchen", names_to = "geschlecht", values_to = "wert")

# nötige Spalten ergänzen/löschen
d_m_gen_2021 <- d_m_gen_2021 %>%
  dplyr::select(-...2) %>%
  dplyr::mutate(geschlecht = dplyr::case_when(
    grepl("Jung", geschlecht) ~ "Männer",
    grepl("Mädc", geschlecht) ~ "Frauen",
    TRUE ~ geschlecht
  ))
d_m_gen_2021$jahr <- 2021
d_m_gen_2021$indikator <- "Alle"

# wert als numerisch
d_m_gen_2021$wert <- as.numeric(d_m_gen_2021$wert)

##Mathe Mittel M und J 2011, 2016 nur DE verfügbar
#Teiltabelle Deutsch löschen
d_m_gen_2011_16 <- d_m_gen_2011_16[7:17,]

# nötige Spalten auswählen
d_m_gen_2011_16 <- d_m_gen_2011_16 %>%
  dplyr::select(Deutsch, Primarbereich, ...3, ...7, ...8) %>%
  dplyr::rename(Jungen_2011 = Primarbereich,
                Mädchen_2011 = ...3,
                Jungen_2016 = ...7,
                Mädchen_2016 = ...8)

# überflüssige Zeilen löschen
d_m_gen_2011_16 <- na.omit(d_m_gen_2011_16)
d_m_gen_2011_16 <- d_m_gen_2011_16 %>%
  dplyr::filter(Deutsch == "Globalskala") %>%
  dplyr::select(-Deutsch)

# geschlecht als Spalte
d_m_gen_2011_16 <- tidyr::pivot_longer(d_m_gen_2011_16, tidyr::everything(), names_to = "geschlecht", values_to = "wert")

# nötige Spalten ergänzen/löschen
d_m_gen_2011_16 <- d_m_gen_2011_16 %>%
  dplyr::mutate(jahr = dplyr::case_when(
    stringr::str_detect(geschlecht, "2011") ~ 2011,
    stringr::str_detect(geschlecht, "2016") ~ 2016
  ),
  geschlecht = dplyr::case_when(
    stringr::str_detect(geschlecht, "Mädc") ~"Frauen",
    stringr::str_detect(geschlecht, "Jung") ~ "Männer"
  ))

d_m_gen_2011_16$region <- "Deutschland"
d_m_gen_2011_16$indikator <- "Alle"

# wert als numerisch
d_m_gen_2011_16$wert <- as.numeric(d_m_gen_2011_16$wert)

## Mathe Mittel Bildung hoch/niedrig, alle Jahre, alle BULA
# nötige Spalten auswählen
d_m_bildung <- d_m_bildung %>%
  dplyr::select(...22, "mehr als 100 Bücher", ...24, ...25, "maximal 100 Bücher", ...28, ...29) %>%
  dplyr::rename(region = ...22,
                indikator_hoch_2011 = "mehr als 100 Bücher",
                indikator_hoch_2016 = ...24,
                indikator_hoch_2021 = ...25,
                indikator_niedrig_2011 = "maximal 100 Bücher",
                indikator_niedrig_2016 = ...28,
                indikator_niedrig_2021 = ...29)

# überflüssige Zeilen löschen
d_m_bildung <- na.omit(d_m_bildung)

# geschlecht als Spalte
d_m_bildung <- tidyr::pivot_longer(d_m_bildung, "indikator_hoch_2011":"indikator_niedrig_2021", names_to = "indikator", values_to = "wert")

# nötige Spalten ergänzen
d_m_bildung <- d_m_bildung %>%
  dplyr::mutate(jahr = dplyr::case_when(
    stringr::str_detect(indikator, "2011") ~2011,
    stringr::str_detect(indikator, "2016") ~2016,
    stringr::str_detect(indikator, "2021") ~2021
  ),
  indikator = dplyr::case_when(
    stringr::str_detect(indikator, "hoch") ~ "kapital_hoch",
    stringr::str_detect(indikator, "niedr") ~ "kapital_niedrig"
  ))
d_m_bildung$geschlecht <- "gesamt"

# wert als numerisch
d_m_bildung$wert <- as.numeric(d_m_bildung$wert)

## Mathe Mittel Einwanderung, alle Jahre, alle BULA
# nötige Spalten auswählen
d_m_migra <- d_m_migra %>%
  dplyr::select(...22, ...23, ...24, ...25, ...30, ...31, ...32) %>%
  dplyr::rename(region = ...22,
                migra_2011 = ...23,
                migra_2016 = ...24,
                migra_2021 = ...25,
                keine_migra_2011 = ...30,
                keine_migra_2016 = ...31,
                keine_migra_2021 = ...32)

# überflüssige Zeilen löschen
d_m_migra <- na.omit(d_m_migra)

# geschlecht als Spalte
d_m_migra <- tidyr::pivot_longer(d_m_migra, "migra_2011":"keine_migra_2021", names_to = "indikator", values_to = "wert")

# nötige Spalten ergänzen
d_m_migra <- d_m_migra %>%
  dplyr::mutate(jahr = dplyr::case_when(
    stringr::str_detect(indikator, "2011") ~2011,
    stringr::str_detect(indikator, "2016") ~2016,
    stringr::str_detect(indikator, "2021") ~2021
  ),
  indikator = dplyr::case_when(
    stringr::str_detect(indikator, "keine") ~ "ohne Zuwanderungsgeschichte",
    TRUE ~ "mit Zuwanderungsgeschichte" #bedeutet hier beide Eltern migriert, Extremwerte verglichen
  ))

d_m_migra$geschlecht <- "gesamt"

# wert als numerisch
d_m_migra$wert <- as.numeric(d_m_migra$wert)

# üs einfügen
d_m_migra$region[d_m_migra$region == "Baden-Wuerttemberg"] <- "Baden-Württemberg"
d_m_migra$region[d_m_migra$region == "Thueringen"] <- "Thüringen"

#### Datensätze zusammenfügen  -----------------------------------------------

#Zusammenfügen
iqb_score <- rbind(d_m_ges, d_m_gen_2021, d_m_gen_2011_16, d_m_bildung, d_m_migra)


### 9. Klasse ---------------------------------------------------------------

#### Rohdatensatz einlesen ------------------------------------------------------

# wd <- getwd()
# setwd(wd)

#setwd("C:/Users/kab/Downloads/datalab/datalab")

# Mathe Mittel ings. DE & BULAs, 2012, 2018
d_m_ges <- readxl::read_excel(paste0(pfad, "IQB009_Abb6.4 (S.207).xlsx"))

# Fachkenntnisse nach Gender
# Mathe Mittel
d_m_gen <- readxl::read_excel(paste0(pfad, "IQB011_Abb7.9-7.15 (S.253-256).xlsx"), sheet = "Abb 7.9_Mathe_GL")
# Bio
d_b_gen <- readxl::read_excel(paste0(pfad, "IQB011_Abb7.9-7.15 (S.253-256).xlsx"), sheet = "Abb 7.10_BF")
# Chemie
d_c_gen <- readxl::read_excel(paste0(pfad, "IQB011_Abb7.9-7.15 (S.253-256).xlsx"), sheet = "Abb 7.12_CF")
# Physik
d_p_gen <- readxl::read_excel(paste0(pfad, "IQB011_Abb7.9-7.15 (S.253-256).xlsx"), sheet = "Abb 7.14_PF")

# Fachkenntnisse nach Status
# Mathe
d_m_stat <- readxl::read_excel(paste0(pfad, "IQB012_Abb8.17-8.23 (S.284-290).xlsx"), sheet = "Abb8.17_GL")
# Bio
d_b_stat <- readxl::read_excel(paste0(pfad, "IQB012_Abb8.17-8.23 (S.284-290).xlsx"), sheet = "Abb8.18_BF")
# Chemie
d_c_stat <- readxl::read_excel(paste0(pfad, "IQB012_Abb8.17-8.23 (S.284-290).xlsx"), sheet = "Abb8.20_CF")
# Physik
d_p_stat <- readxl::read_excel(paste0(pfad, "IQB012_Abb8.17-8.23 (S.284-290).xlsx"), sheet = "Abb8.22_PF")

# Fachkenntnisse nach Zuwanderungsgeschichte
# Mathe
d_m_migra <- readxl::read_excel(paste0(pfad, "IQB013_Abb9.2-9.8 (S.305-315).xlsx"), sheet = "Abb 9.2 GL")
# Bio
d_b_migra <- readxl::read_excel(paste0(pfad, "IQB013_Abb9.2-9.8 (S.305-315).xlsx"), sheet = "Abb 9.3 BF")
# Chemie
d_c_migra <- readxl::read_excel(paste0(pfad, "IQB013_Abb9.2-9.8 (S.305-315).xlsx"), sheet = "Abb 9.5 CF")
# Physik
d_p_migra <- readxl::read_excel(paste0(pfad, "IQB013_Abb9.2-9.8 (S.305-315).xlsx"), sheet = "Abb 9.7 PF")


#### Datensatz aufbereiten ---------------------------------------------------

## Mathe Mittel ings. DE & BULAs, 2012, 2018

# benötigte Spalten auswählen
d_m_ges <- d_m_ges %>%
  dplyr::select(...1, "2012", "2018")

# Missings löschen
d_m_ges <- na.omit(d_m_ges)

# Umbennenen und Spalten ergänzen
d_m_ges <- d_m_ges %>%
  dplyr::rename(
    region = ...1
  ) %>%
  dplyr::mutate(
    geschlecht = "gesamt",
    indikator = "Alle",
    fach = "Mathematik"
  )

# in long Format bringen
d_m_ges <- tidyr::pivot_longer(d_m_ges, "2012":"2018", values_to = "wert", names_to = "jahr")

## Fachkenntnisse nach Gender
aufbereitung_gen <- function(df, fach){
  df <- df %>%
    dplyr::select(...1, "Jungen", ...5, "Mädchen", ...14) %>%
    dplyr::rename(
      region = ...1,
      "2012_j" = Jungen,
      "2018_j" = ...5,
      "2012_m" = Mädchen,
      "2018_m" = ...14
    ) %>%
    dplyr::mutate(
      indikator = "Alle"
    )

  # missings löschen
  df <- na.omit(df)

  # in long Format bringen und spalten zuweisen
  df <- tidyr::pivot_longer(df, "2012_j":"2018_m", names_to = "jahr", values_to = "wert")
  df <- df %>%
    dplyr::mutate(geschlecht = dplyr::case_when(
      grepl("_j", jahr) ~ "Männer",
      grepl("_m", jahr) ~ "Frauen"
    ),
    jahr = dplyr::case_when(
      grepl("12", jahr) ~ 2012,
      grepl("18", jahr) ~ 2018
    ),
    fach = fach
    )
  return(df)
}

d_m_gen <- aufbereitung_gen(d_m_gen, "Mathematik")
d_b_gen <- aufbereitung_gen(d_b_gen, "Biologie")
d_c_gen <- aufbereitung_gen(d_c_gen, "Chemie")
d_p_gen <- aufbereitung_gen(d_p_gen, "Physik")

## Fachkenntnisse nach Status
aufbereitung_stat <- function(df, fach){
  df <- df %>%
    dplyr::select(...1, "2012", "2018") %>%
    dplyr::rename(
      region = ...1
    )

  # Labels auf alle Zeilen übertragen
  df$region <- stats::ave(df$region, cumsum(!is.na(df$region)), FUN=function(x) x[1])

  # zweite Zeile mit niedrigem Status kennzeichnen
  for(x in 2:length(df$region)){
    if(df$region[x] == df$region[x-1]) df$region[x] <- paste0(df$region[x], "low")
  }

  # missings löschen
  df <- na.omit(df)

  # in long Format bringen und spalten zuweisen
  df <- tidyr::pivot_longer(df, "2012":"2018", names_to = "jahr", values_to = "wert")

  # Indikator-Spalte erstellen und Beschriftungen anpassen
  df$indikator <- df$region
  df <- df %>%
    dplyr::mutate(indikator = dplyr::case_when(
      grepl("low", indikator) ~ "niedriger Status",
      TRUE ~ "hoher Status"
    ),
    region = stringr::str_remove(region, "low"),
    region = gsub("[0-9]+", "", region),
    region = gsub(",", "", region),
    fach = fach,
    geschlecht = "gesamt"
    ) %>%
    dplyr::filter(region != "Land")
  return(df)
}

d_m_stat <- aufbereitung_stat(d_m_stat, "Mathematik")
d_b_stat <- aufbereitung_stat(d_b_stat, "Biologie")
d_c_stat <- aufbereitung_stat(d_c_stat, "Chemie")
d_p_stat <- aufbereitung_stat(d_p_stat, "Physik")

## Fachkenntnisse nach Zuwanderungsgeschichte
aufbereitung_migra <- function(df, fach){
  df <- df %>%
    dplyr::select(...1, ...6, ...14) %>%
    dplyr::rename(
      region = ...1,
      "2012" = ...6,
      "2018" = ...14
    )

  # Labels auf alle Zeilen übertragen
  df$region <- stats::ave(df$region, cumsum(!is.na(df$region)), FUN=function(x) x[1])

  # missings löschen
  df <- na.omit(df)

  # zweite Zeile mit niedrigem Status kennzeichnen
  for(x in 2:length(df$region)){
    if(df$region[x] == df$region[x-1]) df$region[x] <- paste0(df$region[x], "mit1")
  }
  for(x in 3:length(df$region)){
    if(df$region[x] == df$region[x-2]) df$region[x] <- paste0(df$region[x], "mit2")
  }

  # in long Format bringen und spalten zuweisen
  df <- tidyr::pivot_longer(df, "2012":"2018", names_to = "jahr", values_to = "wert")

  # Indikator-Spalte erstellen und Beschriftungen anpassen
  df$indikator <- df$region
  df <- df %>%
    dplyr::mutate(indikator = dplyr::case_when(
      grepl("mit1", indikator) ~ "mit Zuwanderungsgeschichte (ein Elternteil)",
      grepl("mit2", indikator) ~ "mit Zuwanderungsgeschichte (beide Elternteile)",
      TRUE ~ "ohne Zuwanderungsgeschichte"
    ),
    region = gsub("mit1", "", region),
    region = gsub("mit2", "", region),
    region = gsub("[0-9]+", "", region),
    region = gsub(",", "", region),
    fach = fach,
    geschlecht = "gesamt",
    region = dplyr::case_when(
      grepl("Baden", region) ~ "Baden-Württemberg",
      grepl("Meck", region) ~ "Mecklenburg-Vorpommern",
      grepl("Anhal", region) ~ "Sachsen-Anhalt",
      grepl("Schles", region) ~ "Schleswig-Holstein",
      grepl("Nord", region) ~ "Nordrhein-Westfalen",
      grepl("Pfal", region) ~ "Rheinland-Pfalz",
      TRUE ~region
    )
    ) %>%
    dplyr::filter(region != "Land")

  return(df)
}

d_m_migra <- aufbereitung_migra(d_m_migra, "Mathematik")
d_b_migra <- aufbereitung_migra(d_b_migra, "Biologie")
d_c_migra <- aufbereitung_migra(d_c_migra, "Chemie")
d_p_migra <- aufbereitung_migra(d_p_migra, "Physik")

### 9. Klasse Daten zusammenfügen
iqb_score_9 <- rbind(d_m_ges, d_m_gen, d_b_gen, d_c_gen, d_p_gen, d_m_stat, d_b_stat, d_c_stat, d_p_stat,
                     d_m_migra, d_b_migra, d_c_migra, d_p_migra)

# Jahre zusammenfügen und Struktur ----------------------------------------

# Spalten ergänzen/angleichen, um zusammenführen zu können
iqb_score$fach <- "Mathematik"
iqb_score$bereich <- "Schule"
iqb_score_9$bereich <- "Schule"
iqb_score$typ <- "score"
iqb_score_9$typ <- "score"
iqb_score$klasse <- "4. Klasse"
iqb_score_9$klasse <- "9. Klasse"


#Spalten sortieren
iqb_score <- iqb_score[,c("bereich", "typ", "klasse", "fach", "indikator", "geschlecht",
                          "region", "jahr", "wert")]
iqb_score_9 <- iqb_score_9[,c("bereich", "typ", "klasse", "fach", "indikator", "geschlecht",
                          "region", "jahr", "wert")]

iqb_score_ges <- rbind(iqb_score, iqb_score_9)

# Numerisch und Kommas rein
iqb_score_ges$wert <- as.numeric(iqb_score_ges$wert)
iqb_score_ges$jahr <- as.numeric(iqb_score_ges$jahr)


## iqb_fragen Teil -----------------------------------------------------------

# Data Lab
# Vorbereitung Datensatz: IQB Daten 4. Klasse (2016, 2021) Befragung Selbstwert/Interesse
# Author: Katharina Brunner, Juli 2023

#### Rohdatensatz einlesen ---------------------------------------------------

# wd <- getwd()
# setwd(wd)

# Einlesen
data <- readxl::read_excel(paste0(pfad, "IQB020_Abb9.3 (S.227)_2021.xlsx"))

#### Datensatz aufbereiten ---------------------------------------------------

# Labels auf alle Zeilen übertragen
data$...1 <- stats::ave(data$...1, cumsum(!is.na(data$...1)), FUN=function(x) x[1])

# Zeilen auswählen
data <- data %>%
  dplyr::select(...1, ...2, "2016", "2021") %>%
  dplyr::rename(
    indikator = ...1,
    geschlecht = ...2
  )

# Fach zuordnen
data$indikator[4:9] <- paste0(data$indikator[4:9], "_Deutsch")
data$indikator[11:17] <- paste0(data$indikator[11:17], "_Mathe")

#missings entfernen
data <- na.omit(data)

# ins long-Format bringen
data <- data %>%
  tidyr::pivot_longer(cols = "2016":"2021", names_to = "jahr", values_to = "wert")

# Fach extra speichern
data <- data %>%
  dplyr::mutate(fach = dplyr::case_when(
    grepl("Deu", indikator) ~ "Deutsch",
    grepl("Math", indikator) ~ "Mathematik"
  ),
  indikator = gsub("_Deutsch", "", indikator),
  indikator = gsub("_Mathe", "", indikator),
  )

# jahr/wert als numerisch speichern
data$jahr <- as.numeric(data$jahr)
data$wert <- as.numeric(data$wert)

# Spalten ergänzen zum Zusammenfügen
data$bereich <- "Schule"
data$typ <- "fragen"
data$region <- "Deutschland"
data$klasse <- "4. Klasse"

# Reihenfolge der Spalten anpassen und bennenen
iqb_fragen <- data[, c("bereich", "typ", "klasse", "fach", "indikator", "geschlecht", "region", "jahr", "wert")]

# Komma anstatt Punkt
iqb_fragen$wert <- as.numeric(iqb_fragen$wert)
iqb_fragen$jahr <- as.numeric(iqb_fragen$jahr)


# iqb zusammenfassen und speichern ----------------------------------------
iqb <- rbind(iqb_standard, iqb_score_ges, iqb_fragen)

usethis::use_data(iqb, overwrite = T)

