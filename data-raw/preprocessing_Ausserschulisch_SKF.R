################################################################################
#
# Preprocessing Data Lab
# Vorbereitung Datensatz: SKF001
# Author: Katharina Brunner, Juni 2023
#
################################################################################

library(dplyr)

# Rohdatensatz einlesen ------------------------------------------------------

wd <- getwd()
setwd(wd)

# pfad analgen

pfad_kab <- "C:/Users/kab/OneDrive - Stifterverband/AP7 MINT-DataLab/02 Datenmaterial/01_Rohdaten/02_Alle Daten/"

pfad <- pfad_kab

data <- readxl::read_excel(paste0(pfad,"SKF001_230130.xlsx"),
                           sheet = "Datentabelle", col_names = FALSE)


# Datensatz ins passende Format bringen --------------------------------------

## header erzeugen
i <- 2
header <- character(ncol(data))

for(i in i:10){
  header[1] <- "jahr"
  header[i] <- paste0(data[1, i], " ", data[2,i])
}

# header übertragen
colnames(data)<-header

# überflüssige Zeilen entfernen
data <- data[c(-1,-2),]

#jahr korrigieren - bei Einlesen verzerrt
data$jahr <- 2012:2022

#in long format speichern
data <- tidyr::pivot_longer(data, cols = "Kitas Anzahl aktive Einrichtungen":"Grundschulen Schätzung teilnehmende Fach- / Lehrkräfte")


# Datensatz zur Nutzung aufbereiten ---------------------------------------

# Spalte für Indikator und Ort erstellen
data <- data %>%
  dplyr::mutate(einrichtung = dplyr::case_when(
    grepl("Kita", name) ~ "Kita",
    grepl("Hort", name) ~ "Hort",
    grepl("Gru", name) ~ "Grundschule"
  ),
  indikator = dplyr::case_when(
    grepl("aktive", name) ~ "aktive Einrichtungen gesamt",
    grepl("zertif", name) ~ "zertifizierte Einrichtungen",
    grepl("Schät", name) ~ "insgesamt fortgebildete Fach- / Lehrkräfte"
  )) %>%
  dplyr::rename(wert = value) %>%
  dplyr::select(-name)

# aktive Einrichtungen gesamt = zerfitizierte Einrichtungen + Einrichtungen mit Fortbildung
# Einrichtungen mit Fortbildung bereichnen
data$wert <- ifelse(grepl("keine", data$wert), NA, data$wert)
data$wert <- as.numeric(data$wert)

emf <- data %>%
  dplyr::filter(indikator == "aktive Einrichtungen gesamt")
ze <- data %>%
  dplyr::filter(indikator == "zertifizierte Einrichtungen")

emf <- emf %>%
  dplyr::left_join(ze, c("jahr", "einrichtung")) %>%
  dplyr::mutate(wert = wert.x - wert.y) %>%
  dplyr::select(c(-indikator.x, -indikator.y, -wert.x, -wert.y))
emf$indikator <- "Einrichtungen mit SKf-Fortbildung"

data <- rbind(data, emf)

# Anzahl neuer forgebildeter Fach- / Lehrkräfte berechnen
nf <- data %>%
  dplyr::filter(indikator == "insgesamt fortgebildete Fach- / Lehrkräfte")

i <- 2022
for(i in 2022:2013){
    nf$wert[nf$jahr == i] <- nf$wert[nf$jahr == i] - nf$wert[nf$jahr == i-1]
}
nf$wert[nf$jahr == 2012] <- NA

nf$indikator <- "neu fortgebildete Fach- / Lehrkräfte"

data <- rbind(data, nf)

# bereich Spalte ergänzen
data$bereich <- "Außerschulisch"

# Spalten in logische Reihenfolge bringen
data <- data[,c("bereich", "einrichtung", "indikator", "jahr", "wert")]


# Datensatz abspeichern ---------------------------------------------------

ausserschulisch_skf <- data

# Datensatz speichern
usethis::use_data(ausserschulisch_skf, overwrite = T)
