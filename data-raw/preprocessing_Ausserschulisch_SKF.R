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
#setwd("C:/Users/kab/Downloads/datalab/datalab")

data <- readxl::read_excel("data-raw/raw/SKF001_230130.xlsx", sheet = "Datentabelle", col_names = FALSE)


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
    grepl("aktive", name) ~ "aktive Einrichtungen",
    grepl("zertif", name) ~ "zertifizierte Einrichtungen",
    grepl("Schät", name) ~ "Fach- / Lehrkräfte"
  )) %>%
  dplyr::rename(wert = value) %>%
  dplyr::select(-name)

# bereich Spalte ergänzen
data$bereich <- "Außerschulisch"

# Spalten in logische Reihenfolge bringen
data <- data[,c("bereich", "einrichtung", "indikator", "jahr", "wert")]


# Datensatz abspeichern ---------------------------------------------------

data$wert <- ifelse(grepl("keine", data$wert), NA, data$wert)
data$wert <- as.numeric(data$wert)

ausserschulisch_skf <- data

# Datensatz speichern
usethis::use_data(ausserschulisch_skf, overwrite = T)
