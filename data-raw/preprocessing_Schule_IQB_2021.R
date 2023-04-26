################################################################################
#
# Data Lab
# Vorbereitung Datensatz: IQB Daten 4. Klasse (2011, 2016, 2021)
# Author: Katharina Brunner, April 2023
#
################################################################################

library(dplyr)

# Rohdatensatz einlesen ------------------------------------------------------

wd <- getwd()
setwd(wd)

# Sheet 2 für alle drei Jahre auswählen
data <- readxl::read_excel("IQB015_Abb3.17&3.19 (S.71&75)_2021.xlsx", sheet = "Abb3.19")

# Nur Spalten mit Land, Standard, und Werten auswählen
data <- data %>%
  dplyr::select("...3", "...4", "perc_2011", "perc_2016", "perc_2021")


# Datensatz aufbereiten ---------------------------------------------------

# Spalten und Standards umbenennen
data <- data %>%
  dplyr::rename(region = ...3,
         indikator = ...4,
         "2011" = perc_2011,
         "2016" = perc_2016,
         "2021" = perc_2021)

# Bundesland Zuweisung auffüllen
data$region <- stats::ave(data$region, cumsum(!is.na(data$region)), FUN=function(x) x[1])

# NAs/überflüssige Spalten löschen
data <- stats::na.omit(data)

# 3 Werte-Spalten in eine Spalte und Jahr als Spalte ergänzen
data <- tidyr::pivot_longer(data, "2011":"2021", names_to = "jahr", values_to = "wert")


# Datensatz abspeichern ---------------------------------------------------

iqb_4klasse <- data
iqb_4klasse$wert <- as.numeric(iqb_4klasse$wert)

usethis::use_data(iqb_4klasse, overwrite = T)

