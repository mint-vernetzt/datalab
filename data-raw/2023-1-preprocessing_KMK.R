################################################################################
#
# Data Lab
# Vorbereitung Datensatz: Schule
# Author: Katharina Brunner, Januar 2023
# Quelle: C:\Users\kbr\OneDrive - Stifterverband\MINTvernetzt (SV)\MINTv_SV_AP7 MINT-DataLab\02 Datenmaterial\02_Prozess\Datenaufbereitung 2023\Schule
#
################################################################################

# Rohdatensatz einlesen ------------------------------------------------------
library(dplyr)
library(tidyr)

wd <- getwd()
setwd(wd)
#setwd("C:/Users/kab/OneDrive - Stifterverband/MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/02 Datenmaterial/01_Rohdaten/02_Alle Daten")

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
data_LK_D  <- read_data("KMK023_Aus_Kurse_2021_Werte.xlsx", "D" , 2021, "Leistungskurse")
data_LK_BW <- read_data("KMK023_Aus_Kurse_2021_Werte.xlsx", "BW", 2021, "Leistungskurse")
data_LK_BY <- read_data("KMK023_Aus_Kurse_2021_Werte.xlsx", "BY", 2021, "Leistungskurse")
data_LK_BE <- read_data("KMK023_Aus_Kurse_2021_Werte.xlsx", "BE", 2021, "Leistungskurse")
data_LK_BB <- read_data("KMK023_Aus_Kurse_2021_Werte.xlsx", "BB", 2021, "Leistungskurse")
data_LK_HB <- read_data("KMK023_Aus_Kurse_2021_Werte.xlsx", "HB", 2021, "Leistungskurse")
data_LK_HH <- read_data("KMK023_Aus_Kurse_2021_Werte.xlsx", "HH", 2021, "Leistungskurse")
data_LK_HE <- read_data("KMK023_Aus_Kurse_2021_Werte.xlsx", "HE", 2021, "Leistungskurse")
data_LK_MV <- read_data("KMK023_Aus_Kurse_2021_Werte.xlsx", "MV", 2021, "Leistungskurse")
data_LK_NI <- read_data("KMK023_Aus_Kurse_2021_Werte.xlsx", "NI", 2021, "Leistungskurse")
data_LK_NW <- read_data("KMK023_Aus_Kurse_2021_Werte.xlsx", "NW", 2021, "Leistungskurse")
data_LK_RP <- read_data("KMK023_Aus_Kurse_2021_Werte.xlsx", "RP", 2021, "Leistungskurse")
data_LK_SL <- read_data("KMK023_Aus_Kurse_2021_Werte.xlsx", "SL", 2021, "Leistungskurse")
data_LK_SN <- read_data("KMK023_Aus_Kurse_2021_Werte.xlsx", "SN", 2021, "Leistungskurse")
data_LK_ST <- read_data("KMK023_Aus_Kurse_2021_Werte.xlsx", "ST", 2021, "Leistungskurse")
data_LK_SH <- read_data("KMK023_Aus_Kurse_2021_Werte.xlsx", "SH", 2021, "Leistungskurse")
data_LK_TH <- read_data("KMK023_Aus_Kurse_2021_Werte.xlsx", "TH", 2021, "Leistungskurse")

data_GK_D  <- read_data("KMK023_Aus_Kurse_2021_Werte.xlsx", "D" , 2021, "Grundkurse")
data_GK_BW <- read_data("KMK023_Aus_Kurse_2021_Werte.xlsx", "BW", 2021, "Grundkurse")
data_GK_BY <- read_data("KMK023_Aus_Kurse_2021_Werte.xlsx", "BY", 2021, "Grundkurse")
data_GK_BE <- read_data("KMK023_Aus_Kurse_2021_Werte.xlsx", "BE", 2021, "Grundkurse")
data_GK_BB <- read_data("KMK023_Aus_Kurse_2021_Werte.xlsx", "BB", 2021, "Grundkurse")
data_GK_HB <- read_data("KMK023_Aus_Kurse_2021_Werte.xlsx", "HB", 2021, "Grundkurse")
data_GK_HH <- read_data("KMK023_Aus_Kurse_2021_Werte.xlsx", "HH", 2021, "Grundkurse")
data_GK_HE <- read_data("KMK023_Aus_Kurse_2021_Werte.xlsx", "HE", 2021, "Grundkurse")
data_GK_MV <- read_data("KMK023_Aus_Kurse_2021_Werte.xlsx", "MV", 2021, "Grundkurse")
data_GK_NI <- read_data("KMK023_Aus_Kurse_2021_Werte.xlsx", "NI", 2021, "Grundkurse")
data_GK_NW <- read_data("KMK023_Aus_Kurse_2021_Werte.xlsx", "NW", 2021, "Grundkurse")
data_GK_RP <- read_data("KMK023_Aus_Kurse_2021_Werte.xlsx", "RP", 2021, "Grundkurse")
data_GK_SL <- read_data("KMK023_Aus_Kurse_2021_Werte.xlsx", "SL", 2021, "Grundkurse")
data_GK_SN <- read_data("KMK023_Aus_Kurse_2021_Werte.xlsx", "SN", 2021, "Grundkurse")
data_GK_ST <- read_data("KMK023_Aus_Kurse_2021_Werte.xlsx", "ST", 2021, "Grundkurse")
data_GK_SH <- read_data("KMK023_Aus_Kurse_2021_Werte.xlsx", "SH", 2021, "Grundkurse")
data_GK_TH <- read_data("KMK023_Aus_Kurse_2021_Werte.xlsx", "TH", 2021, "Grundkurse")

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


# Datensatz in gewünschte From bringe -------------------------------------

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
    region== "RP" ~   "Rheinland-Pflaz",
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

# Datensatz zusammenfügen, anpassen und exportieren --------------------------------------

data_z <- readxl::read_excel("Kurse_21_10_22.xlsx", col_names = T)

#Spalten in gleiche Reihenfolge bringen
data_z <- data_z[,c("bereich", "hinweise", "quelle", "indikator", "fachbereich", "anzeige_geschlecht",
                    "region", "Jahr", "wert")]

#neue Daten anhängen
data_z <- rbind(data_z, data)

#Spaltenbezeichnungen anpassen - für geschlecht vorläufig noch nicht
data_z <- data_z %>%
  rename(#geschlecht = anzeige_geschlecht,
         jahr = Jahr)

# Anpassungen 13.01. kab
# Oberstufenbelegungen hinzufügen

data_zz <- data_z %>%
  tidyr::pivot_wider(names_from = indikator, values_from = wert)%>%
  dplyr::mutate(Oberstufenbelegungen = Grundkurse + Leistungskurse)%>%
  tidyr::pivot_longer(c(Grundkurse, Leistungskurse, Oberstufenbelegungen), values_to = "wert", names_to ="indikator")%>%
  dplyr::select("bereich", "hinweise", "quelle", "indikator", "fachbereich", "anzeige_geschlecht",
         "region", "jahr", "wert")


#exportieren
kurse <- data_zz %>%
  janitor::clean_names() %>%
  janitor::remove_empty() %>%
  dplyr::select(-c("quelle", "hinweise")) %>%
  dplyr::filter(!fachbereich %in% c("Nicht MINT", "MINT")) %>%
  dplyr::mutate(wert = tidyr::replace_na(wert, 0))

kurse <- kurse %>% dplyr::filter(jahr >= 2010)

kurse$wert <- round(kurse$wert)

#kurse <- kurse %>% dplyr::filter(region != "Baden-Württemberg")

#kurse[kurse$region == "Rheinland-Pflaz", "region"] <- "Rheinland-Pfalz"

kurse[kurse$anzeige_geschlecht == "frauen", "anzeige_geschlecht"] <- "Frauen"
kurse[kurse$anzeige_geschlecht == "gesamt", "anzeige_geschlecht"] <- "Gesamt"
kurse[kurse$anzeige_geschlecht == "männer", "anzeige_geschlecht"] <- "Männer"

usethis::use_data(kurse, overwrite = T)

