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

wd <- getwd()
setwd(wd)
#setwd("C:/Users/kab/OneDrive - Stifterverband/MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/02 Datenmaterial/01_Rohdaten/02_Alle Daten")
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
wd2 <- paste0(wd, "/raw")
setwd(wd2)

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

## Datensatz zusammenfügen, anpassen und exportieren --------------------------------------

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

kurse[kurse$region == "Rheinland-Pflaz", "region"] <- "Rheinland-Pfalz"

kurse[kurse$anzeige_geschlecht == "frauen", "anzeige_geschlecht"] <- "Frauen"
kurse[kurse$anzeige_geschlecht == "gesamt", "anzeige_geschlecht"] <- "Gesamt"
kurse[kurse$anzeige_geschlecht == "männer", "anzeige_geschlecht"] <- "Männer"


usethis::use_data(kurse, overwrite = T)



# Erstellt "iqb_standard" -------------------------------------------------

#
# Data Lab
# Vorbereitung Datensatz: IQB Daten 4. Klasse (2011, 2016, 2021)
# Author: Katharina Brunner, April 2023
#

library(dplyr)

## Rohdatensatz einlesen ------------------------------------------------------

wd <- getwd()
setwd(wd)
#setwd("C:/Users/kab/Downloads/datalab/datalab")

# Sheet 2 für alle drei Jahre auswählen
data <- readxl::read_excel("data-raw/raw/IQB015_Abb3.17&3.19 (S.71&75)_2021.xlsx", sheet = "Abb3.19")

# Nur Spalten mit Land, Standard, und Werten auswählen
data <- data %>%
  dplyr::select("...3", "...4", "perc_2011", "perc_2016", "perc_2021")


## Datensatz aufbereiten ---------------------------------------------------

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


## Datensatz abspeichern ---------------------------------------------------

iqb_standard <- data
iqb_standard$wert <- as.numeric(iqb_standard$wert)

usethis::use_data(iqb_standard, overwrite = T)




# Erstellt "iqb_score" ----------------------------------------------------


#
# Data Lab
# Vorbereitung Datensatz: IQB Daten 4. Klasse (2011, 2016, 2021) und 9. Klasse (2012, 2018)
# Author: Katharina Brunner, April 2023
#


library(dplyr)

## Rohdatensatz einlesen ------------------------------------------------------

wd <- getwd()
setwd(wd)

#setwd("C:/Users/kab/Downloads/datalab/datalab")

# Mathe Mittel ings. nur DE 2021
# einlesen IQB016 - sheet "Abb4.7"
d_m_ges <- readxl::read_excel("data-raw/raw/IQB016_Abb4.7&4.14 (S.94&107)_2021.xlsx", sheet = "Abb4.7")

#Mathe Mittel M und J 2021 inkl. Bundesländer
# einlesen IQB017 - sheet "Abb6.2"
d_m_gen_2021 <- readxl::read_excel("data-raw/raw/IQB017_Abb6.1&6.2 (S.131&135)_2021.xlsx", sheet = "Abb6.2")

#Mathe Mittel M und J 2011, 2016 nur DE verfügbar
# einlesen IQB021 - sheet "Tab6.1"
d_m_gen_2011_16 <- readxl::read_excel("data-raw/raw/IQB021_Tab6.1,6.2&6.6 (S.128,133&146)_2021.xlsx", sheet = "Tab6.1")

# Mathe Mittel Bildung hoch/niedrig, alle Jahre, alle BULA
# einlesen IQB018 - sheet "Abb_7.11"
d_m_bildung <- readxl::read_excel("data-raw/raw/IQB018_Abb7.11 (S.172)_2021.xlsx", sheet = "Abb_7.11")

# Mathe Mittel Einwanderung, alle Jahre, alle BULA
# einlesen IQB019 - sheet "Abb_8.9"
d_m_migra <- readxl::read_excel("data-raw/raw/IQB019_Abb8.9 (S.200)_2021.xlsx", sheet = "Abb_8.9")


## Datensätze aufbereiten --------------------------------

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
d_m_gen_2021 <- d_m_gen_2021 %>% dplyr::select(-...2)
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
    stringr::str_detect(geschlecht, "Mädc") ~"Mädchen",
    stringr::str_detect(geschlecht, "Jung") ~ "Jungen"
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


## Datensätze zusammenfügen  -----------------------------------------------

#Zusammenfügen
iqb_score <- rbind(d_m_ges, d_m_gen_2021, d_m_gen_2011_16, d_m_bildung, d_m_migra)

#Spalten sortieren
iqb_score <- iqb_ges[,c("indikator", "geschlecht", "region", "jahr", "wert")]

usethis::use_data(iqb_score, overwrite = T)

