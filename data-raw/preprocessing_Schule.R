# Akronym übergeben für Datensatz-Pfad in Onedrive
akro <- "kab"

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

pfad <- paste0("C:/Users/", akro,
               "/OneDrive - Stifterverband/MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/02 Datenmaterial/01_Rohdaten/02_Alle Daten/")

pfad_kek <- "C:/Users/kab/OneDrive - Stifterverband/AP7 MINT-DataLab/02 Datenmaterial/01_Rohdaten/02_Alle Daten/"

pfad <- pfad_kek

data_LK_D  <- read_data(paste0(pfad, "KMK023_Aus_Kurse_2021_Werte.xlsx"), "D" , 2021, "Leistungskurse")
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

data_z <- readxl::read_excel(paste0(pfad,"Kurse_21_10_22.xlsx"), col_names = T)

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

## prep fuktionen übernehmen ----

states_east_west <- list(west = c("Baden-Württemberg", "Bayern", "Bremen", "Hamburg",
                                  "Hessen", "Niedersachsen", "Nordrhein-Westfalen",
                                  "Rheinland-Pfalz", "Saarland", "Schleswig-Holstein"),
                         east = c("Brandenburg", "Mecklenburg-Vorpommern", "Sachsen",
                                  "Sachsen-Anhalt", "Thüringen", "Berlin"))

# prep_kurse_east_west

df_incl <- kurse

# create dummy variable to indicate east or west
df_incl$dummy_west <- ifelse(df_incl$region %in% states_east_west$west & df_incl$region != "Deutschland", "Westen", NA)
df_incl$dummy_west <- ifelse(df_incl$region %in% states_east_west$east & df_incl$region != "Deutschland", "Osten", df_incl$dummy_west)
df_incl <- na.omit(df_incl)# ifelse erstellt nochmal DE mit "NA" als region-Namen -->löschen

# aggregate values
df_incl <- df_incl %>% dplyr::group_by(jahr, anzeige_geschlecht, indikator, fachbereich, dummy_west, bereich) %>%
  dplyr::summarise(wert = sum(wert, na.rm = T))

names(df_incl)[5] <- "region"

#if(type == "subjects"){

  # df_incl <-  df_incl %>%
  #   dplyr::group_by(region, fachbereich, indikator, jahr) %>%
  #   dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"] +
  #                   wert[anzeige_geschlecht == "Männer"])

  df_incl <- df_incl[, colnames(kurse)]

  kurse <- rbind(kurse, df_incl)

# } else {
#   # # calcualte new "Gesamt"
#   #  df_incl <-  df_incl %>%
#   #    dplyr::group_by(region, fachbereich, indikator, jahr) %>%
#   #    dplyr::mutate(props = wert[anzeige_geschlecht == "Frauen"] +
#   #                    wert[anzeige_geschlecht == "Männer"])
#
#   df_incl <- df_incl[, colnames(kurse)]
#
#
#   kurse <- rbind(kurse, df_incl)
#
# }

# share_mint_kurse
  ## Datensatz so anpassen, dass Funktion nicht mehr benötigt wird

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

# USE ----
usethis::use_data(kurse, overwrite = T)



# Erstellt "iqb" -------------------------------------------------

#
# Data Lab
# Vorbereitung Datensatz: IQB Daten 4. Klasse (2011, 2016, 2021)
# Author: Katharina Brunner, April 2023
#

library(dplyr)


## iqb_standard Teil -------------------------------------------------------


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
d_m_gen <- readxl::read_excel(paste0(pfad, "data-raw/raw/IQB011_Abb7.9-7.15 (S.253-256).xlsx"), sheet = "Abb 7.9_Mathe_GL")
# Bio
d_b_gen <- readxl::read_excel(paste0(pfad, "data-raw/raw/IQB011_Abb7.9-7.15 (S.253-256).xlsx"), sheet = "Abb 7.10_BF")
# Chemie
d_c_gen <- readxl::read_excel(paste0(pfad, "data-raw/raw/IQB011_Abb7.9-7.15 (S.253-256).xlsx"), sheet = "Abb 7.12_CF")
# Physik
d_p_gen <- readxl::read_excel(paste0(pfad, "data-raw/raw/IQB011_Abb7.9-7.15 (S.253-256).xlsx"), sheet = "Abb 7.14_PF")

# Fachkenntnisse nach Status
# Mathe
d_m_stat <- readxl::read_excel(paste0(pfad, "data-raw/raw/IQB012_Abb8.17-8.23 (S.284-290).xlsx"), sheet = "Abb8.17_GL")
# Bio
d_b_stat <- readxl::read_excel(paste0(pfad, "data-raw/raw/IQB012_Abb8.17-8.23 (S.284-290).xlsx"), sheet = "Abb8.18_BF")
# Chemie
d_c_stat <- readxl::read_excel(paste0(pfad, "data-raw/raw/IQB012_Abb8.17-8.23 (S.284-290).xlsx"), sheet = "Abb8.20_CF")
# Physik
d_p_stat <- readxl::read_excel(paste0(pfad, "data-raw/raw/IQB012_Abb8.17-8.23 (S.284-290).xlsx"), sheet = "Abb8.22_PF")

# Fachkenntnisse nach Zuwanderungsgeschichte
# Mathe
d_m_migra <- readxl::read_excel(paste0(pfad, "data-raw/raw/IQB013_Abb9.2-9.8 (S.305-315).xlsx"), sheet = "Abb 9.2 GL")
# Bio
d_b_migra <- readxl::read_excel(paste0(pfad, "data-raw/raw/IQB013_Abb9.2-9.8 (S.305-315).xlsx"), sheet = "Abb 9.3 BF")
# Chemie
d_c_migra <- readxl::read_excel(paste0(pfad, "data-raw/raw/IQB013_Abb9.2-9.8 (S.305-315).xlsx"), sheet = "Abb 9.5 CF")
# Physik
d_p_migra <- readxl::read_excel(paste0(pfad, "data-raw/raw/IQB013_Abb9.2-9.8 (S.305-315).xlsx"), sheet = "Abb 9.7 PF")


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


# PISA Int'l. ----

# kab
# Aug23

## Alle ----

file_path <- paste0("C:/Users/", akro, "/OneDrive - Stifterverband/AP7 MINT-DataLab/02 Datenmaterial/01_Rohdaten/02_Alle Daten")

pat_kek <- "C:/Users/kab/OneDrive - Stifterverband/MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/02 Datenmaterial/01_Rohdaten/02_Alle Daten"

file_path <- path_kek

dat_pisa_g <- read_xls(paste0(file_path, "/", "PISA003_Länderscores_Immigration_Books.xls"), sheet = 3)

sub_cindex <- which(stringr::str_detect(dat_pisa_g[,everything(dat_pisa_g)], "mathematics|science"))
sub_pisa_g <- dat_pisa_g[,sub_cindex]
sub_pisa_g <- sub_pisa_g %>%
  stringr::str_extract(., "mathematics|science")

coln_cindex <- which(stringr::str_detect(dat_pisa_g [,everything(dat_pisa_g )], "Year/Study"))

coln_rindex <- which(dat_pisa_g[,coln_cindex]=="Year/Study")

dat_pisa_g1 <- dat_pisa_g%>%
  slice((coln_rindex-1):nrow(.))

coln_annex <- dat_pisa_g1%>%
  slice(1)%>%
  as.vector()%>%
  unname()%>%
  unlist()%>%
  zoo::na.locf(na.rm = F)

colnames(dat_pisa_g1) <-paste0(dat_pisa_g1[2,], "_", coln_annex)
colnames(dat_pisa_g1) <- gsub("_NA", "", colnames(dat_pisa_g1))

dat_pisa_g2<- dat_pisa_g1[-c(1:2),]

dat_pisa_g3 <- dat_pisa_g2 %>%
  mutate(across(`Year/Study`, ~ zoo::na.locf(.)))

dat_pisa_g4 <- dat_pisa_g3 %>%
  tidyr::pivot_longer(-c("Jurisdiction", "Year/Study"),
                      names_to ="platzhalter", values_to = "wert")%>%
  tidyr::separate_wider_delim(platzhalter, delim="_", names=c("typ","indikator"))%>%
  mutate(fach=sub_pisa_g)%>%
  mutate(fach=case_when(fach=="mathematics" ~ "Mathematik",
                        fach== "science" ~ "Naturwissenschaften",
                        T ~ .$fach)) %>%
  rename(jahr = "Year/Study", land = Jurisdiction)

# dat_pisa_g4 <- dat_pisa_g3 %>%
#   tidyr::pivot_longer(c("Average_All students", "Standard Error_All students"),
#                       names_to ="platzhalter", values_to = "wert")%>%
#   tidyr::separate_wider_delim(platzhalter, delim="_", names=c("typ","indikator"))%>%
#   mutate(fach=sub_pisa_g)
#
#
# dat_pisa_g4$Jurisdiction <- countrycode::countrycode(dat_pisa_g4$Jurisdiction, origin = 'country.name',destination = 'country.name', custom_match = c("International Average (OECD)" = "OECD Durchschnitt"))


pisa_extract <- function(pisa_list_dat, pisa_list_sheeet) {

  # setting path

  dat_pisa_g <- read_xls(paste0(file_path, "/",pisa_list_dat ), sheet = pisa_list_sheeet)

  # fach auslesen
  sub_cindex <- which(stringr::str_detect(dat_pisa_g[,everything(dat_pisa_g)], "mathematics|science"))
  sub_pisa_g <- dat_pisa_g[,sub_cindex]
  sub_pisa_g <- sub_pisa_g %>%
    stringr::str_extract(., "mathematics|science")

  coln_cindex <- which(stringr::str_detect(dat_pisa_g [,everything(dat_pisa_g )], "Year/Study"))

  coln_rindex <- which(dat_pisa_g[,coln_cindex]=="Year/Study")

  dat_pisa_g1 <- dat_pisa_g%>%
    slice((coln_rindex-1):nrow(.))

  coln_annex <- dat_pisa_g1%>%
    slice(1)%>%
    as.vector()%>%
    unname()%>%
    unlist()%>%
    zoo::na.locf(na.rm = F)

  colnames(dat_pisa_g1) <-paste0(dat_pisa_g1[2,], "_", coln_annex)
  colnames(dat_pisa_g1) <- gsub("_NA", "", colnames(dat_pisa_g1))

  dat_pisa_g2<- dat_pisa_g1[-c(1:2),]

  dat_pisa_g3 <- dat_pisa_g2 %>%
    mutate(across(`Year/Study`, ~ zoo::na.locf(.)))
  # %>%
  #   mutate(across(-c(`Year/Study`, `Jurisdiction`),~ stringr::str_remove(., "\\footnotesize")))

  # Hier versuchen Zeug rauszunehmen ^

  dat_pisa_g4 <- dat_pisa_g3 %>%
    tidyr::pivot_longer(-c("Jurisdiction", "Year/Study"),
                        names_to ="platzhalter", values_to = "wert")%>%
    tidyr::separate_wider_delim(platzhalter, delim="_", names=c("typ","indikator"))%>%
    mutate(fach=sub_pisa_g)%>%
    mutate(fach=case_when(fach=="mathematics" ~ "Mathematik",
                          fach== "science" ~ "Naturwissenschaften",
                          T ~ .$fach)) %>%
    rename(jahr = "Year/Study", land = Jurisdiction)


  dat_pisa_g4$land <- countrycode::countrycode(dat_pisa_g4$land, origin = 'country.name',destination = 'country.name.de', custom_match = c("International Average (OECD)" = "OECD Durchschnitt"))

  dat_pisa_g4$source <- pisa_list_dat

  dat_pisa_g5 <- dat_pisa_g4 %>%
    filter(jahr %in% c("2000", "2003", "2006", "2009", "2012", "2015","2018"))

  return(dat_pisa_g5)

}


pisa_list_dat <- c("PISA001_Länderscore_Insgesamt_Gender.xls",
                   "PISA001_Länderscore_Insgesamt_Gender.xls",
                   "PISA001_Länderscore_Insgesamt_Gender.xls",
                   "PISA001_Länderscore_Insgesamt_Gender.xls",
                   "PISA002_Länderscores_Edu_Parents.xls",
                   "PISA002_Länderscores_Edu_Parents.xls",
                   "PISA003_Länderscores_Immigration_Books.xls",
                   "PISA003_Länderscores_Immigration_Books.xls",
                   "PISA003_Länderscores_Immigration_Books.xls",
                   "PISA003_Länderscores_Immigration_Books.xls",
                   "PISA004_Länderscores_Income_Computer.xls",
                   "PISA004_Länderscores_Income_Computer.xls",
                   "PISA004_Länderscores_Income_Computer.xls",
                   "PISA004_Länderscores_Income_Computer.xls")


pisa_list_sheeet <- c(3,4,5,6,2,3,3,4,5,6,3,4,5,6)

pisa_list_output <- purrr::map2(.x = pisa_list_dat, .y =pisa_list_sheeet, .f=pisa_extract )

pisa_list_output <- purrr::list_rbind(pisa_list_output)

pisa_list_output1 <- pisa_list_output %>%
  filter(!wert %in% c("†"))%>%
  mutate(wert = as.numeric(wert),
         typ= case_when(typ == "Average" ~ "Druchschnitt",
                        typ =="Standard Error" ~ "Standardfehler",
                        T ~ .$typ))%>%
  mutate(ordnung = case_when(
    indikator %in% c("101-200 books" , "0-10 books" ,
      "11-25 books" , "201-500 books" , "26-100 books",
      "More than 500 books", "None") ~ "Bücher im Haushalt",
    indikator %in% c( "Less than [$A]",
                      "[$A] or more but less than [$B]",
                      "[$B] or more but less than [$C]",
                      "[$C] or more but less than [$D]",
                      "[$D] or more but less than [$E]",
                      "[$E] or more") ~ "Haushaltseinkommen",
    indikator %in% c("All students", "Female", "Male")~ "Ländermittel",
    indikator %in% c("First-Generation", "Second-Generation", "Native") ~ "Migrationshintergrund",
    indikator %in% c("ISCED 1", "ISCED 3A, ISCED 4", "ISCED 5A, 6",
                     "ISCED 2", "ISCED 3B, C", "ISCED 5B") ~ "Bildungshintergrund",
    indikator %in% c("Yes", "No") ~ "Computerverfügbarkeit"
  ))

pisa_list_output2 <- pisa_list_output1%>%
  select(-source)

pisa1 <- pisa_list_output2 %>%
  filter(ordnung == "Ländermittel")%>%
  rename(geschlecht = indikator, indikator = ordnung)%>%
  mutate(geschlecht = case_when(geschlecht=="All students" ~ "Insgesamt",
                                geschlecht=="Female" ~ "Weiblich",
                                geschlecht=="Male" ~ "Männlich"))%>%
  mutate(ausprägung = geschlecht)

pisa1d <- pisa1 %>%
  janitor::get_dupes(-wert)

pisa2  <- pisa_list_output2 %>%
  filter(ordnung != "Ländermittel")%>%
  mutate(geschlecht = "Insgesamt")%>%
  rename(ausprägung = indikator, indikator = ordnung)%>%
  mutate(ausprägung = case_when( ausprägung =="0-10 books" ~ "0-10",
                                 ausprägung =="101-200 books" ~ "101-200",
                                 ausprägung =="11-25 books" ~ "11-25",
                                 ausprägung =="201-500 books" ~ "201-500",
                                 ausprägung =="26-100 books" ~ "26-100",
                                 ausprägung =="First-Generation" ~ "Erste Generation",
                                 ausprägung =="ISCED 1" ~ "Primarbereich" ,
                                 ausprägung =="ISCED 2" ~  "Sekundarbereich I",
                                 ausprägung =="ISCED 3A, ISCED 4" ~ "Sekundarbereich II (Allgemeinbildend), Postsekundäre Bildung",
                                 ausprägung =="ISCED 3B, C" ~ "Sekundarbereich II (Berufsgebunden), Sekundarbereich II (Nicht Weiterführend)",
                                 ausprägung =="ISCED 5A, 6"~ "Tertiärbereich (Erste Stufe, außer Praxisgebunden), Tertiärbereich (Forschungsqualifikation)",
                                 ausprägung =="ISCED 5B" ~ "Teriärbereich (Erste Stufe, Praxisgebunden)",
                                 ausprägung =="More than 500 books" ~ "Mehr als 500",
                                 ausprägung =="Native" ~ "Keiner",
                                 ausprägung =="Yes" ~ "Verfügbar",
                                 ausprägung =="No" ~ "Nicht Verfügbar",
                                 ausprägung =="Second-Generation" ~ "Zweite Generation",
                                 ausprägung =="None" ~ "Keine",
                                 ausprägung =="or more" ~ "Mehr",
                                 T ~ .$ausprägung))


pisa <- bind_rows(pisa1, pisa2)%>%
  mutate(alter_n = "15 Jahre")%>%
  rename(bereich = indikator, indikator = ausprägung)%>%
  select(-geschlecht)%>%
  pivot_wider(names_from = typ, values_from = wert )%>%
  rename(wert = Druchschnitt)%>%
  mutate(typ = "Durchschnitt")



usethis::use_data(pisa, overwrite = T)


# TIMSS ----

# kab
# Sep 23

## TIMSS Acheivement----

### Gender ----
file_path <- paste0("C:/Users/", akro, "/OneDrive - Stifterverband/AP7 MINT-DataLab/02 Datenmaterial/01_Rohdaten/02_Alle Daten")


# funktion
timss_gender_transform <- function(dat){

  timss_g <- read_xlsx(paste0(file_path, "/", dat))

  timss_g_sub<- colnames(timss_g)[1]

  timss_g_sub1<- stringr::str_extract(timss_g_sub, "Mathematics|Science")

  timss_g1<- timss_g %>%
    slice(1:which(timss_g[1]==	"Benchmarking Participants")-1)%>%
    select(-c(1,2,4))

  timss_g_colanmes <- timss_g1%>%
    slice(which(timss_g1[1]==	"Country"))%>%
    as.vector()%>%
    unlist()%>%
    unname()%>%
    zoo::na.locf()

  timss_g_colanmes2<-timss_g1%>%
    filter(if_any(everything(), ~ .=="Girls"))%>%
    as.vector()%>%
    unlist()%>%
    unname()%>%
    zoo::na.locf(na.rm = F)

  timss_g_colanmes3 <-timss_g1%>%
    filter(if_any(everything(), ~ .=="Girls"))%>%
    mutate(across(everything(), ~case_when(
      is.na(.)~"Signifikanz",
    )))%>%
  as.vector()%>%
    unlist()%>%
    unname()


  k <- paste0(timss_g_colanmes, "_", timss_g_colanmes2, "_", timss_g_colanmes3)

  k <- str_remove_all(k, "_NA")

  colnames(timss_g1) <- k

  colnames(timss_g1)[1] <- "land"

  timss_g2 <- timss_g1%>%
    slice(which(if_any(everything(), ~. == "Girls"))+1 : nrow(.))%>%
    pivot_longer(-"land", values_to = "dummy", names_to = "indikator")%>%
    separate(indikator, c ("jahr", "geschlecht", "indikator"), sep = "_")%>%
    mutate(indikator = case_when(is.na(indikator)~ "Achievement",
                                 T~.$indikator))%>%
    rename(wert = dummy )%>%
    mutate(indikator= case_when(
      indikator == "Achievement" ~ "Score",
      T~.$indikator
      ))

  timss_g2$fach <- timss_g_sub1

  timss_g3 <- timss_g2 %>%
    rename(typ = indikator, indikator = geschlecht)%>%
    mutate(fach = case_when(
      fach == "Mathematics"~ "Mathematik",
      fach == "Science" ~ "Naturwissenschaften"),
      indikator=case_when(indikator == "Boys" ~ "Jungen",
                          indikator == "Girls" ~ "Mädchen",
                          T ~ .$indikator))%>%
    pivot_wider(names_from = typ, values_from = wert)%>%
    rename("Test-Punktzahl" = Score, signifikant = Signifikanz)%>%
    pivot_longer(`Test-Punktzahl`, names_to = "typ", values_to = "wert")%>%
    mutate(signifikant= case_when(signifikant == "p" ~ "Ja",
                                  T ~ NA),
           ordnung = "Gender")%>%
    rename("signifikant höher gegenüber anderem geschlecht" = signifikant)



  timss_g3$land <- countrycode::countrycode(timss_g3$land, origin = 'country.name', destination = 'country.name.de', custom_match = c('England' = 'England',
                                                                                                                                      "Northern Ireland" = "Nordirland") )
  return(timss_g3)
}

l <- timss_gender_transform("TIMSS002_achievement-gender-trends-M4.xlsx")
k <- timss_gender_transform("TIMSS005_achievement-gender-trends-S4.xlsx")

timss_gender <- bind_rows(l,k)

### Achievement ----

# funktion

timss_achv_extract<-function(dat){
  dat1 <- read_excel(paste0(file_path, "/", dat))

  timss_a_sub<- colnames(dat1)[1]

  timss_a_sub1<- stringr::str_extract(timss_a_sub, "Mathematics|Science")

  timss_a1<- dat1 %>%
    slice(1:which(dat1[1]==	"Benchmarking Participants")-1)%>%
    janitor::remove_empty("cols")%>%
    select(-1)

  timss_a_colnames1 <- timss_a1 %>%
    slice(which(if_any(everything(), ~ .=="Country")))%>%
    as.vector()%>%
    unlist()%>%
    unname()%>%
    zoo::na.locf()


  timss_a_colnames2 <- timss_a1 %>%
    slice(which(if_any(everything(), ~ .=="Country")))%>%
    mutate(across(everything(), ~case_when(
      is.na(.)~"standardfehler"
    )))%>%
    as.vector()%>%
    unlist()%>%
    unname()

  timss_a_colnames_fn <- paste0(timss_a_colnames1, "_", timss_a_colnames2)%>%
    str_remove_all(., "_NA")

  colnames(timss_a1) <- timss_a_colnames_fn


   timss_a2 <- timss_a1 %>%
    filter(!is.na(Country))%>%
    filter(Country != "Country")%>%
    pivot_longer(-c(`Country`), names_to="indikator", values_to="dummy")%>%
    separate_wider_delim(indikator, delim = "_", names=c("jahr", "indikator"), too_few = "align_start")%>%
    mutate(indikator=case_when(is.na(indikator)~"wert",
                               T~.$indikator))%>%
    pivot_wider(names_from = indikator, values_from = dummy)%>%
    rename(land = Country)%>%
    mutate(indikator = "Score")

   timss_a2$land <- countrycode::countrycode(timss_a2$land, origin = 'country.name', destination = 'country.name.de', custom_match = c('England' = 'England',
                                                                                                                                       "Northern Ireland" = "Nordirland") )

   timss_a2$fach <-  timss_a_sub1

   timss_a3 <- timss_a2 %>%
     mutate(fach = case_when(
       fach == "Mathematics" ~ "Mathematik",
       fach== "Science" ~ "Naturwissenschaften"
     ),
    indikator = "Insgesamt",
    typ= "Test-Punktzahl",
    ordnung = "Achievement")





  return(timss_a3)

}

j <- timss_achv_extract("TIMSS001_achievement-trends-M4.xlsx")
h <- timss_achv_extract("TIMSS004_achievement-trends-S4.xlsx")

timss_achievement <- bind_rows(j,h)

### Home resources ----


# funktion

timss_res_extract <- function(dat3, jahr, fach){

  file_path <- paste0("C:/Users/", akro, "/OneDrive - Stifterverband/AP7 MINT-DataLab/02 Datenmaterial/01_Rohdaten/02_Alle Daten")


  dat2 <- read_excel(paste0(file_path, "/", dat3))

  # timss_res_sub<- colnames(dat2)[1]
  #
  # timss_res_sub<- stringr::str_extract(timss_res_sub, "Mathematics|Science")

  timss_res<- dat2 %>%
    slice(1:which(dat2[1]==	"Benchmarking Participants")-1)%>%
    select(-c(1,2,4))%>%
    janitor::remove_empty()

  timss_res_coln <- timss_res %>%
    filter(if_any(everything(),~ . =="Country"))%>%
    as.vector()%>%
    unlist()%>%
    unname()



  bem <- which(str_detect(timss_res_coln, "Country"))

  timss_res_coln[bem+1]<- "bemerkung"

  timss_res_coln1 <- zoo::na.locf(timss_res_coln)

  timss_res_coln2 <- timss_res %>%
    slice(which(if_any(everything(),~. =="Percent of Students")))%>%
    as.vector()%>%
    unlist()%>%
    unname()%>%
    zoo::na.locf(na.rm = F)

  timss_res_coln2 <- rev(timss_res_coln2)

  timss_res_coln1 <- rev(timss_res_coln1)

  timss_res_coln2[1:2] <- timss_res_coln1[1:2]

  timss_res_coln2 <- rev(timss_res_coln2)

  timss_res_coln1 <- rev(timss_res_coln1)


  timss_res_coln3 <- timss_res %>%
    slice(which(if_any(everything(),~. =="Percent of Students")))%>%
    mutate(across(-c("...3", "...5","...25"),~case_when(
      is.na(.)~"standardfehler"
    )))%>%
    as.vector()%>%
    unlist()%>%
    unname()


  timss_res_coln_fn <- paste0(timss_res_coln1,"_",timss_res_coln2, "_" ,timss_res_coln3)

  colnames(timss_res) <-timss_res_coln_fn

  timss_res2 <- timss_res %>%
    filter(!if_any(everything(), ~ .%in%c("Percent of Students", "Country")))

  colnames(timss_res2) <- str_remove(colnames(timss_res2), "_NA")
  colnames(timss_res2) <- str_remove(colnames(timss_res2), "_NA")

  timss_res2 <- timss_res2 %>%
    rename(land = Country)

  timss_res3 <- timss_res2 %>%
    pivot_longer(-c(land, bemerkung), values_to = "wert", names_to="indikator")%>%
    separate(indikator, sep="_", into=c("gruppe", "indikator", "typ"))%>%
    mutate(typ = case_when(
      is.na(typ) ~"wert",
      T~.$typ))%>%
    pivot_wider(names_from = typ, values_from = wert)%>%
    mutate("fach" = fach,
           "jahr" =jahr)%>%
    mutate(across(c("wert", "standardfehler"), ~ as.numeric(.)))%>%
    rename(typ = indikator, indikator = gruppe, fußnote_zu_n = bemerkung)%>%
    mutate(typ = case_when(typ == "Average Achievement" ~ "Gemittelte Test-Punktzahl",
                           typ == "Percent of Students" ~ "Prozentsatz der Schüler:innen",
                           T ~ .$typ),
           indikator = case_when(indikator =="Some Resources"~ "Einige Ressourcen",
                                 indikator =="Few Resources"~ "Wenige Ressourcen",
                                 indikator =="Many Resources"~ "Viele Ressourcen",
                                 T~ .$indikator),
           ordnung = "Ressourcen")

  timss_res3$land <- countrycode::countrycode(timss_res3$land, origin = 'country.name', destination = 'country.name.de', custom_match = c('England' = 'England',
                                                                                                                                      "Northern Ireland" = "Nordirland",
                                                                                                                                      "International Average" = "Interantionaler Durchschnitt") )

  return(timss_res3)
}

o <- timss_res_extract("TIMSS007_home-resources-M4.xlsx", "2019", "Mathematik")
p <- timss_res_extract("TIMSS008_home-resources-S4.xlsx" ,"2019", "Naturwissenschaften")


timss_res_dat <- bind_rows(o,p)

### Benchmarks-----


# funktion

timss_benchmarks_extract <- function(dat7){
  file_path <- paste0("C:/Users/", akro, "/OneDrive - Stifterverband/AP7 MINT-DataLab/02 Datenmaterial/01_Rohdaten/02_Alle Daten")


  dat8 <- read_excel(paste0(file_path, "/", dat7))

  timss_b_sub<- colnames(dat8)[1]

  timss_b_sub1<- stringr::str_extract(timss_b_sub, "Mathematics|Science")

  dat_bench <- dat8 %>%
    slice(1:which(if_any(everything(),~ str_detect(.,"Benchmarking Participants")))-1)%>%
    janitor::remove_empty("cols")

  dat_bench_coln_1 <- dat_bench %>%
    filter(if_any(everything(), ~.== "Country"))%>%
    as.vector()%>%
    unlist()%>%
    unname()%>%
    zoo::na.locf()

  dat_bench_coln_2 <- dat_bench %>%
    filter(if_any(everything(), ~ str_detect(., "Percent")))%>%
    as.vector()%>%
    unlist()%>%
    unname()%>%
    zoo::na.locf(na.rm = F)

  dat_bench_coln_3 <- dat_bench %>%
    filter(if_any(everything(), ~ str_detect(., "2019")))%>%
    as.vector()%>%
    unlist()%>%
    unname()%>%
    zoo::na.locf(na.rm = F)

  dat_bench_coln_4 <- dat_bench %>%
    filter(if_any(everything(), ~ str_detect(., "2019")))%>%
    mutate(across(everything(), ~ case_when(
      is.na(.) ~ "signifikanz"
    )))%>%
    as.vector()%>%
    unlist()%>%
    unname()

  dat_bench_coln_4[1] <- NA


  dat_bench_coln_fn <- paste0(dat_bench_coln_1,"_", dat_bench_coln_2,"_", dat_bench_coln_3,"_", dat_bench_coln_4 )

  dat_bench_coln_fn <- str_remove_all(dat_bench_coln_fn, "_NA")

  colnames(dat_bench) <- dat_bench_coln_fn

  dat_bench1a <<- dat_bench %>%
    slice(which(if_any(everything(),~ str_detect(., "2019")))+1:nrow(.))%>%
    select(Country, contains("signifikanz"))%>%
    pivot_longer(-Country, names_to="indikator", values_to = "signifikanz")%>%
    separate(indikator, into = c("indikator", "typ", "jahr"), sep="_")

  dat_bench1b <<- dat_bench %>%
    slice(which(if_any(everything(),~ str_detect(., "2019")))+1:nrow(.))%>%
    select(Country, !contains("signifikanz"))%>%
    mutate(across(-c("Country"), ~ as.numeric(.)))%>%
    pivot_longer(-Country, names_to="indikator", values_to = "wert")%>%
    separate(indikator, into = c("indikator", "typ", "jahr" ), sep="_")

  dat_bench2 <<- dat_bench1b %>%
     left_join(dat_bench1a) %>%
    rename(land = Country, signifikant = signifikanz)%>%
    mutate(typ = case_when(typ=="Percent of Students"~ "Prozentsatz der Schüler:innen",
                           T ~ .$typ),
           ordnung = "Benchmarks",
           bereich = "Schule",
           signifikant = case_when(signifikant == "p"~ "Höher",
                                   signifikant == "s" ~ "Niedriger",
                                   T ~ .$signifikant),
           indikator= case_when(indikator == "Advanced \r\nInternational Benchmark \r\n(625)" ~ "Höchster int'l. Maßstab (625)",
                                indikator == "High \r\nInternational Benchmark\r\n(550)" ~ "Hoher int'l. Maßstab (550)",
                                indikator == "Intermediate \r\nInternational Benchmark \r\n(475)" ~ "Mittlerer int'l. Maßstab (475)",
                                indikator == "Low \r\nInternational Benchmark \r\n(400)" ~ "Niedriger int'l. Maßstab (400)",
                                T ~ .$indikator))%>%
    rename( "atktuell signifikant" = signifikant)%>%
    mutate(fach =case_when(timss_b_sub1 == "Mathematics"~ "Mathematik",
                           timss_b_sub1 == "Science"~ "Naturwissenschaften"))

  dat_bench2$land <- countrycode::countrycode(dat_bench2$land,
                                              origin = 'country.name',
                                              destination = 'country.name.de',
                                              custom_match = c('England' = 'England',
                                                               "Northern Ireland" = "Nordirland") )


return(dat_bench2)
}


x <- timss_benchmarks_extract("TIMSS003_benchmarks-trends-M4.xlsx")
y <- timss_benchmarks_extract("TIMSS006_benchmarks-trends-S4.xlsx")

# y1 <- y %>%
#   select(1:4)%>%
#   unique()
#
# x1 <- x %>%
#   select(1:4)%>%
#   unique()
#
# d <- setdiff(x1,y1)

# Südafika ist neu dazugekommen; SA und Norw. nur 5 Landesteile

timss_benchmarks <- bind_rows(x,y)

### TIMSS Merger ----

timss <- bind_rows(
  timss_benchmarks %>% mutate(across(wert,~ as.numeric(.))),
  timss_res_dat %>% mutate(across(wert,~ as.numeric(.))),
  timss_achievement %>% mutate(across(wert,~ as.numeric(.))),
  timss_gender %>% mutate(across(wert,~ as.numeric(.)))
)

usethis::use_data(timss, overwrite=T)




