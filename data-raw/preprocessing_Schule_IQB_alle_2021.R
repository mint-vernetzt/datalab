################################################################################
#
# Data Lab
# Vorbereitung Datensatz: IQB Daten 4. Klasse (2011, 2016, 2021) und 9. Klasse (2012, 2018)
# Author: Katharina Brunner, April 2023
#
################################################################################

library(dplyr)

# Rohdatensatz einlesen ------------------------------------------------------

wd <- getwd()
setwd(wd)

# Mathe Mittel ings. nur DE 2021
# einlesen IQB016 - sheet "Abb4.7"
d_m_ges <- readxl::read_excel("IQB016_Abb4.7&4.14 (S.94&107)_2021.xlsx", sheet = "Abb4.7")

#Mathe Mittel M und J 2021 inkl. Bundesländer
# einlesen IQB017 - sheet "Abb6.2"
d_m_gen_2021 <- readxl::read_excel("IQB017_Abb6.1&6.2 (S.131&135)_2021.xlsx", sheet = "Abb6.2")

#Mathe Mittel M und J 2011, 2016 nur DE verfügbar
# einlesen IQB021 - sheet "Tab6.1"
d_m_gen_2011_16 <- readxl::read_excel("IQB021_Tab6.1,6.2&6.6 (S.128,133&146)_2021.xlsx", sheet = "Tab6.1")

# Mathe Mittel Bildung hoch/niedrig, alle Jahre, alle BULA
# einlesen IQB018 - sheet "Abb_7.11"
d_m_bildung <- readxl::read_excel("IQB018_Abb7.11 (S.172)_2021.xlsx", sheet = "Abb_7.11")

# Mathe Mittel Einwanderung, alle Jahre, alle BULA
# einlesen IQB019 - sheet "Abb_8.9"
d_m_migra <- readxl::read_excel("IQB019_Abb8.9 (S.200)_2021.xlsx", sheet = "Abb_8.9")


# Datensätze aufbereiten --------------------------------

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
    stringr::str_detect(indikator, "hoch") ~ "status_hoch",
    stringr::str_detect(indikator, "niedr") ~ "status_niedrig"
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
    stringr::str_detect(indikator, "keine") ~ "keine Migrationsgeschichte",
    TRUE ~ "Migrationsgeschichte" #bedeutet hier beide Eltern migriert, Extremwerte verglichen
  ))

d_m_migra$geschlecht <- "gesamt"

# wert als numerisch
d_m_migra$wert <- as.numeric(d_m_migra$wert)

# üs einfügen
d_m_migra$region[d_m_migra$region == "Baden-Wuerttemberg"] <- "Baden-Württemberg"
d_m_migra$region[d_m_migra$region == "Thueringen"] <- "Thüringen"


# Datensätze zusammenfügen  -----------------------------------------------

#Zusammenfügen
iqb_ges <- rbind(d_m_ges, d_m_gen_2021, d_m_gen_2011_16, d_m_bildung, d_m_migra)

#Spalten sortieren
iqb_ges <- iqb_ges[,c("indikator", "geschlecht", "region", "jahr", "wert")]

usethis::use_data(iqb_ges, overwrite = T)

