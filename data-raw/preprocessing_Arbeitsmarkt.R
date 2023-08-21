################################################################################
#
# Data Lab
# Vorbereitung Datensatz: Arbeitsmarkt Zeitreihe (Beschäftigte und Auszubildende)
# Author: Katharina Brunner, Juli 2023
# Quelle: C:\Users\kbr\OneDrive - Stifterverband\MINTvernetzt (SV)\MINTv_SV_AP7 MINT-DataLab\02 Datenmaterial\02_Prozess\Datenaufbereitung 2023\Arbeitsmarkt
#
################################################################################

# für Pfad zum Einlesen aus Onedrive
akro <- "kbr"
pfad <- paste0("C:/Users/", akro,
               "/OneDrive - Stifterverband/MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/02 Datenmaterial/01_Rohdaten/02_Alle Daten/")


# Erstellt "arbeitsmarkt" -------------------------------------------------

## Alten/neuen Datensatz einlesen ---------------------------------------------------

# Alternative:
# data_z <- readxl::read_excel(system.file(package="datalab", "data-raw/Arbeitsmarkt.xlsx"), col_names = T)

data_z <- readxl::read_excel("data-raw/raw/Arbeitsmarkt.xlsx", col_names = T)

# Alternative:
# data <- readxl::read_excel(system.file(package="datalab",
#                                        "data-raw/BA006_221123_Besch_MINT.xlsx"),
#                            sheet = "Auswertung", col_names = F, range = "A17:AK7576")


data <- readxl::read_excel(paste0(pfad, "BA006_221123_Besch_MINT.xlsx"),
                           sheet = "Auswertung", col_names = F, range = "A17:AK7576")

data_n <- readxl::read_excel(paste0(pfad, "BA009_230717_EA_344636_SvB_Azubi_MINT.xlsx"),
                             sheet = "Auswertung", col_names = F, range = "A16:AH7521")

# Spalten zusammenfassen/löschen für 2022
data$...1 <- dplyr::coalesce(data$...4, data$...3, data$...2, data$...1) # Regionen in eine Spalte
data$...5 <- dplyr::coalesce(data$...8, data$...7, data$...6, data$...5) # MINT/Niveau in eine Spalte
data <- data[,-c(2,3,4,6,7,8,9)] # nun überflüssige Spalten löschen

# Spalten zusammenfassen/löschen für 2023
data_n$...2 <- dplyr::coalesce(data_n$...5, data_n$...4, data_n$...3, data_n$...2) # MINT/Niveau in eine Spalte
data_n <- data_n[,-c(3,4,5,6)] # nun überflüssige Spalten löschen


## data von 2021 und data_n von 2022 haben selbe Form
## funktion für Aufbereitung


zeitreihe_aufbereitung <- function(data, jahr){

# Header ergänzen
header <- c("region", "fachbereich",
            #
            "Beschäftigte",
            "weibliche Beschäftigte",
            "Beschäftigte u25",
            "Beschäftigte ü55",
            #
            "Beschäftigte (nur SVB)",
            "weibliche Beschäftigte (nur SVB)",
            "Beschäftigte u25 (nur SVB)",
            "Beschäftigte ü55 (nur SVB)",
            "Auszubildende",
            "weibliche Auszubildende",
            #
            "Beschäftigte (nur GFB)",
            "weibliche Beschäftigte (nur GFB)",
            "Beschäftigte u25 (nur GFB)",
            "Beschäftigte ü55 (nur GFB)",
            #
            "ausländische Beschäftigte",
            "ausländische weibliche Beschäftigte",
            "ausländische Beschäftigte u25",
            "ausländische Beschäftigte ü55",
            #
            "ausländische Beschäftigte (nur SVB)",
            "ausländische weibliche Beschäftigte (nur SVB)",
            "ausländische Beschäftigte u25 (nur SVB)",
            "ausländische Beschäftigte ü55 (nur SVB)",
            "ausländische Auszubildende",
            "ausländische weibliche Auszubildende",
            #
            "ausländische Beschäftigte (nur GFB)",
            "ausländische weibliche Beschäftigte (nur GFB)",
            "ausländische Beschäftigte u25 (nur GFB)",
            "ausländische Beschäftigte ü55 (nur GFB)"
)
colnames(data) <- header


## Aufbereiten in gewünschte DF-Struktur ---------------------------------

#NA definieren anstelle */0
data <- data %>%
  dplyr::mutate_all(~replace(., . %in% c(0, "*"), NA))

#Fachbereich und Arbeitslevel trennen
data$anforderung <- ifelse(data$fachbereich %in% c("Helfer", "Fachkraft", "Spezialist",
                                                   "Experte", "keine Angabe"), data$fachbereich, "Gesamt")
data$fachbereich <- ifelse(data$fachbereich %in% c("Helfer", "Fachkraft", "Spezialist",
                                                   "Experte", "keine Angabe"), NA, data$fachbereich)

data$anforderung[data$anforderung=="keine Angabe"]<-"keine Zuordnung möglich"
data$fachbereich[data$fachbereich=="Insgesamt"]<-"Alle"
data$fachbereich[data$fachbereich=="MINT-Berufe"]<-"MINT"
data$fachbereich[data$fachbereich=="Technik"]<-"Technik (gesamt)"

# Lücken füllen, die durch Zellverbünde entstanden sind
data$region <- stats::ave(data$region, cumsum(!is.na(data$region)), FUN=function(x) x[1])
data$fachbereich <- stats::ave(data$fachbereich, cumsum(!is.na(data$fachbereich)), FUN=function(x) x[1])

# ins long-Format bringen
data <- data %>%
  tidyr::pivot_longer(cols = "Beschäftigte":"ausländische Beschäftigte ü55 (nur GFB)")


## nötige Variablen auswählen/ergänzen -----------------------------------------------

# Entferen von Variablen die (aktuell) nicht analysiert werden
data <- subset(data, !(name %in% c("Beschäftigte", "weibliche Beschäftigte", "Beschäftigte u25", "Beschäftigte ü55",
                                   "Beschäftigte u25 (nur GFB)", "Beschäftigte ü55 (nur GFB)",
                                   "ausländische Beschäftigte", "ausländische weibliche Beschäftigte", "ausländische Beschäftigte u25",
                                   "ausländische Beschäftigte ü55", "ausländische Beschäftigte u25 (nur GFB)",
                                   "ausländische Beschäftigte ü55 (nur GFB)")))
data <- data %>%
  dplyr::mutate(
    # quelle = "Bundesagentur für Arbeit, 2022: Auf Anfrage (Auftragsnummer 335970)",
    # hinweise = "eigene Berechnungen durch MINTvernetzt",
    bereich = "Arbeitsmarkt",
    jahr = jahr,
    geschlecht = dplyr::case_when(
      stringr::str_detect(data$name, "weiblich")~"Frauen",
      TRUE ~ "Gesamt"
    ),
    kategorie = dplyr::case_when(
      stringr::str_detect(data$name, "Auszubildende")~"Auszubildende",
      TRUE ~ "Beschäftigte"
    ),
    name = dplyr::case_when(
      name == "Beschäftigte (nur SVB)"~"Beschäftigte",
      name == "weibliche Beschäftigte (nur SVB)"~ "Beschäftigte",
      name == "Beschäftigte u25 (nur SVB)"~"Beschäftigte u25",
      name == "Beschäftigte ü55 (nur SVB)"~"Beschäftigte ü55",
      name == "weibliche Auszubildende"~"Auszubildende",
      name == "Beschäftigte (nur GFB)"~"in Minijobs",
      name == "weibliche Beschäftigte (nur GFB)"~"in Minijobs",
      name == "ausländische Beschäftigte (nur SVB)"~"ausländische Beschäftigte",
      name == "ausländische weibliche Beschäftigte (nur SVB)"~ "ausländische Beschäftigte",
      name == "ausländische Beschäftigte u25 (nur SVB)"~"ausländische Beschäftigte u25",
      name == "ausländische Beschäftigte ü55 (nur SVB)"~"ausländische Beschäftigte ü55",
      name == "ausländische weibliche Auszubildende"~"ausländische Auszubildende",
      name == "ausländische Beschäftigte (nur GFB)"~"ausländisch in Minijobs",
      name == "ausländische weibliche Beschäftigte (nur GFB)"~"ausländisch in Minijobs",
      TRUE ~ name
    )) %>%
  dplyr::rename(wert=value,
                indikator=name)


# Spalten in logische Reihenfolge bringen
data <- data[,c("bereich", "kategorie", "indikator", "fachbereich", "geschlecht", "region", "jahr", "anforderung", "wert"
                #, "hinweise", "quelle"
)]

}

data <- zeitreihe_aufbereitung(data = data, jahr = 2021)
data_n <- zeitreihe_aufbereitung(data = data_n, jahr = 2022)

## Datensätze zu zusammenlegen vorbereiten ---------------------------------

# Bei beiden einheitlich! Habs jetzt raus für beide (kbr)
data_z <- data_z %>%
  dplyr::select(-hinweise, -quelle)

# Namen aus alten DF aktualisieren/korrigieren
data_z <- data_z %>%
  dplyr::rename(anforderung = anforderungsniveau,
         geschlecht = anzeige_geschlecht,
         jahr = Jahr)

data_z$region[data_z$region=="Rheinland-Pflaz"]<-"Rheinland-Pfalz"
data_z[data_z$geschlecht == "frauen", "geschlecht"] <- "Frauen"
data_z[data_z$geschlecht == "gesamt", "geschlecht"] <- "Gesamt"
data_z[data_z$anforderung == "gesamt", "anforderung"] <- "Gesamt"

#neue Daten an alten Datensatz angelichen
data_k <- data
data_kn <- data_n

zeitreihe_angleichen <- function(data_k){

data_k <- data_k %>%
  dplyr::mutate(indikator = dplyr::case_when(
    indikator == "Beschäftigte" ~ "Beschäftigte",
    indikator == "Auszubildende"~"Auszubildende"),
    fachbereich = dplyr::case_when(
      fachbereich == "Alle"~"Alle",
      fachbereich == "MINT"~"MINT"),
    region = dplyr::case_when(
      region %in% c("Deutschland", "Baden-Württemberg", "Bayern", "Berlin", "Brandenburg",
                    "Bremen", "Hamburg", "Hessen", "Mecklenburg-Vorpommern", "Niedersachsen",
                    "Nordrhein-Westfalen", "Rheinland-Pfalz", "Saarland", "Sachsen", "Sachsen-Anhalt",
                    "Schleswig-Holstein", "Thüringen") ~ region
    )
  )
data_k <- na.omit(data_k)
data_k <- data_k %>%
  dplyr::select(-kategorie)

}

data_k <- zeitreihe_angleichen(data_k = data_k)
data_kn <- zeitreihe_angleichen(data_k = data_kn)

# Spalten in gleiche Reihenfolge bringen
data_z <- data_z[,c("bereich", "indikator", "fachbereich", "geschlecht", "region", "jahr", "anforderung", "wert"
                    #, "hinweise", "quelle"
)]


## Datensatz in data speichern  ------------------------------------------------------

arbeitsmarkt <- rbind(data_z, data_k, data_kn)
arbeitsmarkt$wert <- as.numeric(arbeitsmarkt$wert)

usethis::use_data(arbeitsmarkt, overwrite = T)



# Erstellt "arbeitsmarkt_detail" ------------------------------------------

## Rohdaten einlesen ---------------------------------------------------

# Einlesen Hauptdatensatz
# data <- readxl::read_excel(system.file(package="datalab",
#                                        "data-raw/BA006_221123_Besch_MINT.xlsx"),
#                            sheet = "Auswertung", col_names = F, range = "A17:AK7576")

# läuft seit neuem Laptop mit Code drüber nicht mehr druch, daher umgeschrieben (kbr)

library(magrittr)

# setwd("C:/Users/kab/Downloads/datalab/datalab/data-raw")
# path<-"C:/Users/kab/Downloads/datalab/datalab/data-raw/BA006_221123_Besch_MINT.xlsx"
# data <- readxl::read_excel("BA006_221123_Besch_MINT.xlsx",
#                            sheet = "Auswertung", col_names = F, range = "A17:AK7576")
#
data <- readxl::read_excel(paste0(pfad, "BA006_221123_Besch_MINT.xlsx"),
                           sheet = "Auswertung", col_names = F, range = "A17:AK7576")

data_n <- readxl::read_excel(paste0(pfad, "BA009_230717_EA_344636_SvB_Azubi_MINT.xlsx"),
                             sheet = "Auswertung", col_names = F, range = "A16:AH7521")

# Spalten zusammenfassen/löschen
data$...1 <- dplyr::coalesce(data$...4, data$...3, data$...2, data$...1) # Regionen in eine Spalte
data$...5 <- dplyr::coalesce(data$...8, data$...7, data$...6, data$...5) # Hilfspalte für MINT/Niveau gesamt, wird später getrennt

# Spalten zusammenfassen/löschen für 2023
data_n$...2 <- dplyr::coalesce(data_n$...5, data_n$...4, data_n$...3, data_n$...2) # MINT/Niveau in eine Spalte
#data_n <- data_n[,-c(3,4,5,6)] # nun überflüssige Spalten löschen

## Daten von 2021 region formatieren (kab)

data1 <- data %>%
  dplyr::mutate(bundesland=dplyr::case_when(
    ...1 == "Deutschland" ~ "Deutschland",
    ...1 == "Westdeutschland (o. Berlin)" ~"Westdeutschland (o. Berlin)",
    ...1 == "Ostdeutschland (einschl. Berlin)" ~ "Ostdeutschland (einschl. Berlin)",
    ...1 == "Baden-Württemberg" ~"Baden-Württemberg",
    ...1 == "Bayern" ~"Bayern",
    ...1 == "Berlin" ~"Berlin",
    ...1 == "Brandenburg" ~ "Brandenburg",
    ...1 == "Bremen" ~ "Bremen",
    ...1 == "Hamburg" ~"Hamburg",
    ...1 == "Hessen" ~"Hessen",
    ...1 == "Mecklenburg-Vorpommern" ~"Mecklenburg-Vorpommern",
    ...1 == "Niedersachsen" ~"Niedersachsen",
    ...1 == "Nordrhein-Westfalen" ~"Nordrhein-Westfalen",
    ...1 == "Rheinland-Pfalz" ~"Rheinland-Pfalz",
    ...1 == "Saarland" ~"Saarland",
    ...1 == "Sachsen-Anhalt" ~"Sachsen-Anhalt",
    ...1 == "Sachsen" ~"Sachsen",
    ...1 == "Schleswig-Holstein" ~"Schleswig-Holstein",
    ...1 == "Thüringen" ~"Thüringen"
  ))%>% tidyr::separate(...4, c("a","b","c"), sep = ",")%>% #reicht nicht ganz, müsste auch nach : separieren für Sachsen-Anhalt Kreise
  dplyr::rename(ort = a)

# für LKs von Sachsen-Anhalt Trennung mit :
data1 <- data1 %>%
  tidyr::separate(ort, c("ort", "d"), sep = ":")
data1$b <- ifelse(!is.na(data1$d), data1$d, data1$b)
data1 <- data1 %>% dplyr::select(-d)

# Trennen von Infromation und Schlüsselnummer für Städte in Sachsen-Anhalt & Thüringen an :
data1 <- data1 %>%
  tidyr::separate(b, c("b", "d"), sep = ":")
data1$c <- ifelse(!is.na(data1$d), data1$d, data1$c)
data1 <- data1 %>% dplyr::select(-d)

data1$bundesland <- zoo::na.locf(data1$bundesland)

# Schlüsselnummern in "c" eintragen, falls fehlen
data1$c <- ifelse(!grepl("[^A-Za-z]", data1$c), data1$b, data1$c)
data1$b <- ifelse(data1$b == data1$c, NA, data1$b)

# zwischen gleichnamigen Stadt- und Landkreisen unterscheiden
## Hilfsvarialbe, die Stadt/Landkreise, die es doppelt gibt, in Hilfs-String schreibt
help <- data.frame(table(data1$ort))
help <- help %>% dplyr::filter(Freq != 1)

# für die ausgewählten Fälle (-->%in% help) falls "Stadt" in näherer Bezeichnung in Spalte b vorkommt, Stadt vorschreiben, sonst Landkreis
data1$ort <- ifelse(data1$ort %in% help$Var1 & grepl("tadt", data1$b) , stringr::str_c("Stadt ", data1$ort), data1$ort)
data1$ort <- ifelse(data1$ort %in% help$Var1 & !grepl("tadt", data1$b), stringr::str_c("Landkreis ", data1$ort),data1$ort)

# Spezialfall Dillingen ist hier 2. Augsbrug - Name und Schlüsselnummer korrekt in c schreiben:
data1$ort <- ifelse(data1$c == "von 01.01.1973", stringr::str_c(data1$ort, " ", data1$c), data1$ort)
data1$c <- ifelse(data1$c == "von 01.01.1973", data1$b, data1$c)
data1$ort <- ifelse(grepl("von 01.01.1973",data1$ort), "Dillingen a. d. Donau", data1$ort)
data1$c <- ifelse(grepl("Dillingen",data1$ort), "09773", data1$c)
data1$b <- ifelse(grepl("Dillingen",data1$ort), NA, data1$b)

# Spezifalfall Oldenburg mit Beschreibung Oldenburg in Klammern, daher nicht erkannt als identisch in Ansatz davor
data1$ort <- ifelse(data1$ort == "Oldenburg (Oldenburg)", "Stadt Oldenburg", data1$ort)
data1$ort <- ifelse(data1$ort == "Oldenburg", "Landkreis Oldenburg", data1$ort)

# Spezialfall Eisenach - Leerzeichen vor Schlüsselnummer
data1$c <- ifelse(data1$ort == "Eisenach", 16056, data1$c)

data1 <- data1 %>%
  dplyr::rename(
    schluesselnummer = c,
    zusatz = b
  )

data <- data1[,-c(2,3,8:11)] # nun überflüssige Spalten löschen

## Daten von 2022 region formatieren
data_n1 <- data_n
data_n1$...3 <- data_n1$...1
data_n1 <- data_n1 %>%
  dplyr::mutate(bundesland=dplyr::case_when(
    ...1 == "Deutschland" ~ "Deutschland",
    ...1 == "Westdeutschland" ~"Westdeutschland (o. Berlin)",
    ...1 == "Ostdeutschland" ~ "Ostdeutschland (einschl. Berlin)",
    ...1 == "Baden-Württemberg" ~"Baden-Württemberg",
    ...1 == "Bayern" ~"Bayern",
    ...1 == "Berlin" ~"Berlin",
    ...1 == "Brandenburg" ~ "Brandenburg",
    ...1 == "Bremen" ~ "Bremen",
    ...1 == "Hamburg" ~"Hamburg",
    ...1 == "Hessen" ~"Hessen",
    ...1 == "Mecklenburg-Vorpommern" ~"Mecklenburg-Vorpommern",
    ...1 == "Niedersachsen" ~"Niedersachsen",
    ...1 == "Nordrhein-Westfalen" ~"Nordrhein-Westfalen",
    ...1 == "Rheinland-Pfalz" ~"Rheinland-Pfalz",
    ...1 == "Saarland" ~"Saarland",
    ...1 == "Sachsen-Anhalt" ~"Sachsen-Anhalt",
    ...1 == "Sachsen" ~"Sachsen",
    ...1 == "Schleswig-Holstein" ~"Schleswig-Holstein",
    ...1 == "Thüringen" ~"Thüringen"
  ))%>% tidyr::separate(...3, c("a","b"), sep = ",")%>% #reicht nicht ganz, müsste auch nach : separieren für Sachsen-Anhalt Kreise
  dplyr::rename(ort = a)

  #schlüsselnummer nur mit Leerzeichen getrennt
  data_n1$...4 <- stringr::str_extract(data_n1$ort, "[[:digit:]]+")
  data_n1$ort <- gsub("[[:digit:]]", "", data_n1$ort)
  data_n1$ort <- stringr::str_trim(data_n1$ort)

  # zwischen gleichnamigen Stadt- und Landkreisen unterscheiden
  ## Hilfsvarialbe, die Stadt/Landkreise, die es doppelt gibt, in Hilfs-String schreibt
  help <- data.frame(table(data_n1$ort))
  help <- help %>% dplyr::filter(Freq != 1)

  # für die ausgewählten Fälle (-->%in% help) falls "Stadt" in näherer Bezeichnung in Spalte b vorkommt, Stadt vorschreiben, sonst Landkreis
  data_n1$ort <- ifelse(data_n1$ort %in% help$Var1 & grepl("tadt", data_n1$b) , stringr::str_c("Stadt ", data_n1$ort), data_n1$ort)
  data_n1$ort <- ifelse(data_n1$ort %in% help$Var1 & !grepl("tadt", data_n1$b), stringr::str_c("Landkreis ", data_n1$ort),data_n1$ort)

  # Spezifalfall Oldenburg mit Beschreibung Oldenburg in Klammern, daher nicht erkannt als identisch in Ansatz davor
  data_n1$ort <- ifelse(grepl("Olde", data_n1$ort), "Stadt Oldenbrug", data_n1$ort)
  data_n1$ort <- ifelse(grepl("Olde", data_n1$ort) & is.na(data_n1$b), "Landkreis Oldenbrug", data_n1$ort)

  data_n <- data_n1 %>%
    dplyr::rename(
      schluesselnummer = ...4,
      zusatz = b
    )

  data_n <- data_n[,-c(6,7)]

# Header ergänzen daten von 2021
header <- c("region", "ort", "zusatz", "schluesselnummer", "fachbereich",
            #
            "Beschäftigte",
            "weibliche Beschäftigte",
            "Beschäftigte u25",
            "Beschäftigte ü55",
            #
            "Beschäftigte (nur SVB)",
            "weibliche Beschäftigte (nur SVB)",
            "Beschäftigte u25 (nur SVB)",
            "Beschäftigte ü55 (nur SVB)",
            "Auszubildende",
            "weibliche Auszubildende",
            #
            "Beschäftigte (nur GFB)",
            "weibliche Beschäftigte (nur GFB)",
            "Beschäftigte u25 (nur GFB)",
            "Beschäftigte ü55 (nur GFB)",
            #
            "ausländische Beschäftigte",
            "ausländische weibliche Beschäftigte",
            "ausländische Beschäftigte u25",
            "ausländische Beschäftigte ü55",
            #
            "ausländische Beschäftigte (nur SVB)",
            "ausländische weibliche Beschäftigte (nur SVB)",
            "ausländische Beschäftigte u25 (nur SVB)",
            "ausländische Beschäftigte ü55 (nur SVB)",
            "ausländische Auszubildende",
            "ausländische weibliche Auszubildende",
            #
            "ausländische Beschäftigte (nur GFB)",
            "ausländische weibliche Beschäftigte (nur GFB)",
            "ausländische Beschäftigte u25 (nur GFB)",
            "ausländische Beschäftigte ü55 (nur GFB)",

            "bundesland"
)
colnames(data) <- header

# Header ergänzen daten von 2022
header <- c("region", "fachbereich", "ort", "zusatz", "schluesselnummer",
            #
            "Beschäftigte",
            "weibliche Beschäftigte",
            "Beschäftigte u25",
            "Beschäftigte ü55",
            #
            "Beschäftigte (nur SVB)",
            "weibliche Beschäftigte (nur SVB)",
            "Beschäftigte u25 (nur SVB)",
            "Beschäftigte ü55 (nur SVB)",
            "Auszubildende",
            "weibliche Auszubildende",
            #
            "Beschäftigte (nur GFB)",
            "weibliche Beschäftigte (nur GFB)",
            "Beschäftigte u25 (nur GFB)",
            "Beschäftigte ü55 (nur GFB)",
            #
            "ausländische Beschäftigte",
            "ausländische weibliche Beschäftigte",
            "ausländische Beschäftigte u25",
            "ausländische Beschäftigte ü55",
            #
            "ausländische Beschäftigte (nur SVB)",
            "ausländische weibliche Beschäftigte (nur SVB)",
            "ausländische Beschäftigte u25 (nur SVB)",
            "ausländische Beschäftigte ü55 (nur SVB)",
            "ausländische Auszubildende",
            "ausländische weibliche Auszubildende",
            #
            "ausländische Beschäftigte (nur GFB)",
            "ausländische weibliche Beschäftigte (nur GFB)",
            "ausländische Beschäftigte u25 (nur GFB)",
            "ausländische Beschäftigte ü55 (nur GFB)",

            "bundesland"
)
colnames(data_n) <- header

# bundesland füllen
data_n$bundesland <- stats::ave(data_n$bundesland, cumsum(!is.na(data_n$bundesland)), FUN=function(x) x[1])

## Aufbereiten in gewünschte DF-Struktur ---------------------------------

detailliert_aufbereiten <- function(data, jahr){

# Anpassungen (kab)

#NA definieren anstelle */0
# data <- data %>%
#   dplyr::mutate_all(~dplyr::replace(., . %in% c(0, "*"), NA))

# Orte/Schlüsselnummern trennen
data <- data %>%
  dplyr::mutate(dplyr::across(c(6:33), as.numeric))

data[data == 0] <- NA

data$ort <- ifelse(is.na(data$ort), data$region, data$ort)
data$zusatz <- ifelse(is.na(data$zusatz), data$region, data$zusatz)
data$schluesselnummer <- ifelse(is.na(data$schluesselnummer), data$region, data$schluesselnummer)

data$ort <- zoo::na.locf(data$ort )
data$zusatz <- zoo::na.locf(data$zusatz)
data$schluesselnummer <- zoo::na.locf(data$schluesselnummer)



data <- data %>%
  dplyr::filter(!is.na(fachbereich))%>%
  dplyr::select(-region)%>%
  dplyr::rename(region = ort)

data$zusatz <- ifelse(data$zusatz == data$region, NA, data$zusatz )
data$schluesselnummer <- ifelse(data$schluesselnummer == data$region, NA, data$schluesselnummer )


#Fachbereich und Arbeitslevel trennen
data$anforderung <- ifelse(data$fachbereich %in% c("Helfer", "Fachkraft", "Spezialist",
                                                   "Experte", "keine Angabe"), data$fachbereich, "Gesamt")
data$fachbereich <- ifelse(data$fachbereich %in% c("Helfer", "Fachkraft", "Spezialist",
                                                   "Experte", "keine Angabe"), NA, data$fachbereich)

data$anforderung[data$anforderung=="keine Angabe"]<-"keine Zuordnung möglich"
data$fachbereich[data$fachbereich=="Insgesamt"]<-"Alle"
data$fachbereich[data$fachbereich=="MINT-Berufe"]<-"MINT"
data$fachbereich[data$fachbereich=="Technik"]<-"Technik (gesamt)"

# Lücken füllen, die durch Zellverbünde entstanden sind
# data$region <- stats::ave(data$region, cumsum(!is.na(data$region)), FUN=function(x) x[1])
data$fachbereich <- stats::ave(data$fachbereich, cumsum(!is.na(data$fachbereich)), FUN=function(x) x[1])

# ins long-Format bringen
data <- data %>%
  tidyr::pivot_longer(cols = "Beschäftigte":"ausländische Beschäftigte ü55 (nur GFB)")


## nötige Variablen auswählen/ergänzen -----------------------------------------------

# Entferen von Variablen die (aktuell) nicht analysiert werden
data <- subset(data, !(name %in% c("Beschäftigte", "weibliche Beschäftigte", "Beschäftigte u25", "Beschäftigte ü55",
                                   "Beschäftigte u25 (nur GFB)", "Beschäftigte ü55 (nur GFB)",
                                   "ausländische Beschäftigte", "ausländische weibliche Beschäftigte", "ausländische Beschäftigte u25",
                                   "ausländische Beschäftigte ü55", "ausländische Beschäftigte u25 (nur GFB)",
                                   "ausländische Beschäftigte ü55 (nur GFB)")))
data <- data %>%
  dplyr::mutate(
    # quelle = "Bundesagentur für Arbeit, 2022: Auf Anfrage (Auftragsnummer 335970)",
    # hinweise = "eigene Berechnungen durch MINTvernetzt",
    bereich = "Arbeitsmarkt",
    jahr = jahr,
    geschlecht = dplyr::case_when(
      stringr::str_detect(data$name, "weiblich")~"Frauen",
      TRUE ~ "Gesamt"
    ),
    kategorie = dplyr::case_when(
      stringr::str_detect(data$name, "Auszubildende")~"Auszubildende",
      TRUE ~ "Beschäftigte"
    ),
    name = dplyr::case_when(
      name == "Beschäftigte (nur SVB)"~"Beschäftigte",
      name == "weibliche Beschäftigte (nur SVB)"~ "Beschäftigte",
      name == "Beschäftigte u25 (nur SVB)"~"Beschäftigte u25",
      name == "Beschäftigte ü55 (nur SVB)"~"Beschäftigte ü55",
      name == "weibliche Auszubildende"~"Auszubildende",
      name == "Beschäftigte (nur GFB)"~"in Minijobs",
      name == "weibliche Beschäftigte (nur GFB)"~"in Minijobs",
      name == "ausländische Beschäftigte (nur SVB)"~"ausländische Beschäftigte",
      name == "ausländische weibliche Beschäftigte (nur SVB)"~ "ausländische Beschäftigte",
      name == "ausländische Beschäftigte u25 (nur SVB)"~"ausländische Beschäftigte u25",
      name == "ausländische Beschäftigte ü55 (nur SVB)"~"ausländische Beschäftigte ü55",
      name == "ausländische weibliche Auszubildende"~"ausländische Auszubildende",
      name == "ausländische Beschäftigte (nur GFB)"~"ausländisch in Minijobs",
      name == "ausländische weibliche Beschäftigte (nur GFB)"~"ausländisch in Minijobs",
      TRUE ~ name
    )) %>%
  dplyr::rename(wert=value,
                indikator=name)

data <- data %>%
  dplyr::rename(
    landkreis = region,
    landkreis_zusatz = zusatz,
    landkreis_nummer = schluesselnummer
  )


# Spalten in logische Reihenfolge bringen
data <- data[,c("bereich", "kategorie", "indikator", "fachbereich", "geschlecht", "bundesland", "landkreis", "landkreis_zusatz", "landkreis_nummer", "jahr", "anforderung", "wert"
                #, "hinweise", "quelle"
)]

######## Weitere Anpassungen/Berechnungen von Andi

data <- data %>%
  dplyr::mutate(wert = ifelse(is.na(wert), 0, wert))

# Calculate Beschäftigte 25-55
data_alter <- data %>% dplyr::filter(indikator %in% c("Beschäftigte", "Beschäftigte u25", "Beschäftigte ü55"),
                                     geschlecht == "Gesamt")

data_alter <- data_alter %>% dplyr::group_by(bereich, kategorie, fachbereich, geschlecht, bundesland, landkreis,
                                             landkreis_zusatz, landkreis_nummer, jahr, anforderung) %>%
  dplyr::summarise(wert = wert - dplyr::lead(wert, 1) - dplyr::lead(wert, 2)) %>%
  dplyr::mutate(indikator = "Beschäftigte 25-55") %>%
  dplyr::filter(!is.na(wert)) %>%
  dplyr::bind_rows(., data_alter)

# Calculate ausländische Beschäftigte 25-55
data_ausl_alter <- data %>% dplyr::filter(indikator %in% c("ausländische Beschäftigte", "ausländische Beschäftigte u25", "ausländische Beschäftigte ü55"),
                                          geschlecht == "Gesamt")

data_ausl_alter <- data_ausl_alter %>% dplyr::group_by(bereich, kategorie, fachbereich, geschlecht, bundesland, landkreis,
                                                       landkreis_zusatz, landkreis_nummer, jahr, anforderung) %>%
  dplyr::summarise(wert = wert - dplyr::lead(wert, 1) - dplyr::lead(wert, 2)) %>%
  dplyr::mutate(indikator = "ausländische Beschäftigte 25-55") %>%
  dplyr::filter(!is.na(wert)) %>%
  dplyr::bind_rows(., data_ausl_alter)

# Calculate males
data_geschlecht <- data %>% dplyr::filter(!indikator %in% c("Beschäftigte u25", "Beschäftigte ü55",
                                                            "ausländische Beschäftigte u25", "ausländische Beschäftigte ü55")) %>%
  dplyr::group_by(bereich, kategorie, indikator, fachbereich, bundesland, landkreis,
                  landkreis_zusatz, landkreis_nummer, jahr, anforderung) %>%
  dplyr::summarise(wert = wert - dplyr::lead(wert, 1)) %>%
  dplyr::mutate(geschlecht = "Männer") %>%
  dplyr::filter(!is.na(wert)) %>%
  dplyr::bind_rows(., data)

data_final <- dplyr::bind_rows(data_geschlecht, data_alter, data_ausl_alter) %>%
  dplyr::distinct()
}
data <- detailliert_aufbereiten(data = data, jahr = 2021)
data_n <- detailliert_aufbereiten(data = data_n, jahr = 2022)

## Azubi-Datensatz ---------------------------------------------------------


### Rohdaten einlesen ---------------------------------------------------

#Einlesen Auzubi extra Datensatz
# data_a <- readxl::read_excel(system.file(package = "datalab", "data-raw/BA007_221205_AusbV_MINT.xlsx"),
#                            sheet = "Auswertung2", col_names = F, range = "A12:L4201")

# läuft seit neuem Laptop mit Code drüber nicht mehr druch, daher umgeschrieben (kbr)
# data_a <- readxl::read_excel("BA007_221205_AusbV_MINT.xlsx",
#                              sheet = "Auswertung2", col_names = F, range = "A12:L4201")
data_a <- readxl::read_excel(paste0(pfad, "BA007_221205_AusbV_MINT.xlsx"),
                             sheet = "Auswertung2", col_names = F, range = "A12:L4201")

# Spalten zusammenfassen/löschen
data_a$...1 <- dplyr::coalesce(data_a$...4, data_a$...3, data_a$...2, data_a$...1) # Regionen in eine Spalte
data_a$...5 <- dplyr::coalesce(data_a$...8, data_a$...7, data_a$...6, data_a$...5) # Fachbereich in eine Spalte

# Bundesland-Spalte erstellen
data_a1 <- data_a %>%
  dplyr::mutate(bundesland=dplyr::case_when(
    ...1 == "Deutschland" ~ "Deutschland",
    ...1 == "Westdeutschland (ohne Berlin)" ~"Westdeutschland (o. Berlin)",
    ...1 == "Ostdeutschland (einschl. Berlin)" ~ "Ostdeutschland (einschl. Berlin)",
    ...1 == "Baden-Württemberg" ~"Baden-Württemberg",
    ...1 == "Bayern" ~"Bayern",
    ...1 == "Berlin" ~"Berlin",
    ...1 == "Brandenburg" ~ "Brandenburg",
    ...1 == "Bremen" ~ "Bremen",
    ...1 == "Hamburg" ~"Hamburg",
    ...1 == "Hessen" ~"Hessen",
    ...1 == "Mecklenburg-Vorpommern" ~"Mecklenburg-Vorpommern",
    ...1 == "Niedersachsen" ~"Niedersachsen",
    ...1 == "Nordrhein-Westfalen" ~"Nordrhein-Westfalen",
    ...1 == "Rheinland-Pfalz" ~"Rheinland-Pfalz",
    ...1 == "Saarland" ~"Saarland",
    ...1 == "Sachsen-Anhalt" ~"Sachsen-Anhalt",
    ...1 == "Sachsen" ~"Sachsen",
    ...1 == "Schleswig-Holstein" ~"Schleswig-Holstein",
    ...1 == "Thüringen" ~"Thüringen"
  ))%>% tidyr::separate(...4, c("a","b","c"), sep = ",")%>%
  dplyr::rename(ort = a)

# für LKs von Sachsen-Anhalt Trennung mit :
data_a1 <- data_a1 %>%
  tidyr::separate(ort, c("ort", "d"), sep = ":")
data_a1$b <- ifelse(!is.na(data_a1$d), data_a1$d, data_a1$b)
data_a1 <- data_a1 %>% dplyr::select(-d)

# Trennen von Infromation und Schlüsselnummer für Städte in Sachsen-Anhalt & Thüringen an :
data_a1 <- data_a1 %>%
  tidyr::separate(b, c("b", "d"), sep = ":")
data_a1$c <- ifelse(!is.na(data_a1$d), data_a1$d, data_a1$c)
data_a1 <- data_a1 %>% dplyr::select(-d)


data_a1$bundesland <- zoo::na.locf(data_a1$bundesland)

data_a1$c <- ifelse(!grepl("[^A-Za-z]", data_a1$c), data_a1$b, data_a1$c)
data_a1$b <- ifelse(data_a1$b == data_a1$c, NA, data_a1$b)


# zwischen gleichnamigen Stadt- und Landkreisen unterscheiden
## Hilfsvarialbe, die Stadt/Landkreise, die es doppelt gibt, in Hilfs-String schreibt
help <- data.frame(table(data_a1$ort))
help <- help %>% dplyr::filter(Freq != 1)

# für die ausgewählten Fälle (-->%in% help) falls "Stadt" in näherer Bezeichnung in Spalte b vorkommt, Stadt vorschreiben, sonst Landkreis
data_a1$ort <- ifelse(data_a1$ort %in% help$Var1 & grepl("tadt", data_a1$b) , stringr::str_c("Stadt ", data_a1$ort), data_a1$ort)
data_a1$ort <- ifelse(data_a1$ort %in% help$Var1 & !grepl("tadt", data_a1$b), stringr::str_c("Landkreis ", data_a1$ort),data_a1$ort)

# Spezialfall Augsbrug mit zwei verschiedene Landkreisangaben nähere Erklärung hinzufügen und Schlüsselnummer korrekt in c schreiben:
data_a1$ort <- ifelse(data_a1$c == "von 01.01.1973", stringr::str_c(data_a1$ort, " ", data_a1$c), data_a1$ort)
data_a1$c <- ifelse(data_a1$c == "von 01.01.1973", data_a1$b, data_a1$c)
data_a1$ort <- ifelse(grepl("von 01.01.1973",data_a1$ort), "Dillingen a. d. Donau", data_a1$ort)
data_a1$c <- ifelse(grepl("Dillingen",data_a1$ort), "09773", data_a1$c)
data_a1$b <- ifelse(grepl("Dillingen",data_a1$ort), NA, data_a1$b)

# Spezifalfall Oldenburg mit Beschreibung Oldenburg in Klammern, daher nicht erkannt als identisch in Ansatz davor
data_a1$ort <- ifelse(data_a1$ort == "Oldenburg (Oldenburg)", "Stadt Oldenburg", data_a1$ort)
data_a1$ort <- ifelse(data_a1$ort == "Oldenburg", "Landkreis Oldenburg", data_a1$ort)

# Spezialfall Eisenach - Leerzeichen vor Schlüsselnummer
data_a1$c <- ifelse(data_a1$ort == "Eisenach", 16056, data_a1$c)

data_a1 <- data_a1 %>%
  dplyr::rename(
    schluesselnummer = c,
    zusatz = b
  )

data_a1 <- data_a1[,-c(2,3,8:11)] # nun überflüssige Spalten löschen

# Header ergänzen
header_a <- c("region", "ort", "zusatz", "schluesselnummer", "fachbereich", "gesamt", "männer", "frauen", "bundesland")
colnames(data_a1) <- header_a


### Aufbereiten in gewünschte DF-Struktur ---------------------------------

#NA definieren anstelle */0
# data_a <- data_a %>%
#   dplyr::mutate_all(~replace(., . %in% c(0, "*"), NA))

data_a <- data_a1 %>%
  dplyr::mutate(dplyr::across(c(6:8), as.numeric))

data_a[data_a == 0] <- NA

data_a$ort <- ifelse(is.na(data_a$ort), data_a$region, data_a$ort)
data_a$zusatz <- ifelse(is.na(data_a$zusatz), data_a$region, data_a$zusatz)
data_a$schluesselnummer <- ifelse(is.na(data_a$schluesselnummer), data_a$region, data_a$schluesselnummer)

data_a$ort <- zoo::na.locf(data_a$ort )
data_a$zusatz <- zoo::na.locf(data_a$zusatz)
data_a$schluesselnummer <- zoo::na.locf(data_a$schluesselnummer)



data_a <- data_a %>%
  dplyr::filter(!is.na(fachbereich))%>%
  dplyr::select(-region)%>%
  dplyr::rename(region = ort)

data_a$zusatz <- ifelse(data_a$zusatz == data_a$region, NA, data_a$zusatz )
data_a$schluesselnummer <- ifelse(data_a$schluesselnummer == data_a$region, NA, data_a$schluesselnummer )


# Lücken füllen, die durch Zellverbünde entstanden sind
#data_a$region <- stats::ave(data_a$region, cumsum(!is.na(data_a$region)), FUN=function(x) x[1])

# Männer entfernen (haben gesamt und frauen)
# kann man so nicht machen, würde Daten verlieren, da aus Datenschutz manchmal nur Zahlen der Männer und nicht Gesamt/Frauen angegeben sind
# data_a <- data_a %>%
#   dplyr::select(-männer)

# ins long-Format bringen
data_a <- data_a %>%
  tidyr::pivot_longer(cols = "gesamt":"frauen")


### Variablen ergänzen/benennen -----------------------------------------------

data_a <- data_a %>%
  dplyr::mutate(
    fachbereich = dplyr::case_when(
      fachbereich=="Insgesamt"~"Alle",
      fachbereich=="MINT-Berufe"~"MINT",
      fachbereich=="Technik"~ "Technik (gesamt)",
      TRUE ~ fachbereich
    ),
    kategorie = "Auszubildende",
    indikator = "Auszubildende (1. Jahr)",
    # quelle = "Bundesagentur für Arbeit, 2022: Auf Anfrage (Auftragsnummer 335970)",
    # hinweise = "eigene Berechnungen durch MINTvernetzt",
    bereich = "Arbeitsmarkt",
    jahr = 2021,
    anforderung = "Gesamt") %>%
  dplyr::rename(geschlecht = name,
                wert = value,
                landkreis = region,
                landkreis_zusatz = zusatz,
                landkreis_nummer = schluesselnummer
  )

data_a$geschlecht[data_a$geschlecht == "frauen"]<-"Frauen"
data_a$geschlecht[data_a$geschlecht == "gesamt"]<-"Gesamt"
data_a$geschlecht[data_a$geschlecht == "männer"]<-"Männer"

# Spalten in logische Reihenfolge bringen
data_a <- data_a[,c("bereich","kategorie", "indikator", "fachbereich", "geschlecht", "bundesland", "landkreis", "landkreis_zusatz", "landkreis_nummer", "jahr", "anforderung", "wert"
                    #, "hinweise", "quelle"
)]

# nach Andis anpassungen nochmal sortieren und 2021 und 2022 zusammenpacken
## Hambrug und Berlin mit Lk Nummer ergänzen
data_hh <- data_n[data_n$bundesland == "Hamburg",]
data_hh$landkreis_nummer <- "02000"
data_hh$landkreis_zusatz <-"Freie und Hansestadt"

data_b <- data_n[data_n$bundesland == "Berlin",]
data_b$landkreis_nummer <- "11000"
data_b$landkreis_zusatz <-"Stadt"

data_n <- rbind(data_n, data_hh, data_b)

data_final <- rbind(data, data_n)

data_final <- data_final[,c("bereich","kategorie", "indikator", "fachbereich", "geschlecht", "bundesland", "landkreis", "landkreis_zusatz", "landkreis_nummer", "jahr", "anforderung", "wert"
                    #, "hinweise", "quelle"
)]

## arbeitsmarkt detailliert zusammenführen ---------------------------------


arbeitsmarkt_detail_final <- rbind(data_final, data_a)

arbeitsmarkt_detail_final$landkreis <- ifelse(arbeitsmarkt_detail_final$bundesland==arbeitsmarkt_detail_final$landkreis & is.na(arbeitsmarkt_detail_final$landkreis_nummer
), "alle Landkreise", arbeitsmarkt_detail_final$landkreis)
# zusätzlich für 2022 weil bula und lk unterschiedlich benannt bei West und Ost
arbeitsmarkt_detail_final$landkreis <- ifelse(arbeitsmarkt_detail_final$landkreis == "Westdeutschland" | arbeitsmarkt_detail_final$landkreis == "Ostdeutschland",
                                              "alle Landkreise", arbeitsmarkt_detail_final$landkreis)

#Wert als numerisch definieren und etwaige Gruppierungen entfernen
arbeitsmarkt_detail_final <- arbeitsmarkt_detail_final %>% dplyr::ungroup()
arbeitsmarkt_detail_final$wert <- as.numeric(arbeitsmarkt_detail_final$wert)


#### Datensatz speichern

arbeitsmarkt_detail <- arbeitsmarkt_detail_final
usethis::use_data(arbeitsmarkt_detail, overwrite = T)


# Erstellt "data_naa" -----------------------------------------------------

## Datensatz für Top Berufe

## Rohdaten 2022 einlesen -------------------------------------------------------

# nur 2022 einlesen, bei gleichzeitigem Einlesen von 2017 und 2022 führen 699 abweichende Spalten zu Fehlern im Code (insgesamt > frauen)

# data_naa_a <- readxl::read_excel(system.file(package="datalab",
#                                             "data-raw/BA008_Ausbildungsmarkt-MINT-Frauenanteil-2022.xlsx"),
#                                 sheet  = "Verträge_Daten", range = "A4:D238") #nur Spalten von Anfang
# data_naa <- readxl::read_excel(system.file(package="datalab",
#                                            "data-raw/BA008_Ausbildungsmarkt-MINT-Frauenanteil-2022.xlsx"),
#                                sheet  = "Verträge_Daten", range = "TA4:AMS238") #alles aus 2022, auch Regionen

# läuft mit neuem Laptop oben nicht mehr durch, deshalb umgeschrieben (kbr)
data_naa_a <- readxl::read_excel(paste0(pfad, "BA008_Ausbildungsmarkt-MINT-Frauenanteil-2022.xlsx"),
                                 sheet  = "Verträge_Daten", range = "A4:D238") #nur Spalten von Anfang

data_naa <- readxl::read_excel(paste0(pfad, "BA008_Ausbildungsmarkt-MINT-Frauenanteil-2022.xlsx"),
                               sheet  = "Verträge_Daten", range = "TA4:AMS238") #alles aus 2022, auch Regionen

data_naa <- cbind(data_naa_a, data_naa)


## Aufbereitung Datensatz --------------------------------------------------

# behalte Landkreise
# remove all districts
# data_naa <- data_naa %>% dplyr::select(-contains("X"))

# remove
data_naa <- data_naa %>% subset(select = -c(Bezeichnung))

# Fachrichtungen zuweisen und Aggregate löschen
data_naa <- data_naa %>% dplyr::filter(!(code %in% c("-----", "M", "MI", "MM", "MT", "MTB", "MTG", "MTP", "MTV")))

data_naa <- data_naa %>% dplyr::mutate(code = dplyr::case_when(
  stringr::str_detect(data_naa$code, "MI") ~"Informatik",
  stringr::str_detect(data_naa$code, "MM") ~"Mathematik/Naturwissenschaft",
  stringr::str_detect(data_naa$code, "MTB") ~"Bau- und Gebäudetechnik",
  stringr::str_detect(data_naa$code, "MTG") ~"Gesundheitstechnik - Fachkräfte",
  stringr::str_detect(data_naa$code, "MTP") ~"Produktionstechnik",
  stringr::str_detect(data_naa$code, "MTV") ~"Verkehrs-, Sicherheits- und Veranstaltungstechnik",
  TRUE ~ code
))


# clean column with job titles
# rename column
data_naa <- data_naa %>% dplyr::rename(ebene = "Ebene",
                                       beruf = "Bezeichnung BIBB modifiziert",
                                       fachrichtung = "code")

# remove all column which provide information about the "Frauenanteil"
data_naa <- data_naa %>% dplyr::select(-contains("anteil"))

# reshape data_naa in long format
data_naa <- data_naa %>% tidyr::gather(region, anzahl, -ebene, -beruf, -fachrichtung)

# extract the information of gender contained in the strings of the
# column "region"
data_naa <- data_naa %>% dplyr::mutate(geschlecht_aggregat = stringr::str_extract(region, "_w_"),
                                       # replace "_w_" with "weiblich"
                                       geschlecht_aggregat = gsub("^.*\\_","weiblich", geschlecht_aggregat),
                                       # the remaining NA are replaced by the label "insgesamt"
                                       geschlecht_aggregat = tidyr::replace_na(geschlecht_aggregat, 'insgesamt')) %>%
  # extract the information of the year from the string in column "region"
  tidyr::extract(region, c("region", "jahr"), "(.*)_([^_]+)") %>%
  # clean the string so that only the names of the "region" are left
  dplyr::mutate(region = sub(".NAA.*", "", region),
                # replace dot with dash
                region = gsub('\\.', '-', region)) %>%
  # drop row if no label for the job title is given
  dplyr::filter(fachrichtung != "")

#Trennen von AA-code und region
data_naa <- data_naa %>% tidyr::separate(region, into = c("code", "region"),
                                         sep = "(?<=[0-9])(?=\\s?[A-Z])", remove = FALSE)
data_naa$region <- ifelse(grepl("[A-Za-z]", data_naa$code), data_naa$code, data_naa$region)
data_naa$code <- ifelse(grepl("[A-Za-z]", data_naa$code), NA, data_naa$code)

# create a sub-data_naa frame split by gender
data_naa_weiblich <- data_naa %>% dplyr::group_by(ebene, fachrichtung, region, jahr) %>%
  dplyr::filter(geschlecht_aggregat == "weiblich")

# same for "insgesamt"
data_naa_insgesamt <- data_naa %>% dplyr::group_by(ebene, fachrichtung, region, jahr) %>%
  dplyr::filter(geschlecht_aggregat == "insgesamt")


# now subtract the values for "weiblich" from "insgesamt" to create "männnlich"
# first create new data_frame (copy)
data_naa_maennlich <- data_naa_insgesamt
# second subtract values
data_naa_maennlich$anzahl <- data_naa_maennlich$anzahl - data_naa_weiblich$anzahl
# specify gender as "männlich"
data_naa_maennlich$geschlecht_aggregat <- "Männer"

# combine data_frames
data_naa <- rbind(data_naa_insgesamt, data_naa_weiblich, data_naa_maennlich)

# läuft nicht durch
# insert zero if NA
# data_naa <- data_naa %>%
#   dplyr::mutate(anzahl = tidyr::replace_na(anzahl, 0))

data_naa <- data_naa %>%
  dplyr::rename(geschlecht = "geschlecht_aggregat",
                wert = "anzahl",
                fachbereich = "fachrichtung")

data_naa[data_naa$geschlecht == "weiblich", "geschlecht"] <- "Frauen"
data_naa[data_naa$geschlecht == "Männer", "geschlecht"] <- "Männer"
data_naa[data_naa$geschlecht == "insgesamt", "geschlecht"] <- "Gesamt"

data_naa[data_naa$region == "Ost", "region"] <- "Ostdeutschland (mit Berlin)"
data_naa[data_naa$region == "West", "region"] <- "Westdeutschland (ohne Berlin)"

# sort data_naa
data_naa <- data_naa[,c("ebene", "fachbereich", "beruf", "code", "region", "jahr", "geschlecht", "wert")]

data_naa_22 <- data_naa

rm(data_naa_a, data_naa_insgesamt, data_naa_maennlich, data_naa_weiblich)


## Rohdaten 2017 einlesen --------------------------------------------------

# nur 2017 einlesen, bei gleichzeitigem Einlesen von 2017 und 2022 führen 699 abweichende Spalten zu Fehlern im Code (insgesamt > frauen)
#
# data_naa <- readxl::read_excel(system.file(package="datalab",
#                                             "data-raw/BA008_Ausbildungsmarkt-MINT-Frauenanteil-2022.xlsx"),
#                                 sheet  = "Verträge_Daten", range = "A4:SW238") #alle Daten aus 2017 incl Regionen

# funktioniert drüber seit neuem Laptop nicht mehr, deshalb umgeschrieben (kbr)
data_naa <- readxl::read_excel(paste0(pfad, "BA008_Ausbildungsmarkt-MINT-Frauenanteil-2022.xlsx"),
                               sheet  = "Verträge_Daten", range = "A4:SW238") #alle Daten aus 2017 incl Regionen


## Aufbereitung Datensatz --------------------------------------------------


# behalte Landkreise
# remove all districts
# data_naa <- data_naa %>% dplyr::select(-contains("X"))

# remove
data_naa <- data_naa %>% subset(select = -c(Bezeichnung))

# Fachrichtungen zuweisen und Aggregate löschen
data_naa <- data_naa %>% dplyr::filter(!(code %in% c("-----", "M", "MI", "MM", "MT", "MTB", "MTG", "MTP", "MTV")))

data_naa <- data_naa %>% dplyr::mutate(code = dplyr::case_when(
  stringr::str_detect(data_naa$code, "MI") ~"Informatik",
  stringr::str_detect(data_naa$code, "MM") ~"Mathematik/Naturwissenschaft",
  stringr::str_detect(data_naa$code, "MTB") ~"Bau- und Gebäudetechnik",
  stringr::str_detect(data_naa$code, "MTG") ~"Gesundheitstechnik - Fachkräfte",
  stringr::str_detect(data_naa$code, "MTP") ~"Produktionstechnik",
  stringr::str_detect(data_naa$code, "MTV") ~"Verkehrs-, Sicherheits- und Veranstaltungstechnik",
  TRUE ~ code
))


# clean column with job titles
# rename column
data_naa <- data_naa %>% dplyr::rename(ebene = "Ebene",
                                       beruf = "Bezeichnung BIBB modifiziert",
                                       fachrichtung = "code")

# remove all column which provide information about the "Frauenanteil"
data_naa <- data_naa %>% dplyr::select(-contains("anteil"))

# reshape data_naa in long format
data_naa <- data_naa %>% tidyr::gather(region, anzahl, -ebene, -beruf, -fachrichtung)

# extract the information of gender contained in the strings of the
# column "region"
data_naa <- data_naa %>% dplyr::mutate(geschlecht_aggregat = stringr::str_extract(region, "_w_"),
                                       # replace "_w_" with "weiblich"
                                       geschlecht_aggregat = gsub("^.*\\_","weiblich", geschlecht_aggregat),
                                       # the remaining NA are replaced by the label "insgesamt"
                                       geschlecht_aggregat = tidyr::replace_na(geschlecht_aggregat, 'insgesamt')) %>%
  # extract the information of the year from the string in column "region"
  tidyr::extract(region, c("region", "jahr"), "(.*)_([^_]+)") %>%
  # clean the string so that only the names of the "region" are left
  dplyr::mutate(region = sub(".NAA.*", "", region),
                # replace dot with dash
                region = gsub('\\.', '-', region)) %>%
  # drop row if no label for the job title is given
  dplyr::filter(fachrichtung != "")

#Trennen von AA-code und region
data_naa <- data_naa %>% tidyr::separate(region, into = c("code", "region"),
                                         sep = "(?<=[0-9])(?=\\s?[A-Z])", remove = FALSE)
data_naa$region <- ifelse(grepl("[A-Za-z]", data_naa$code), data_naa$code, data_naa$region)
data_naa$code <- ifelse(grepl("[A-Za-z]", data_naa$code), NA, data_naa$code)

# create a sub-data_naa frame split by gender
data_naa_weiblich <- data_naa %>% dplyr::group_by(ebene, fachrichtung, region, jahr) %>%
  dplyr::filter(geschlecht_aggregat == "weiblich")

# same for "insgesamt"
data_naa_insgesamt <- data_naa %>% dplyr::group_by(ebene, fachrichtung, region, jahr) %>%
  dplyr::filter(geschlecht_aggregat == "insgesamt")


# now subtract the values for "weiblich" from "insgesamt" to create "männnlich"
# first create new data_frame (copy)
data_naa_maennlich <- data_naa_insgesamt
# second subtract values
data_naa_maennlich$anzahl <- data_naa_maennlich$anzahl - data_naa_weiblich$anzahl
# specify gender as "männlich"
data_naa_maennlich$geschlecht_aggregat <- "Männer"

# combine data_frames
data_naa <- rbind(data_naa_insgesamt, data_naa_weiblich, data_naa_maennlich)

# läuft nicht durch
# insert zero if NA
# data_naa <- data_naa %>%
#   dplyr::mutate(anzahl = tidyr::replace_na(anzahl, 0))

data_naa <- data_naa %>%
  dplyr::rename(geschlecht = "geschlecht_aggregat",
                wert = "anzahl",
                fachbereich = "fachrichtung")

data_naa[data_naa$geschlecht == "weiblich", "geschlecht"] <- "Frauen"
data_naa[data_naa$geschlecht == "Männer", "geschlecht"] <- "Männer"
data_naa[data_naa$geschlecht == "insgesamt", "geschlecht"] <- "Gesamt"

data_naa[data_naa$region == "Ost", "region"] <- "Ostdeutschland (mit Berlin)"
data_naa[data_naa$region == "West", "region"] <- "Westdeutschland (ohne Berlin)"

# sort data_naa
data_naa <- data_naa[,c("ebene", "fachbereich", "beruf", "code", "region", "jahr", "geschlecht", "wert")]

rm(data_naa_insgesamt, data_naa_maennlich, data_naa_weiblich)

data_naa_17 <- data_naa


## Rohdaten 2020 einlesen --------------------------------------------------

# 2020 einlesen

# data_naa_a <- readxlsb::read_xlsb(system.file(package="datalab",
#                                             "data-raw/BA002_Ausbildungsmarkt-MINT-Frauenanteil-2020.xlsb"),
#                                 sheet  = "Vertraege_Daten", range = "A4:D218")
# data_naa <- readxlsb::read_xlsb(system.file(package="datalab",
#                                             "data-raw/BA002_Ausbildungsmarkt-MINT-Frauenanteil-2020.xlsb"),
#                                 sheet  = "Vertraege_Daten", range = "TA4:AMV218")

# funktioniert seit neuem Laptop nicht mehr so, deshalb umgeschrieben (kbr)
data_naa_a <- readxlsb::read_xlsb(paste0(pfad, "BA002_Ausbildungsmarkt-MINT-Frauenanteil-2020.xlsb"),
                                  sheet  = "Vertraege_Daten", range = "A4:D218")
data_naa <- readxlsb::read_xlsb(paste0(pfad, "BA002_Ausbildungsmarkt-MINT-Frauenanteil-2020.xlsb"),
                                sheet  = "Vertraege_Daten", range = "TA4:AMV218")

data_naa <- cbind(data_naa_a, data_naa)


## Aufbereitung Datensatz --------------------------------------------------

# behalte Landkreise
# remove all districts
# data_naa <- data_naa %>% dplyr::select(-contains("X"))

# remove
data_naa <- data_naa %>% subset(select = -c(Bezeichnung))

# Fachrichtungen zuweisen und Aggregate löschen
data_naa <- data_naa %>% dplyr::filter(!(code %in% c("-----", "M", "MI", "MM", "MT", "MTB", "MTG", "MTP", "MTV")))

data_naa <- data_naa %>% dplyr::mutate(code = dplyr::case_when(
  stringr::str_detect(data_naa$code, "MI") ~"Informatik",
  stringr::str_detect(data_naa$code, "MM") ~"Mathematik/Naturwissenschaft",
  stringr::str_detect(data_naa$code, "MTB") ~"Bau- und Gebäudetechnik",
  stringr::str_detect(data_naa$code, "MTG") ~"Gesundheitstechnik - Fachkräfte",
  stringr::str_detect(data_naa$code, "MTP") ~"Produktionstechnik",
  stringr::str_detect(data_naa$code, "MTV") ~"Verkehrs-, Sicherheits- und Veranstaltungstechnik",
  TRUE ~ code
))


# clean column with job titles
# rename column
data_naa <- data_naa %>% dplyr::rename(ebene = "Ebene",
                                       beruf = "Bezeichnung.BIBB.modifiziert",
                                       fachrichtung = "code")

# remove all column which provide information about the "Frauenanteil"
data_naa <- data_naa %>% dplyr::select(-contains("anteil"))

# reshape data_naa in long format
data_naa <- data_naa %>% tidyr::gather(region, anzahl, -ebene, -beruf, -fachrichtung)

# extract the information of gender contained in the strings of the
# column "region"
data_naa <- data_naa %>% dplyr::mutate(geschlecht_aggregat = stringr::str_extract(region, "_w_"),
                                       # replace "_w_" with "weiblich"
                                       geschlecht_aggregat = gsub("^.*\\_","weiblich", geschlecht_aggregat),
                                       # the remaining NA are replaced by the label "insgesamt"
                                       geschlecht_aggregat = tidyr::replace_na(geschlecht_aggregat, 'insgesamt')) %>%
  # extract the information of the year from the string in column "region"
  tidyr::extract(region, c("region", "jahr"), "(.*)_([^_]+)") %>%
  # clean the string so that only the names of the "region" are left
  dplyr::mutate(region = sub(".NAA.*", "", region),
                # replace dot with dash
                region = gsub('\\.', '-', region)) %>%
  # drop row if no label for the job title is given
  dplyr::filter(fachrichtung != "")

#Trennen von AA-code und region

# X vor Code löschen
data_naa <- data_naa %>%
  dplyr::mutate(region = sub("X", "", region))
# Namenselemente an Bindestrich trennen und in separate Spalten schreiben (Extra-Spalten für Regionen mit - in Namen nötig)
data_naa <- data_naa %>%
  tidyr::separate(region, into = c("code","codeb", "region", "regionb", "regionc", "regiond"),
                  sep = "-", remove = FALSE)
#Deutschland und Bundesländer von Code zu Region sortieren
data_naa$region <- ifelse(grepl("[A-Za-z]", data_naa$code), data_naa$code, data_naa$region)
data_naa$code <- ifelse(grepl("[A-Za-z]", data_naa$code), NA, data_naa$code)
data_naa$regionb <- ifelse(grepl("[A-Za-z]", data_naa$codeb), data_naa$codeb, data_naa$regionb)
data_naa$codeb <- ifelse(grepl("[A-Za-z]", data_naa$codeb), NA, data_naa$codeb)
# Code Bausteine zusammenschreiben
data_naa$code <- ifelse(is.na(data_naa$codeb), data_naa$code, paste(data_naa$code, data_naa$codeb))
# Regionen wieder zusammenschreiben mit zwischenzeitlich gelöschten -
data_naa$regionc <- ifelse(is.na(data_naa$regiond), data_naa$regionc, paste(data_naa$regionc, data_naa$regiond, sep = "-"))
data_naa$regionb <- ifelse(is.na(data_naa$regionc), data_naa$regionb, paste(data_naa$regionb, data_naa$regionc, sep = "-"))
data_naa$region <- ifelse(is.na(data_naa$regionb), data_naa$region, paste(data_naa$region, data_naa$regionb, sep = "-"))
# überflüssige temporäre Spalten löschen
data_naa <- data_naa %>% dplyr::select(-codeb, -regionb, -regionc, -regiond)


# create a sub-data_naa frame split by gender
data_naa_weiblich <- data_naa %>% dplyr::group_by(ebene, fachrichtung, region, jahr) %>%
  dplyr::filter(geschlecht_aggregat == "weiblich")

# same for "insgesamt"
data_naa_insgesamt <- data_naa %>% dplyr::group_by(ebene, fachrichtung, region, jahr) %>%
  dplyr::filter(geschlecht_aggregat == "insgesamt")


# now subtract the values for "weiblich" from "insgesamt" to create "männnlich"
# first create new data_frame (copy)
data_naa_maennlich <- data_naa_insgesamt
# second subtract values
data_naa_maennlich$anzahl <- data_naa_maennlich$anzahl - data_naa_weiblich$anzahl
# specify gender as "männlich"
data_naa_maennlich$geschlecht_aggregat <- "Männer"

# combine data_frames
data_naa <- rbind(data_naa_insgesamt, data_naa_weiblich, data_naa_maennlich)

# läuft nicht durch
# insert zero if NA
# data_naa <- data_naa %>%
#   dplyr::mutate(anzahl = tidyr::replace_na(anzahl, 0))

data_naa <- data_naa %>%
  dplyr::rename(geschlecht = "geschlecht_aggregat",
                wert = "anzahl",
                fachbereich = "fachrichtung")

data_naa[data_naa$geschlecht == "weiblich", "geschlecht"] <- "Frauen"
data_naa[data_naa$geschlecht == "Männer", "geschlecht"] <- "Männer"
data_naa[data_naa$geschlecht == "insgesamt", "geschlecht"] <- "Gesamt"

data_naa[data_naa$region == "Ost", "region"] <- "Ostdeutschland (mit Berlin)"
data_naa[data_naa$region == "West", "region"] <- "Westdeutschland (ohne Berlin)"

# sort data_naa
data_naa <- data_naa[,c("ebene", "fachbereich", "beruf", "code", "region", "jahr", "geschlecht", "wert")]

rm(data_naa_a, data_naa_insgesamt, data_naa_maennlich, data_naa_weiblich)

data_naa_20 <- data_naa


## Zusammenfassen und speichern --------------------------------------------

# Jahre kombinieren
data_naa <- rbind(data_naa_17, data_naa_20, data_naa_22)

# Ebene 2 = Berufsgruppen
# Ebene 3 = Berufe

#alt, würde Fachbereiche Filtern, aber wollen für top 10 der Berufe ja Berufs-Ebene (kbr)
# data_naa <- data_naa %>% dplyr::filter(ebene == "Ebene 1")

#alt, würde das einfach drinlassen und dann kann man selbst später Berufsebene ausgewählem, die interessiert (kbr)
#data_naa <- data_naa %>% dplyr::filter(ebene == "Ebene 3")

# für shinyapp:

usethis::use_data(data_naa, overwrite = T)


