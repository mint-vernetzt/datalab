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
                                  sheet  = "Verträge_Daten", range = "A4:D218")
data_naa <- readxlsb::read_xlsb(paste0(pfad, "BA002_Ausbildungsmarkt-MINT-Frauenanteil-2020.xlsb"),
                                sheet  = "Verträge_Daten", range = "TA4:AMV218")

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

################################################################################
#
# Data Lab
# Vorbereitung Datensatz: Arbeitsmarkt international
# Author: Katharina Brunner, August 2023
#
################################################################################


# Internationale Daten ----------------------------------------------------

## Vorbereitung - Packages und Funktionen
library(dplyr)

iscedf13_transform_lang <- function(dat) {

  # fächer benennen

  dat <- dat %>%
    dplyr::mutate(fach = dplyr::case_when(stringr::str_ends("F00", dat$fach)~ "Allgemeine Bildungsgänge und Qualifikationen",
                            stringr::str_ends("F01", dat$fach)~ "Pädagogik",
                            stringr::str_ends("F02", dat$fach) ~ "Geisteswissenschaften und Künste",
                            stringr::str_ends("F03", dat$fach) ~ "Sozialwissenschaften, Journalismus und Informationswesen",
                            stringr::str_ends("F04", dat$fach) ~ "Wirtschaft, Verwaltung und Recht",
                            stringr::str_ends("F05", dat$fach) ~ "Naturwissenschaften, Mathematik und Statistik",
                            stringr::str_ends("F06", dat$fach) ~ "Informatik & Kommunikationstechnologie",
                            stringr::str_ends("F07", dat$fach) ~ "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                            stringr::str_ends("F08", dat$fach) ~ "Landwirtschaft, Forstwirtschaft, Fischerei und Tiermedizin",
                            stringr::str_ends("F09", dat$fach) ~ "Gesundheit, Medizin und Sozialwesen",
                            stringr::str_ends("F10", dat$fach) ~ "Dienstleistungen",
                            stringr::str_ends("F050", dat$fach) ~ "Naturwissenschaften, Mathematik und Statistik nicht näher definiert",
                            stringr::str_ends("F051", dat$fach) ~ "Biologie und verwandte Wissenschaften",
                            stringr::str_ends("F052", dat$fach) ~ "Umwelt",
                            stringr::str_ends("F053", dat$fach) ~ "Exakte Naturwissenschaften",
                            stringr::str_ends("F054", dat$fach) ~ "Mathematik und Statistik",
                            stringr::str_ends("F058", dat$fach) ~ "Interdisziplinäre Programme und Qualifikationen mit dem Schwerpunkt Naturwissenschaften,
Mathematik und Statistik",
                            stringr::str_ends("F059", dat$fach) ~ "Naturwissenschaften, Mathematik und Statistik nicht andernorts klassifiziert",
                            stringr::str_ends("F070", dat$fach) ~ "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe nicht näher definiert",
                            stringr::str_ends("F071", dat$fach) ~ "Ingenieurwesen und Technische Berufe",
                            stringr::str_ends("F072", dat$fach) ~ "Verarbeitendes Gewerbe und Bergbau",
                            stringr::str_ends("F073", dat$fach) ~ "Architektur und Baugewerbe",
                            stringr::str_ends("F078", dat$fach) ~ "Interdisziplinäre Programme und Qualifikationen mit dem Schwerpunkt Ingenieurwesen,
 verarbeitendes Gewerbe und Baugewerbe",
                            stringr::str_ends("F079", dat$fach) ~ "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe nicht andernorts klassifiziert",

                            stringr::str_ends("F05T07", dat$fach) ~ "MINT",
                            stringr::str_ends("F050_59", dat$fach) ~ "Weitere Naturwissenschaften, Mathematik und Statistik",
                            stringr::str_ends("F070_79", dat$fach) ~ "Weitere Ingenieurwesen, verarbeitendes Gewerbe, Baugewerbe",

                            stringr::str_detect("TOTAL", dat$fach) ~ "Alle",
                            stringr::str_detect("_T", dat$fach) ~ "Alle",
                            stringr::str_ends("UNK", dat$fach) ~ "Unbekannt",
                            stringr::str_ends("F99", dat$fach) ~ "Unbekannt",
                            T ~ dat$fach))

  return(dat)
}

iscedf13_transform_kurz <- function(dat) {

  # fächer benennen

  dat <- dat %>%
    mutate(fach = case_when(stringr::str_ends("F00", dat$fach)~ "Allgemeine Bildungsgänge und Qualifikationen",
                            stringr::str_ends("F01", dat$fach)~ "Pädagogik",
                            stringr::str_ends("F02", dat$fach) ~ "Geisteswissenschaften und Künste",
                            stringr::str_ends("F03", dat$fach) ~ "Sozialwissenschaften, Journalismus und Informationswesen",
                            stringr::str_ends("F04", dat$fach) ~ "Wirtschaft, Verwaltung und Recht",
                            stringr::str_ends("F05", dat$fach) ~ "Naturwissenschaften, Mathematik und Statistik",
                            stringr::str_ends("F06", dat$fach) ~ "Informatik & Kommunikationstechnologie",
                            stringr::str_ends("F07", dat$fach) ~ "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                            stringr::str_ends("F08", dat$fach) ~ "Landwirtschaft, Forstwirtschaft, Fischerei und Tiermedizin",
                            stringr::str_ends("F09", dat$fach) ~ "Gesundheit, Medizin und Sozialwesen",
                            stringr::str_ends("F10", dat$fach) ~ "Dienstleistungen",

                            stringr::str_ends("F05T07", dat$fach) ~ "MINT",
                            stringr::str_detect("TOTAL", dat$fach) ~ "Alle",
                            stringr::str_detect("_T", dat$fach) ~ "Alle",
                            stringr::str_ends("UNK", dat$fach) ~ "Unbekannt",
                            stringr::str_ends("F99", dat$fach) ~ "Unbekannt",
                            T ~ dat$fach))

  return(dat)
}

## OECD 1 - Anteile Beschäftigung------------------------------------------

### Rohdaten einlesen -------------------------------------------------------

akro <- "kbr"
data <- read.csv(paste0("C:/Users/", akro,
                        "/OneDrive - Stifterverband/MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/02 Datenmaterial/01_Rohdaten/02_Alle Daten/OECD001_employment_per_field.csv"),
                 header = TRUE, sep = ",", dec = ".")


### Datensatz in passende Form bringen --------------------------------------

data <- data %>% dplyr::filter(Measure == "Value") #SE ausfiltern

data <- data %>%
  dplyr::select(COUNTRY, Country, ISC11A, Gender, Age, FIELD,
                INDICATOR.1, Reference.year, Value) %>%
  dplyr::rename(land_code = COUNTRY,
                land = Country,
                anforderung = ISC11A,
                geschlecht = Gender,
                ag = Age,
                fach = FIELD,
                variable = INDICATOR.1,
                jahr = Reference.year,
                wert = Value)

# Land zuweisen
## Aggregate extra
table(data$land_code) #E23, G20, OAVG
data_agg <- data %>%
  dplyr::filter(land_code %in% c("E23", "G20", "OAVG")) %>%
  dplyr::mutate(land = dplyr::case_when(
    land_code == "E23" ~ "OECD-Mitglieder aus der EU",
    land_code == "G20" ~ "G20",
    land_code == "OAVG" ~ "OECD"
  ))

data <- data %>%
  dplyr::filter(!(land_code %in% c("E23", "G20", "OAVG")))
data$land <- countrycode::countryname(data$land, destination = "country.name.de")

data <- rbind(data, data_agg)

# Anforderungsniveau zuweisen
data <- data %>%
  dplyr::mutate(anforderung = dplyr::case_when(
    anforderung ==  "L5" ~ "kurzes tertiäres Bildungsprogramm (ISCED 5)",
    anforderung ==  "L6" ~ "Bachelor oder vergleichbar (ISCED 6)",
    anforderung ==  "L7T8" ~ "Master, Promotion oder vergleichbar (ISCED 7, 8)",
    anforderung ==  "L5T8" ~ "Gesamt"
  ))

# ins Deutsche übersetzten - als Fkt für weitere intern. Datensätze - ausbauen
data <- data %>%
  dplyr::mutate(

    geschlecht = dplyr::case_when(
      geschlecht == "Men" ~ "Männer",
      geschlecht == "Women" ~ "Frauen",
      geschlecht == "Total" ~ "Gesamt",
      T ~ geschlecht
    ),
    variable = dplyr::case_when(
      variable == "Employment rate" ~ "Erwerbstätigenquote",
      variable == "Inactivity rate" ~ "Nichterwerbsquote",
      variable == "Unemployment rate" ~ "Arbeitslosenquote",
      variable == "Share of population by field of study" ~ "Anteil an Bevölkerung nach Studienbereich",
      T ~ variable
    )
  )

# Fachbereiche mit Kekelis Funktion zuweisen

data <- iscedf13_transform_kurz(data)
data <- subset(data, !(is.na(data$fach)))

# Altersgruppen mit Indikator kombinieren
data <- data %>%
  dplyr::rename(indikator = ag) %>%
  dplyr::filter(indikator %in% c("25-64 years", "55-64 years")) %>%
  dplyr::mutate(indikator = dplyr::case_when(
    indikator == "25-64 years" ~ "Beschäftigte",
    indikator == "55-64 years" ~ "Beschäftigte ü55",
    T ~ indikator
  ))

# missings ausfiltern
data <- na.omit(data)

# bereich ergänzen und sortieren
data$bereich <- "Arbeitsmarkt"
data$quelle <- "OECD"
data$typ <- "In Prozent"
data$population <- "OECD"


# Spalten in logische Reihenfolge bringen
data<- data[,c("bereich", "quelle", "variable", "typ", "indikator", "fach", "geschlecht", "population", "land", "jahr", "anforderung", "wert")]
colnames(data)[6] <- "fachbereich"

# umbenennen
arbeitsmarkt_fachkräfte_oecd <- data

# speichern
usethis::use_data(arbeitsmarkt_fachkräfte_oecd, overwrite = T)


## OECD 2 - Anzahl Absolvent*innen ------------------------------------------

### Rohdaten einlesen -------------------------------------------------------
akro <- "kbr"
dat <- read.csv(paste0("C:/Users/", akro,
                        "/OneDrive - Stifterverband/MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/02 Datenmaterial/01_Rohdaten/02_Alle Daten/OECD002_Anzahl_Absolv_nach_Feld_OECD.csv"),
                 header = TRUE, sep = ",", dec = ".")

### Datensatz in passende Form bringen --------------------------------------

dat <- dat %>%
  dplyr::select(COUNTRY, Country, EDUCATION_LEV, Gender, EDUCATION_FIELD, Year, Value) %>%
  dplyr::rename(land_code = COUNTRY,
                land = Country,
                anforderung = EDUCATION_LEV,
                geschlecht = Gender,
                fach = EDUCATION_FIELD,
                jahr = Year,
                wert = Value)

# Land zuweisen / übersetzen
dat$land <- countrycode::countryname(dat$land, destination = "country.name.de")

# Anforderungsniveau zuweisen
dat <- dat %>%
  dplyr::mutate(anforderung = dplyr::case_when(
    anforderung ==  "ISCED11_35" ~ "Erstausbildung (ISCED 35)",
    anforderung ==  "ISCED11_45" ~ "Ausbildung (ISCED 45)",
    anforderung ==  "ISCED11_5" ~ "kurzes tertiäres Bildungsprogramm (ISCED 5)",
    anforderung ==  "ISCED11_6" ~ "Bachelor oder vergleichbar (ISCED 6)",
    anforderung ==  "ISCED11_7" ~ "Master oder vergleichbar (ISCED 7)",
    anforderung ==  "ISCED11_8" ~ "Promotion (ISCED 8)"
  ))

# Fachbereich zuweisen - mit Kekelis Funktion
## MINT aggregieren
dat_mint <- dat %>%
  dplyr::filter(fach %in% c("F05", "F06", "F07")) %>%
  dplyr::group_by(land_code, land, anforderung, geschlecht, jahr) %>%
  dplyr::summarise(wert = sum(wert)) %>%
  dplyr::ungroup()
dat_mint$fach <- "F05T07"
## Gesamt berechnen
dat_ges <- dat %>%
  dplyr::filter(fach %in% c("F00", "F01", "F02", "F3","F04","F05","F06","F07","F08","F09","F10", "F99")) %>%
  dplyr::group_by(land_code, land, anforderung, geschlecht, jahr) %>%
  dplyr::summarise(wert = sum(wert)) %>%
  dplyr::ungroup()
dat_ges$fach <- "TOTAL"
## weitere Naturwissenschaften/Ingen-Wissenschaften berechnen
dat_nw <- dat %>%
  dplyr::filter(fach %in% c("F050", "F059")) %>%
  dplyr::group_by(land_code, land, anforderung, geschlecht, jahr) %>%
  dplyr::summarise(wert = sum(wert)) %>%
  dplyr::ungroup()
dat_nw$fach <- "F050_59"
dat_iw <- dat %>%
  dplyr::filter(fach %in% c("F070", "F079")) %>%
  dplyr::group_by(land_code, land, anforderung, geschlecht, jahr) %>%
  dplyr::summarise(wert = sum(wert)) %>%
  dplyr::ungroup()
dat_iw$fach <- "F070_79"
## einzelnen löschen
dat <- dat %>% filter(!(fach %in% c("F050", "F059", "F070", "F079")))

dat <- rbind(dat_mint, dat, dat_iw, dat_nw, dat_ges)

## Labels übertragen
dat <- iscedf13_transform_lang(dat)

# übersetzen
dat <- dat %>%
  dplyr::mutate(
    geschlecht = dplyr::case_when(
      geschlecht == "Male" ~ "Männer",
      geschlecht == "Female" ~ "Frauen",
      geschlecht == "Total" ~ "Gesamt",
      T ~ geschlecht
    ))

# missings ausfiltern
dat <- na.omit(dat)

# bereich ergänze und in Reihenfolge bringen
dat$bereich <- "Arbeitsmarkt"
dat$quelle <- "OECD"
dat$typ <- "Anzahl"
dat$population <- "OECD"

# Spalten in logische Reihenfolge bringen
dat<- dat[,c("bereich", "quelle", "typ", "fach",
               "geschlecht", "population", "land", "jahr", "anforderung", "wert")]
colnames(dat)[6] <- "fachbereich"


# umbenennen
arbeitsmarkt_absolvent_oecd <- dat

# speichern
usethis::use_data(arbeitsmarkt_absolvent_oecd, overwrite = T)

## OECD 3 - Anteil Feld /Frauen von Absolvent*innen--------------------------

### Rohdaten einlesen -------------------------------------------------------
akro <- "kbr"
dat <- read.csv(paste0("C:/Users/", akro,
                       "/OneDrive - Stifterverband/MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/02 Datenmaterial/01_Rohdaten/02_Alle Daten/OECD003_Anteil_Absol_nach_Feld_an_allen_Feldern_OECD.csv"),
                header = TRUE, sep = ",", dec = ".")

### Datensatz in passende Form bringen --------------------------------------

dat <- dat %>%
  dplyr::select(COUNTRY, Country, Indicator, EDUCATION_LEV, Gender, EDUCATION_FIELD,
                Year, Value) %>%
  dplyr::rename(land_code = COUNTRY,
                land = Country,
                anforderung = EDUCATION_LEV,
                geschlecht = Gender,
                fach = EDUCATION_FIELD,
                variable = Indicator,
                jahr = Year,
                wert = Value)

# Land zuweisen / übersetzen
dat_agg <- dat %>%
  dplyr::filter(land_code %in% c("E22", "OAVG")) %>%
  dplyr::mutate(land = dplyr::case_when(
    land_code == "E22" ~ "OECD-Mitglieder aus der EU",
    land_code == "OAVG" ~ "OECD"
  ))
dat <- dat %>% dplyr::filter(!(land_code %in% c("E22", "OAVG")))
dat$land <- countrycode::countryname(dat$land, destination = "country.name.de")

dat <- rbind(dat, dat_agg)

# Anforderungsniveau zuweisen
dat <- dat %>%
  dplyr::mutate(anforderung = dplyr::case_when(
    anforderung ==  "ISCED11_35" ~ "Erstausbildung (ISCED 35)",
    anforderung ==  "ISCED11_45" ~ "Ausbildung (ISCED 45)",
    anforderung ==  "ISCED11_5" ~ "kurzes tertiäres Bildungsprogramm (ISCED 5)",
    anforderung ==  "ISCED11_6" ~ "Bachelor oder vergleichbar (ISCED 6)",
    anforderung ==  "ISCED11_7" ~ "Master oder vergleichbar (ISCED 7)",
    anforderung ==  "ISCED11_8" ~ "Promotion (ISCED 8)",
    anforderung == "ISCED11_5T8" ~ "tertiäre Bildung (gesamt)"
  ))

# Fachbereich zuweisen - mit Kekelis Funktion
dat <- iscedf13_transform_kurz(dat)

# übersetzen
dat <- dat %>%
  dplyr::mutate(
    geschlecht = dplyr::case_when(
      geschlecht == "Male" ~ "Männer",
      geschlecht == "Female" ~ "Frauen",
      geschlecht == "Total" ~ "Gesamt",
      T ~ geschlecht
    ),
    variable = dplyr::case_when(
      variable == "Distribution of new entrants by field of education" ~
        "Anteil Ausbildungs-/Studiumsanfänger*innen nach Fach an allen Fächern",
      variable == "Share of graduates by field" ~
        "Anteil Absolvent*innen nach Fach an allen Fächern",
      variable == "Share of graduates by gender in fields of education" ~
        "Frauen-/Männeranteil Absolvent*innen nach Fachbereichen",
      variable == "Share of new entrants for each field of education by gender" ~
        "Frauen-/Männeranteil Ausbildungs-/Studiumsanfänger*innen nach Fachbereichen",
      T ~ variable
    )
  )

# missings ausfiltern
dat <- na.omit(dat)

# bereich ergänze und in Reihenfolge bringen
dat$bereich <- "Arbeitsmarkt"
dat$typ <- "In Prozent"
dat$quelle <- "OECD"
dat$population <- "OECD"
# dat$indikator <- "Alle"


# Spalten in logische Reihenfolge bringen
dat<- dat[,c("bereich", "quelle", "variable", "typ", "fach",
             "geschlecht", "population", "land", "jahr", "anforderung", "wert")]
colnames(dat)[6] <- "fachbereich"
# umbenennen
arbeitsmarkt_anfänger_absolv_oecd <- dat

# speichern
usethis::use_data(arbeitsmarkt_anfänger_absolv_oecd, overwrite = T)

## OECD 4 - Frauenanteil Absolvent*innen nach Feld -----------------------------
# ACHTUNG - SCHON ENTHALTEN IN OECD3

# ### Rohdaten einlesen -------------------------------------------------------
# akro <- "kbr"
# dat <- read.csv(paste0("C:/Users/", akro,
#                        "/OneDrive - Stifterverband/MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/02 Datenmaterial/01_Rohdaten/02_Alle Daten/OECD004_Frauenatneil_nach_Beruf_OECD.csv"),
#                 header = TRUE, sep = ",", dec = ".")
#
# ### Datensatz in passende Form bringen --------------------------------------
#
# dat <- dat %>%
#   dplyr::select(COUNTRY, Country, EDUCATION_LEV, Gender, EDUCATION_FIELD,
#                 Year, Value) %>%
#   dplyr::rename(land_code = COUNTRY,
#                 land = Country,
#                 anforderung = EDUCATION_LEV,
#                 geschlecht = Gender,
#                 fach = EDUCATION_FIELD,
#                 jahr = Year,
#                 wert = Value)
#
# # Land zuweisen / übersetzen
# dat_agg <- dat %>%
#   dplyr::filter(land_code %in% c("E22", "OAVG")) %>%
#   dplyr::mutate(land = dplyr::case_when(
#     land_code == "E22" ~ "OECD-Mitglieder aus der EU",
#     land_code == "OAVG" ~ "OECD"
#   ))
# dat <- dat %>% dplyr::filter(!(land_code %in% c("E22", "OAVG")))
# dat$land <- countrycode::countryname(dat$land, destination = "country.name.de")
#
# dat <- rbind(dat, dat_agg)
#
# # Anforderungsniveau zuweisen
# dat <- dat %>%
#   dplyr::mutate(anforderung = dplyr::case_when(
#     anforderung ==  "ISCED11_35" ~ "Erstausbildung (ISCED 35)",
#     anforderung ==  "ISCED11_45" ~ "Ausbildung (ISCED 45)",
#     anforderung ==  "ISCED11_5" ~ "kurzes tertiäres Bildungsprogramm (ISCED 5)",
#     anforderung ==  "ISCED11_6" ~ "Bachelor oder vergleichbar (ISCED 6)",
#     anforderung ==  "ISCED11_7" ~ "Master oder vergleichbar (ISCED 7)",
#     anforderung ==  "ISCED11_8" ~ "Promotion (ISCED 8)",
#     anforderung == "ISCED11_5T8" ~ "tertiäre Bildung (gesamt)"
#   ))
#
# # Fachbereich zuweisen - mit Kekelis Funktion
# dat <- iscedf13_transform_kurz(dat)
#
# # übersetzen
# dat <- dat %>%
#   dplyr::mutate(
#     geschlecht = dplyr::case_when(
#       geschlecht == "Male" ~ "Männer",
#       geschlecht == "Female" ~ "Frauen",
#       geschlecht == "Total" ~ "Gesamt",
#       T ~ geschlecht
#     )
#   )
#
# # missings ausfiltern
# dat <- na.omit(dat)
#
# # bereich ergänze und in Reihenfolge bringen
# dat$bereich <- "Arbeitsmarkt"
# dat$indikator <- "Alle"
# dat$typ <- "In Prozent"
# dat$quelle <- "OECD"
# dat$population <- "OECD"
# dat$variable <- "Frauenanteil Beschäftigte nach Fachbereich"
#
# # Spalten in logische Reihenfolge bringen
# dat<- dat[,c("bereich", "quelle", "variable", "typ", "indikator", "fach",
#              "geschlecht", "population", "land", "jahr", "anforderung", "wert")]
# colnames(dat)[6] <- "fachbereich"
# # umbenennen
# oecd4 <- dat


## OECD 5 - Anzahl Azubis / Studis nach Feld & Gender------------------------

### Rohdaten einlesen -------------------------------------------------------
akro <- "kbr"
dat <- read.csv(paste0("C:/Users/", akro,
                       "/OneDrive - Stifterverband/MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/02 Datenmaterial/01_Rohdaten/02_Alle Daten/OECD005_Anzahl_Studi_Azubi_nach_Fach_Sex.csv"),
                header = TRUE, sep = ",", dec = ".")


### Datensatz in passende Form bringen --------------------------------------

dat <- dat %>%
  dplyr::select(COUNTRY, Country, EDUCATION_LEV, Gender, EDUCATION_FIELD,
                Year, Value) %>%
  dplyr::rename(land_code = COUNTRY,
                land = Country,
                anforderung = EDUCATION_LEV,
                geschlecht = Gender,
                fach = EDUCATION_FIELD,
                jahr = Year,
                wert = Value)

# Land zuweisen / übersetzen

dat$land <- countrycode::countryname(dat$land, destination = "country.name.de")


# Anforderungsniveau zuweisen
dat <- dat %>%
  dplyr::mutate(anforderung = dplyr::case_when(
    anforderung ==  "ISCED11_35" ~ "Erstausbildung (ISCED 35)",
    anforderung ==  "ISCED11_45" ~ "Ausbildung (ISCED 45)",
    anforderung ==  "ISCED11_5" ~ "kurzes tertiäres Bildungsprogramm (ISCED 5)",
    anforderung ==  "ISCED11_54" ~ "kurzes tertiäres Bildungsprogramm (allgemeinbildend)",
    anforderung ==  "ISCED11_55" ~ "kurzes tertiäres Bildungsprogramm (berufsorientiert)",
    anforderung ==  "ISCED11_6" ~ "Bachelor oder vergleichbar (ISCED 6)",
    anforderung ==  "ISCED11_64" ~ "Bachelor oder vergleichbar (akademisch)",
    anforderung ==  "ISCED11_65" ~ "Bachelor oder vergleichbar (berufsorientiert)",
    anforderung ==  "ISCED11_7" ~ "Master oder vergleichbar (ISCED 7)",
    anforderung ==  "ISCED11_74" ~ "Master oder vergleichbar (akademisch)",
    anforderung ==  "ISCED11_75" ~ "Master oder vergleichbar (berufsorientiert)",
    anforderung ==  "ISCED11_8" ~ "Promotion (ISCED 8)",
    anforderung == "ISCED11_5T8" ~ "tertiäre Bildung (gesamt)",
    T ~ anforderung
  ))

# Fachbereich zuweisen - mit Kekelis Funktion
## weitere Naturwissenschaften/Ingen-Wissenschaften berechnen
dat_nw <- dat %>%
  dplyr::filter(fach %in% c("F050", "F059")) %>%
  dplyr::group_by(land_code, land, anforderung, geschlecht, jahr) %>%
  dplyr::summarise(wert = sum(wert)) %>%
  dplyr::ungroup()
dat_nw$fach <- "F050_59"
dat_iw <- dat %>%
  dplyr::filter(fach %in% c("F070", "F079")) %>%
  dplyr::group_by(land_code, land, anforderung, geschlecht, jahr) %>%
  dplyr::summarise(wert = sum(wert)) %>%
  dplyr::ungroup()
dat_iw$fach <- "F070_79"
## einzelnen löschen
dat <- dat %>% filter(!(fach %in% c("F050", "F059", "F070", "F079")))

dat <- rbind(dat, dat_iw, dat_nw)

dat <- iscedf13_transform_lang(dat)

# übersetzen
dat <- dat %>%
  dplyr::mutate(
    geschlecht = dplyr::case_when(
      geschlecht == "Male" ~ "Männer",
      geschlecht == "Female" ~ "Frauen",
      geschlecht == "Total" ~ "Gesamt",
      T ~ geschlecht
    )
  )

# indikator nach akademisch vs. berufsorientiert ergänzen
dat$indikator <- ifelse(grepl("berufsorientiert", dat$anforderung) |
                          grepl("usbildung", dat$anforderung), "berufsorientiert", "akademisch")

dat$indikator <- ifelse(dat$anforderung == "kurzes tertiäres Bildungsprogramm (ISCED 5)" |
                          dat$anforderung == "Bachelor oder vergleichbar (ISCED 6)" |
                          dat$anforderung == "Master oder vergleichbar (ISCED 7)" |
                          dat$anforderung == "tertiäre Bildung (gesamt)", "Alle", dat$indikator)



# missings ausfiltern
dat <- na.omit(dat)

# bereich ergänze und in Reihenfolge bringen
dat$bereich <- "Arbeitsmarkt"
#dat$variable <- "Anzahl Studierende/Auszubildende"
dat$quelle <- "OECD"
dat$population <- "OECD"
dat$typ <- "Anzahl"

# Spalten in logische Reihenfolge bringen
dat<- dat[,c("bereich", "quelle", "typ", "indikator", "fach",
             "geschlecht", "population", "land", "jahr", "anforderung", "wert")]
colnames(dat)[6] <- "fachbereich"

# aus irgendeinem Grund sind hinter den absoluten Anzhalen an Studis Kommastellen teils - auch schon in Rohdaten
# Bei Abgleich mit Tabellen der Datenbank - Zahlen Stimmen, wenn man die Kommastellen rundet (also auch aufrundet)
# mach ich hier:
dat$wert <- round(dat$wert)

# umbenennen
arbeitsmarkt_anzahl_azubis_oecd <- dat

# speichern
usethis::use_data(arbeitsmarkt_anzahl_azubis_oecd, overwrite = T)


## EUROSTAT0 - Anzahl Beschäftigte nach Branche -----------------------------
# macht keinen Sinn für uns - finden kein MINT hier
# ### Rohdaten einlesen -------------------------------------------------------
# akro <- "kbr"
# dat <- read.csv(paste0("C:/Users/", akro,
#                        "/OneDrive - Stifterverband/MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/02 Datenmaterial/01_Rohdaten/02_Alle Daten/EUROSTAT002_custom_Labor_nach_Berufsfeld.csv.gz"),
#                 header = TRUE, sep = ",", dec = ".")
#
# ### Datensatz in passende Form bringen --------------------------------------
#
# dat <- dat %>%
#   dplyr::select(nace_r2, OBS_VALUE, geo, TIME_PERIOD) %>%
#   dplyr::rename(land = geo,
#                 jahr = TIME_PERIOD,
#                 branche = nace_r2,
#                 wert = OBS_VALUE)
#
# # Land zuweisen / übersetzen
# dat$land <- countrycode::countrycode(dat$land, origin = "eurostat", destination = "country.name.de")
#
# # Branchen zuweisen
# dat <- dat %>%
#   dplyr::mutate(branche = dplyr::case_when(
#     branche ==  "B" ~ "Bergbau und Gewinnung von Steinen und Erden",
#     branche ==  "C" ~ "Verarbeitendes Gewerbe/Herstellung von Waren",
#     branche ==  "D" ~ "Energieversorgung",
#     branche ==  "E" ~ "Wasserversorgung; Abwasser- und Abfallentsorgung und Beseitigung von Umweltverschmutzungen",
#     branche ==  "F" ~ "Baugewerbe/Bau",
#     branche ==  "G" ~ "Handel; Instandhaltung und Reparatur von Kraftfahrzeugen",
#     branche ==  "H" ~ "Verkehr und Lagerei",
#     branche ==  "I" ~ "Gastgewerbe/Beherbergung und Gastronomie",
#     branche ==  "J" ~ "Information und Kommunikation",
#     branche ==  "L" ~ "Grundstücks- und Wohnungswesen",
#     branche ==  "M" ~ "Erbringung von freiberuflichen, wissenschaftlichen und technischen Dienstleistungen",
#     branche ==  "N" ~ "Erbringung von sonstigen wirtschaftlichen Dienstleistungen",
#     T ~ branche
#   ))
#
#
# # missings ausfiltern
# dat <- na.omit(dat)
#
# # bereich ergänze und in Reihenfolge bringen
# dat$bereich <- "Arbeitsmarkt"
# dat$indikator <- "Alle"
# dat$geschlecht <- "Gesamt"
# dat$anforderung <- "Gesamt"
# dat$einheit <- "Anzahl"
# dat <- dat %>% dplyr::rename(fachbereich = anforderung)
#
# # Spalten in logische Reihenfolge bringen
# dat<- dat[,c("bereich", "indikator", "fachbereich", "geschlecht", "land", "jahr", "anforderung", "einheit", "wert")]
#
# # umbenennen
# eurostat1 <- dat


## EUROSTAT1 - Anzahl Ingenieure & Wissenschaftler---------------------------

### Rohdaten einlesen -------------------------------------------------------
akro <- "kbr"
dat <- read.csv(paste0("C:/Users/", akro,
                       "/OneDrive - Stifterverband/MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/02 Datenmaterial/01_Rohdaten/02_Alle Daten/EUROSTAT002_custom_Labor_Tech_and_Scie.csv.gz"),
                header = TRUE, sep = ",", dec = ".")

### Datensatz in passende Form bringen --------------------------------------

dat <- dat %>%
  dplyr::select(category, sex, geo, TIME_PERIOD, unit, OBS_VALUE) %>%
  dplyr::rename(land = geo,
                indikator = category,
                jahr = TIME_PERIOD,
                variable = unit,
                geschlecht = sex,
                wert = OBS_VALUE)

# Land zuweisen / übersetzen
dat$land <- countrycode::countrycode(dat$land, origin = "eurostat", destination = "country.name.de")

# Indikator & Einheit zuweisen
dat <- dat %>%
  dplyr::mutate(
    indikator = dplyr::case_when(
    indikator ==  "HRST" ~ "Ausgebildete und/oder Beschäftite",
    indikator ==  "HRSTE" ~ "Ausgebildete",
    indikator ==  "HRSTO" ~ "Beschäftigte",
    indikator ==  "HRSTC" ~ "Ausgebildet und Beschäftigt",
    indikator ==  "SE" ~ "Naturwissenschaftler*innen und Ingenieur*innen",
    T ~ indikator
    ),
    variable = dplyr::case_when(
      variable == "THS_PER" ~ "Anzahl in Tsd.",
      variable == "PC_POP" ~ "Anteil an Gesamtbevölkerung",
      variable == "PC_ACT" ~ "Anteil an arbeitender Bevölkerung"
    ),
    geschlecht = dplyr::case_when(
      geschlecht == "F" ~ "Frauen",
      geschlecht == "M" ~ "Männer",
      geschlecht == "T" ~ "Gesamt"
    )
  )


# missings ausfiltern
dat <- na.omit(dat)

# bereich ergänze und in Reihenfolge bringen
dat$bereich <- "Arbeitsmarkt"
dat$anforderung <- "Gesamt"
dat$fachbereich <- "MINT"
dat$quelle <- "EUROSTAT"
dat$population <- "EU"
dat$typ <- ifelse(grepl("Anzah", dat$variable), "Anzahl", "In Prozent")

# Spalten in logische Reihenfolge bringen
dat<- dat[,c("bereich", "quelle", "variable", "typ", "indikator", "fachbereich",
             "geschlecht", "population", "land", "jahr", "anforderung", "wert")]
# umbenennen
arbeitsmarkt_beschäftigte_eu <- dat

# speichern:
usethis::use_data(arbeitsmarkt_beschäftigte_eu , overwrite = T)


## internationale Daten zusammenbringen ------------------------------------
#arbeitsmarkt_international <- rbind(eurostat1, oecd1, oecd2, oecd3, oecd5)

# in shinyapp:
#usethis::use_data(arbeitsmarkt_international, overwrite = T)




############################################################################

# Daten Fachkräftemangel --------------------------------------------------
library(dplyr)
##  Engpassanalyse -------------------------------------------------------

### BULA Vergleich ####

#### Rohdaten einlesen -------------------------------------------------------
akro <- "kbr"
pfad <- paste0("C:/Users/", akro,
               "/OneDrive - Stifterverband/MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/02 Datenmaterial/01_Rohdaten/02_Alle Daten/")

epa_einlesen <- function(name, sheet_s){
  df <- readxl::read_excel(paste0(pfad, name),
                           sheet = sheet_s)
  return(df)
}

sheets <- c("Fachkräfte", "Spezialisten", "Experten")
name <- c("BA014_EPA_2020_Länderergebnisse.xlsx",
          "BA015_2021_Länderergebnisse.xlsx",
          "BA016_2022_Länderergebnisse.xlsx")

epa20_f <- epa_einlesen(name[1], sheets[1])
epa20_s <- epa_einlesen(name[1], sheets[2])
epa20_e <- epa_einlesen(name[1], sheets[3])

epa21_f <- epa_einlesen(name[1], sheets[1])
epa21_s <- epa_einlesen(name[1], sheets[2])
epa21_e <- epa_einlesen(name[1], sheets[3])

epa22_f <- epa_einlesen(name[1], sheets[1])
epa22_s <- epa_einlesen(name[1], sheets[2])
epa22_e <- epa_einlesen(name[1], sheets[3])

#### Datensatz in passende Form aufbereiten ----------------------------------

# überflüssige Zeilen/Spalten entfernen
epa_zuschneiden <- function(df){
  header <- as.character(df[5,])
  header[1] <- "beruf"
  colnames(df) <- header
  df <- na.omit(df)

  return(df)
}

epa20_f <- epa_zuschneiden(epa20_f)
epa20_s <- epa_zuschneiden(epa20_s)
epa20_e <- epa_zuschneiden(epa20_e)

epa21_f <- epa_zuschneiden(epa21_f)
epa21_s <- epa_zuschneiden(epa21_s)
epa21_e <- epa_zuschneiden(epa21_e)

epa22_f <- epa_zuschneiden(epa22_f)
epa22_s <- epa_zuschneiden(epa22_s)
epa22_e <- epa_zuschneiden(epa22_e)


# ins Long Format bringen
epa20_f <- tidyr::pivot_longer(epa20_f, cols = "Deutschland":"Sachsen", values_to = "wert", names_to = "region")
epa20_e <- tidyr::pivot_longer(epa20_e, cols = "Deutschland":"Sachsen", values_to = "wert", names_to = "region")
epa20_s <- tidyr::pivot_longer(epa20_s, cols = "Deutschland":"Sachsen", values_to = "wert", names_to = "region")

epa21_f <- tidyr::pivot_longer(epa21_f, cols = "Deutschland":"Sachsen", values_to = "wert", names_to = "region")
epa21_e <- tidyr::pivot_longer(epa21_e, cols = "Deutschland":"Sachsen", values_to = "wert", names_to = "region")
epa21_s <- tidyr::pivot_longer(epa21_s, cols = "Deutschland":"Sachsen", values_to = "wert", names_to = "region")

epa22_f <- tidyr::pivot_longer(epa22_f, cols = "Deutschland":"Sachsen", values_to = "wert", names_to = "region")
epa22_e <- tidyr::pivot_longer(epa22_e, cols = "Deutschland":"Sachsen", values_to = "wert", names_to = "region")
epa22_s <- tidyr::pivot_longer(epa22_s, cols = "Deutschland":"Sachsen", values_to = "wert", names_to = "region")


# zusammenfügen
epa20_f$anforderung <- "Fachkräfte"
epa20_e$anforderung <- "Expert*innen"
epa20_s$anforderung <- "Spezialist*innen"
epa20 <- rbind(epa20_f, epa20_e, epa20_s)

epa21_f$anforderung <- "Fachkräfte"
epa21_e$anforderung <- "Expert*innen"
epa21_s$anforderung <- "Spezialist*innen"
epa21 <- rbind(epa21_f, epa21_e, epa21_s)

epa22_f$anforderung <- "Fachkräfte"
epa22_e$anforderung <- "Expert*innen"
epa22_s$anforderung <- "Spezialist*innen"
epa22 <- rbind(epa22_f, epa22_e, epa22_s)


# Spalten ergänzen
epa20$bereich <- "Arbeitsmarkt"
epa20$jahr <- 2020
epa21$bereich <- "Arbeitsmarkt"
epa21$jahr <- 2021
epa22$bereich <- "Arbeitsmarkt"
epa22$jahr <- 2022

# alles zusammen
epa <- rbind(epa20, epa21, epa22)

# Bezeichnungen von Berufen in Text und Code trennen
epa$beruf_schlüssel <- stringr::str_extract(epa$beruf, "[[:digit:]]+") #zahlen übertragen
epa$beruf <- gsub("[[:digit:]]", " ", epa$beruf) #zahlen entfernen
epa$beruf <- stringr::str_trim(epa$beruf) #Leerzeichen entfernen

# Spalten sortieren
epa <- epa[, c("bereich", "beruf", "beruf_schlüssel", "region", "anforderung", "jahr", "wert")]

# wert numerisch speichern
epa$wert <- as.numeric(epa$wert)

# Kategorie-Variable für Engpassrisiko erstellen
epa <- epa %>%
  dplyr::mutate(epa_kat = dplyr::case_when(
    wert >= 2.0 ~ "Engpassberuf",
    wert < 2.0 & wert >= 1.5 ~ "Anzeichen eines Engpassberufs",
    wert < 1.5 ~ "kein Engpassberuf"
  ))

# in shinyapp:
usethis::use_data(epa, overwrite = T)


### Detaillierte Daten für DE ####


#### Rohdaten einlesen -------------------------------------------------------
akro <- "kbr"
pfad <- paste0("C:/Users/", akro,
               "/OneDrive - Stifterverband/MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/02 Datenmaterial/01_Rohdaten/02_Alle Daten/")

epa_einlesen <- function(name, sheet_s){
  df <- readxl::read_excel(paste0(pfad, name),
                           sheet = sheet_s,
                           range = "A9:X707")
  return(df)
}

sheets <- c("Fachkräfte", "Spezialisten", "Experten")
name <- c("BA010_2019_Anhang.xlsx", "BA011_EPA_2020_Ergebnisse_Bund_detailliert.xlsx",
          "BA012_Ergebnisse_Engpassanalyse_2021_Deutschland_detailliert.xlsx",
          "BA013_EPA_DE_detailliert_2022.xlsx")

epa_de19_f <- epa_einlesen(name = name[1], sheets[1])
epa_de19_s <- epa_einlesen(name = name[1], sheets[2])
epa_de19_e <- epa_einlesen(name = name[1], sheets[3])
epa_de19_f$jahr <- 2019
epa_de19_e$jahr <- 2019
epa_de19_s$jahr <- 2019

epa_de20_f <- epa_einlesen(name = name[2], sheets[1])
epa_de20_s <- epa_einlesen(name = name[2], sheets[2])
epa_de20_e <- epa_einlesen(name = name[2], sheets[3])
epa_de20_f$jahr <- 2020
epa_de20_e$jahr <- 2020
epa_de20_s$jahr <- 2020

epa_de21_f <- epa_einlesen(name = name[3], sheets[1])
epa_de21_s <- epa_einlesen(name = name[3], sheets[2])
epa_de21_e <- epa_einlesen(name = name[3], sheets[3])
epa_de21_f$jahr <- 2021
epa_de21_e$jahr <- 2021
epa_de21_s$jahr <- 2021

epa_de22_f <- epa_einlesen(name = name[4], sheets[1])
epa_de22_s <- epa_einlesen(name = name[4], sheets[2])
epa_de22_e <- epa_einlesen(name = name[4], sheets[3])
epa_de22_f$jahr <- 2022
epa_de22_e$jahr <- 2022
epa_de22_s$jahr <- 2022

#### Datensatz in passende Form aufbereiten ----------------------------------

epa_aufbereiten_19 <- function(df){

  # anforderungsniveau und jahr zuweisen
  if(grepl("_f", deparse(substitute(df)))) df$anforderung <- "Fachkräfte"
  if(ncol(df) == 26){

  }else{
    if(grepl("_e", deparse(substitute(df)))) df$anforderung <- "Expert*innen"
    if(ncol(df) == 26){

    }else{
      if(grepl("_s", deparse(substitute(df)))) df$anforderung <- "Spezialist*innen"
    }
  }

  # leere spalten löschen
  if(df$anforderung[1] == "Fachkräfte") {
    df <- df %>% dplyr::select(-`...24`)
  }else{
    df <- df %>% dplyr::select(-c(`...21`, `...22`, `...23`, `...24`))
  }

  if(df$anforderung[1] == "Fachkräfte"){
    df <- df %>%
      rename(
        "beruf" = `...1`,
        "Anzahl Beschäftigte" = "sozialversicherungspflichtig Beschäftigte; ohne Arbeitnehmerüberlassung und Azubis (Juni 2019)",
        "Engpassindikator Vakanzzeit" = "Vakanzzeit",
        "Engpassindikator Arbeitsuchenden-Stellen-Relation" = "Arbeitsuchenden-Stellen-Relation",
        "Engpassindikator Berufssp. Arbeitslosenquote" = "Berufssp. Arbeitslosenquote",
        "Engpassindikator Veränderung des Anteils s.v. pfl. Beschäftigung von Ausländern" = "Veränderung des Anteils s.v. pfl. Beschäftigung von Ausländern",
        "Engpassindikator Abgangsrate aus Arbeitslosigkeit" = "Abgangsrate aus Arbeitslosigkeit",
        "Engpassindikator Entwicklung der mittleren Entgelte" = "Entwicklung der mittleren Entgelte",
        "Anzahl Indikatoren Engpassanalyse" = "Anzahl der bewerteten Indikatoren...10",
        "wert_EPA" = "Durchschnittliche Punktezahl...12",
        "Risikoindikator Veränderung des Anteils älterer Beschäftigter (60 Jahre und älter)" = "Veränderung des Anteils älterer Beschäftigter (60 Jahre und älter)",
        "Risikoindikator Anteil unb. Ausbildungsstellen an allen gem. Ausbildungsstellen" = "Anteil unb. Ausbildungsstellen an allen gem. Ausbildungsstellen",
        "Risikoindikator Absolventen-Beschäftigten-Relation" = "Absolventen-Beschäftigten-Relation",
        "Risikoindikator Substituierbarkeitspotenzial" = "Substituierbarkeitspotenzial",
        "Anzahl Indikatoren Risikoanalyse" = "Anzahl der bewerteten Indikatoren...17",
        "wert_Risiko" = "Durchschnittliche Punktezahl...19",
        "Ergänzungsindikator Berufliche Mobilität" = "Berufliche Mobilität",
        "Ergänzungsindikator Arbeitsstellenbestandsquote" = "Arbeitsstellenbestandsquote",
        "Ergänzungsindikator Teilzeitquote" = "Teilzeitquote",
        "Ergänzungsindikator Selbständigenanteil" = "Selbständigenanteil" )

  }else{
    df <- df %>%
      rename(
        "beruf" = `...1`,
        "Anzahl Beschäftigte" = "sozialversicherungspflichtig Beschäftigte; ohne Arbeitnehmerüberlassung und Azubis (Juni 2019)",
        "Engpassindikator Vakanzzeit" = "Vakanzzeit",
        "Engpassindikator Arbeitsuchenden-Stellen-Relation" = "Arbeitsuchenden-Stellen-Relation",
        "Engpassindikator Berufssp. Arbeitslosenquote" = "Berufssp. Arbeitslosenquote",
        "Engpassindikator Veränderung des Anteils s.v. pfl. Beschäftigung von Ausländern" = "Veränderung des Anteils s.v. pfl. Beschäftigung von Ausländern",
        "Engpassindikator Abgangsrate aus Arbeitslosigkeit" = "Abgangsrate aus Arbeitslosigkeit",
        "Engpassindikator Entwicklung der mittleren Entgelte" = "Entwicklung der mittleren Entgelte",
        "Anzahl Indikatoren Engpassanalyse" = "Anzahl der bewerteten Indikatoren...9",
        "wert_EPA" = "Durchschnittliche Punktezahl...11",
        "Risikoindikator Veränderung des Anteils älterer Beschäftigter (60 Jahre und älter)" = "Veränderung des Anteils älterer Beschäftigter (60 Jahre und älter)",
        "Risikoindikator Substituierbarkeitspotenzial" = "Substituierbarkeitspotenzial",
        "Anzahl Indikatoren Risikoanalyse" = "Anzahl der bewerteten Indikatoren...14",
        "wert_Risiko" = "Durchschnittliche Punktezahl...16",
        "Ergänzungsindikator Berufliche Mobilität" = "Berufliche Mobilität",
        "Ergänzungsindikator Arbeitsstellenbestandsquote" = "Arbeitsstellenbestandsquote",
        "Ergänzungsindikator Teilzeitquote" = "Teilzeitquote",
        "Ergänzungsindikator Selbständigenanteil" = "Selbständigenanteil" )
  }

  # als numerisch speichern für pivote_longer
  if(df$anforderung[1] == "Fachkräfte") {
    df[,c(2, 4:23)] <- sapply(df[,c(2, 4:23)], as.numeric)
  }else{
    df[,c(2:20)] <- sapply(df[,c(2:20)], as.numeric)
  }

  # nicht interessante Spalten noch raus
  if(df$anforderung[1] == "Fachkräfte") {
    df <- df %>% dplyr::select(-c("Punktezahl...11", "Punktezahl...18"))
  }else{
    df <- df %>% dplyr::select(-c("Punktezahl...10", "Punktezahl...15"))
  }

  # in long-format bringen
  df <- tidyr::pivot_longer(df, cols = c("Engpassindikator Vakanzzeit":"Ergänzungsindikator Selbständigenanteil"),
                            values_to = "wert", names_to = "indikator")
  # indikator ergänzen
  df <- df %>%
    dplyr::mutate(kategorie = dplyr::case_when(
      grepl("Engpass", df$indikator) ~ "Engpassanalyse",
      grepl("EPA", df$indikator) ~ "Engpassanalyse",
      grepl("Risiko", df$indikator) ~ "Risikoanalyse",
      grepl("Ergän", df$indikator) ~ "Ergänzungsindikatoren"
    )
    )

  # indikator kürzen
  df$indikator <- gsub(pattern = "Engpassindikator ", "", df$indikator)
  df$indikator <- gsub(pattern = "Risikoindikator ", "", df$indikator)
  df$indikator <- gsub(pattern = "Ergänzungsindikator ", "", df$indikator)

  # gesamtwert wieder als extra Spalte - logischer zum weiterarbeiten damit
  df_ges <- df %>%
    dplyr::filter(indikator %in% c("wert_EPA", "wert_Risiko")) %>%
    dplyr::rename(gesamtwert = wert) %>%
    dplyr::select(-indikator)

  if(df$anforderung[1] == "Fachkräfte"){
    df <- df %>%
      dplyr::left_join(df_ges, by = c("beruf", "Anzahl Beschäftigte", "Geregelte Berufsausbildung", "jahr", "anforderung",
                                      "kategorie"),
                       relationship = "many-to-many")
  }else{
    df <- df %>%
      dplyr::left_join(df_ges, by = c("beruf", "Anzahl Beschäftigte", "jahr", "anforderung",
                                      "kategorie"),
                       relationship = "many-to-many")
  }

  df <- df %>% dplyr::filter(!(indikator %in% c("wert_EPA", "wert_Risiko")))

  # Anzahl vorliegender Indikatoren als extra Spalte - logischer zum weiterarbeiten damit
  df_anz <- df %>%
    dplyr::filter(grepl("Anzahl", indikator)) %>%
    dplyr::rename(indikator_anzahl = wert) %>%
    dplyr::select(-indikator)

  if(df$anforderung[1] == "Fachkräfte"){
    df <- df %>%
      dplyr::left_join(df_anz, by = c("beruf", "Anzahl Beschäftigte", "Geregelte Berufsausbildung", "jahr", "anforderung",
                                      "kategorie", "gesamtwert"),
                       relationship = "many-to-many")
  }else{
    df <- df %>%
      dplyr::left_join(df_anz, by = c("beruf", "Anzahl Beschäftigte", "jahr", "anforderung",
                                      "kategorie", "gesamtwert"),
                       relationship = "many-to-many")
  }

  df <- df %>% dplyr::filter(!(grepl("Anzahl", indikator)))

  return(df)
}

epa_de19_f <- epa_aufbereiten_19(epa_de19_f)
epa_de19_s <- epa_aufbereiten_19(epa_de19_s)
epa_de19_e <- epa_aufbereiten_19(epa_de19_e)

epa_aufbereiten_20 <- function(df){

  # anforderungsniveau und jahr zuweisen
  if(grepl("_f", deparse(substitute(df)))) df$anforderung <- "Fachkräfte"
  if(ncol(df) == 26){

  }else{
    if(grepl("_e", deparse(substitute(df)))) df$anforderung <- "Expert*innen"
    if(ncol(df) == 26){

    }else{
      if(grepl("_s", deparse(substitute(df)))) df$anforderung <- "Spezialist*innen"
    }
  }

  # leere spalten löschen
  if(df$anforderung[1] == "Fachkräfte") {
    df <- df %>% dplyr::select(-`...24`)
  }else{
    df <- df %>% dplyr::select(-c(`...21`, `...22`, `...23`, `...24`))
  }

  if(df$anforderung[1] == "Fachkräfte"){
    df <- df %>%
      rename(
        "beruf" = `...1`,
        "Anzahl Beschäftigte" = "sozialversicherungspflichtig Beschäftigte; ohne Arbeitnehmerüberlassung und Azubis (Juni 2020)",
        "Engpassindikator Vakanzzeit" = "Vakanzzeit",
        "Engpassindikator Arbeitsuchenden-Stellen-Relation" = "Arbeitsuchenden-Stellen-Relation",
        "Engpassindikator Berufssp. Arbeitslosenquote" = "Berufssp. Arbeitslosenquote",
        "Engpassindikator Veränderung des Anteils s.v. pfl. Beschäftigung von Ausländern" = "Veränderung des Anteils s.v. pfl. Beschäftigung von Ausländern",
        "Engpassindikator Abgangsrate aus Arbeitslosigkeit" = "Abgangsrate aus Arbeitslosigkeit",
        "Engpassindikator Entwicklung der mittleren Entgelte" = "Entwicklung der mittleren Entgelte",
        "Anzahl Indikatoren Engpassanalyse" = "Anzahl der bewerteten Indikatoren...10",
        "wert_EPA" = "Durchschnittliche Punktezahl...12",
        "Risikoindikator Veränderung des Anteils älterer Beschäftigter (60 Jahre und älter)" = "Veränderung des Anteils älterer Beschäftigter (60 Jahre und älter)",
        "Risikoindikator Anteil unb. Ausbildungsstellen an allen gem. Ausbildungsstellen" = "Anteil unb. Ausbildungsstellen an allen gem. Ausbildungsstellen",
        "Risikoindikator Absolventen-Beschäftigten-Relation" = "Absolventen-Beschäftigten-Relation",
        "Risikoindikator Substituierbarkeitspotenzial" = "Substituierbarkeitspotenzial",
        "Anzahl Indikatoren Risikoanalyse" = "Anzahl der bewerteten Indikatoren...17",
        "wert_Risiko" = "Durchschnittliche Punktezahl...19",
        "Ergänzungsindikator Berufliche Mobilität" = "Berufliche Mobilität",
        "Ergänzungsindikator Arbeitsstellenbestandsquote" = "Arbeitsstellenbestandsquote",
        "Ergänzungsindikator Teilzeitquote" = "Teilzeitquote",
        "Ergänzungsindikator Selbständigenanteil" = "Selbständigenanteil" )

  }else{
    df <- df %>%
      rename(
        "beruf" = `...1`,
        "Anzahl Beschäftigte" = "sozialversicherungspflichtig Beschäftigte; ohne Arbeitnehmerüberlassung und Azubis (Juni 2020)",
        "Engpassindikator Vakanzzeit" = "Vakanzzeit",
        "Engpassindikator Arbeitsuchenden-Stellen-Relation" = "Arbeitsuchenden-Stellen-Relation",
        "Engpassindikator Berufssp. Arbeitslosenquote" = "Berufssp. Arbeitslosenquote",
        "Engpassindikator Veränderung des Anteils s.v. pfl. Beschäftigung von Ausländern" = "Veränderung des Anteils s.v. pfl. Beschäftigung von Ausländern",
        "Engpassindikator Abgangsrate aus Arbeitslosigkeit" = "Abgangsrate aus Arbeitslosigkeit",
        "Engpassindikator Entwicklung der mittleren Entgelte" = "Entwicklung der mittleren Entgelte",
        "Anzahl Indikatoren Engpassanalyse" = "Anzahl der bewerteten Indikatoren...9",
        "wert_EPA" = "Durchschnittliche Punktezahl...11",
        "Risikoindikator Veränderung des Anteils älterer Beschäftigter (60 Jahre und älter)" = "Veränderung des Anteils älterer Beschäftigter (60 Jahre und älter)",
        "Risikoindikator Substituierbarkeitspotenzial" = "Substituierbarkeitspotenzial",
        "Anzahl Indikatoren Risikoanalyse" = "Anzahl der bewerteten Indikatoren...14",
        "wert_Risiko" = "Durchschnittliche Punktezahl...16",
        "Ergänzungsindikator Berufliche Mobilität" = "Berufliche Mobilität",
        "Ergänzungsindikator Arbeitsstellenbestandsquote" = "Arbeitsstellenbestandsquote",
        "Ergänzungsindikator Teilzeitquote" = "Teilzeitquote",
        "Ergänzungsindikator Selbständigenanteil" = "Selbständigenanteil" )
  }

  # als numerisch speichern für pivote_longer
  if(df$anforderung[1] == "Fachkräfte") {
    df[,c(2, 4:23)] <- sapply(df[,c(2, 4:23)], as.numeric)
  }else{
    df[,c(2:20)] <- sapply(df[,c(2:20)], as.numeric)
  }

  # nicht interessante Spalten noch raus
  if(df$anforderung[1] == "Fachkräfte") {
    df <- df %>% dplyr::select(-c("Punktezahl...11", "Punktezahl...18"))
  }else{
    df <- df %>% dplyr::select(-c("Punktezahl...10", "Punktezahl...15"))
  }

  # in long-format bringen
  df <- tidyr::pivot_longer(df, cols = c("Engpassindikator Vakanzzeit":"Ergänzungsindikator Selbständigenanteil"),
                            values_to = "wert", names_to = "indikator")
  # indikator ergänzen
  df <- df %>%
    dplyr::mutate(kategorie = dplyr::case_when(
      grepl("Engpass", df$indikator) ~ "Engpassanalyse",
      grepl("EPA", df$indikator) ~ "Engpassanalyse",
      grepl("Risiko", df$indikator) ~ "Risikoanalyse",
      grepl("Ergän", df$indikator) ~ "Ergänzungsindikatoren"
    )
    )

  # indikator kürzen
  df$indikator <- gsub(pattern = "Engpassindikator ", "", df$indikator)
  df$indikator <- gsub(pattern = "Risikoindikator ", "", df$indikator)
  df$indikator <- gsub(pattern = "Ergänzungsindikator ", "", df$indikator)

  # gesamtwert wieder als extra Spalte - logischer zum weiterarbeiten damit
  df_ges <- df %>%
    dplyr::filter(indikator %in% c("wert_EPA", "wert_Risiko")) %>%
    dplyr::rename(gesamtwert = wert) %>%
    dplyr::select(-indikator)

  if(df$anforderung[1] == "Fachkräfte"){
    df <- df %>%
      dplyr::left_join(df_ges, by = c("beruf", "Anzahl Beschäftigte", "Geregelte Berufsausbildung", "jahr", "anforderung",
                                      "kategorie"),
                       relationship = "many-to-many")
  }else{
    df <- df %>%
      dplyr::left_join(df_ges, by = c("beruf", "Anzahl Beschäftigte", "jahr", "anforderung",
                                      "kategorie"),
                       relationship = "many-to-many")
  }

  df <- df %>% dplyr::filter(!(indikator %in% c("wert_EPA", "wert_Risiko")))

  # Anzahl vorliegender Indikatoren als extra Spalte - logischer zum weiterarbeiten damit
  df_anz <- df %>%
    dplyr::filter(grepl("Anzahl", indikator)) %>%
    dplyr::rename(indikator_anzahl = wert) %>%
    dplyr::select(-indikator)

  if(df$anforderung[1] == "Fachkräfte"){
    df <- df %>%
      dplyr::left_join(df_anz, by = c("beruf", "Anzahl Beschäftigte", "Geregelte Berufsausbildung", "jahr", "anforderung",
                                      "kategorie", "gesamtwert"),
                       relationship = "many-to-many")
  }else{
    df <- df %>%
      dplyr::left_join(df_anz, by = c("beruf", "Anzahl Beschäftigte", "jahr", "anforderung",
                                      "kategorie", "gesamtwert"),
                       relationship = "many-to-many")
  }

  df <- df %>% dplyr::filter(!(grepl("Anzahl", indikator)))

  return(df)
}

epa_de20_f <- epa_aufbereiten_20(epa_de20_f)
epa_de20_s <- epa_aufbereiten_20(epa_de20_s)
epa_de20_e <- epa_aufbereiten_20(epa_de20_e)

epa_aufbereiten_21 <- function(df){

  # anforderungsniveau und jahr zuweisen
  if(grepl("_f", deparse(substitute(df)))) df$anforderung <- "Fachkräfte"
  if(ncol(df) == 26){

  }else{
    if(grepl("_e", deparse(substitute(df)))) df$anforderung <- "Expert*innen"
    if(ncol(df) == 26){

    }else{
      if(grepl("_s", deparse(substitute(df)))) df$anforderung <- "Spezialist*innen"
    }
  }

  # leere spalten löschen
  if(df$anforderung[1] == "Fachkräfte") {
    df <- df %>% dplyr::select(-c(`...2`, `...3`))
  }else{
    df <- df %>% dplyr::select(-c(`...21`, `...22`, `...23`, `...24`))
  }

  if(df$anforderung[1] == "Fachkräfte"){
    df <- df %>%
      rename(
        "beruf" = `...1`,
        "Anzahl Beschäftigte" = "sozialversicherungspflichtig Beschäftigte; ohne Arbeitnehmerüberlassung und Azubis (Juni 2021)",
        "Engpassindikator Vakanzzeit" = "Vakanzzeit (Median)",
        "Engpassindikator Arbeitsuchenden-Stellen-Relation" = "Arbeitsuchenden-Stellen-Relation",
        "Engpassindikator Berufssp. Arbeitslosenquote" = "Berufssp. Arbeitslosenquote",
        "Engpassindikator Veränderung des Anteils s.v. pfl. Beschäftigung von Ausländern" = "Veränderung des Anteils s.v. pfl. Beschäftigung von Ausländern",
        "Engpassindikator Abgangsrate aus Arbeitslosigkeit" = "Abgangsrate aus Arbeitslosigkeit",
        "Engpassindikator Entwicklung der mittleren Entgelte" = "Entwicklung der mittleren Entgelte",
        "Anzahl Indikatoren Engpassanalyse" = "Anzahl der bewerteten Indikatoren...12",
        "wert_EPA" = "Durchschnittliche Punktezahl...14",
        "Risikoindikator Veränderung des Anteils älterer Beschäftigter (60 Jahre und älter)" = "Veränderung des Anteils älterer Beschäftigter (60 Jahre und älter)",
        "Risikoindikator Anteil unb. Ausbildungsstellen an allen gem. Ausbildungsstellen" = "Anteil unb. Ausbildungsstellen an allen gem. Ausbildungsstellen",
        "Risikoindikator Absolventen-Beschäftigten-Relation" = "Absolventen-Beschäftigten-Relation",
        "Risikoindikator Substituierbarkeitspotenzial" = "Substituierbarkeitspotenzial",
        "Anzahl Indikatoren Risikoanalyse" = "Anzahl der bewerteten Indikatoren...19",
        "wert_Risiko" = "Durchschnittliche Punktezahl...21",
        "Ergänzungsindikator Berufliche Mobilität" = "Berufliche Mobilität",
        "Ergänzungsindikator Arbeitsstellenbestandsquote" = "Arbeitsstellenbestandsquote",
        "Ergänzungsindikator Teilzeitquote" = "Teilzeitquote")

  }else{
    df <- df %>%
      rename(
        "beruf" = `...1`,
        "Anzahl Beschäftigte" = "sozialversicherungspflichtig Beschäftigte; ohne Arbeitnehmerüberlassung und Azubis (Juni 2021)",
        "Engpassindikator Vakanzzeit" = "Vakanzzeit (Median)",
        "Engpassindikator Arbeitsuchenden-Stellen-Relation" = "Arbeitsuchenden-Stellen-Relation",
        "Engpassindikator Berufssp. Arbeitslosenquote" = "Berufssp. Arbeitslosenquote",
        "Engpassindikator Veränderung des Anteils s.v. pfl. Beschäftigung von Ausländern" = "Veränderung des Anteils s.v. pfl. Beschäftigung von Ausländern",
        "Engpassindikator Abgangsrate aus Arbeitslosigkeit" = "Abgangsrate aus Arbeitslosigkeit",
        "Engpassindikator Entwicklung der mittleren Entgelte" = "Entwicklung der mittleren Entgelte",
        "Anzahl Indikatoren Engpassanalyse" = "Anzahl der bewerteten Indikatoren...9",
        "wert_EPA" = "Durchschnittliche Punktezahl...11",
        "Risikoindikator Veränderung des Anteils älterer Beschäftigter (60 Jahre und älter)" = "Veränderung des Anteils älterer Beschäftigter (60 Jahre und älter)",
        "Risikoindikator Substituierbarkeitspotenzial" = "Substituierbarkeitspotenzial",
        "Anzahl Indikatoren Risikoanalyse" = "Anzahl der bewerteten Indikatoren...14",
        "wert_Risiko" = "Durchschnittliche Punktezahl...16",
        "Ergänzungsindikator Berufliche Mobilität" = "Berufliche Mobilität",
        "Ergänzungsindikator Arbeitsstellenbestandsquote" = "Arbeitsstellenbestandsquote",
        "Ergänzungsindikator Teilzeitquote" = "Teilzeitquote",
        "Ergänzungsindikator Selbständigenanteil" = "Selbständigenanteil" )
  }

  # als numerisch speichern für pivote_longer
  if(df$anforderung[1] == "Fachkräfte") {
    df[,c(2, 4:23)] <- sapply(df[,c(2, 4:23)], as.numeric)
  }else{
    df[,c(2:20)] <- sapply(df[,c(2:20)], as.numeric)
  }

  # nicht interessante Spalten noch raus
  if(df$anforderung[1] == "Fachkräfte") {
    df <- df %>% dplyr::select(-c("Punktezahl...13", "Punktezahl...20"))
  }else{
    df <- df %>% dplyr::select(-c("Punktezahl...10", "Punktezahl...15"))
  }

  # in long-format bringen
  if(df$anforderung[1] == "Fachkräfte"){
    df <- tidyr::pivot_longer(df, cols = c("Engpassindikator Vakanzzeit":"Ergänzungsindikator Teilzeitquote"),
                              values_to = "wert", names_to = "indikator")
  }else{
    df <- tidyr::pivot_longer(df, cols = c("Engpassindikator Vakanzzeit":"Ergänzungsindikator Selbständigenanteil"),
                              values_to = "wert", names_to = "indikator")
  }
  # indikator ergänzen
  df <- df %>%
    dplyr::mutate(kategorie = dplyr::case_when(
      grepl("Engpass", df$indikator) ~ "Engpassanalyse",
      grepl("EPA", df$indikator) ~ "Engpassanalyse",
      grepl("Risiko", df$indikator) ~ "Risikoanalyse",
      grepl("Ergän", df$indikator) ~ "Ergänzungsindikatoren"
    )
    )

  # indikator kürzen
  df$indikator <- gsub(pattern = "Engpassindikator ", "", df$indikator)
  df$indikator <- gsub(pattern = "Risikoindikator ", "", df$indikator)
  df$indikator <- gsub(pattern = "Ergänzungsindikator ", "", df$indikator)

  # gesamtwert wieder als extra Spalte - logischer zum weiterarbeiten damit
  df_ges <- df %>%
    dplyr::filter(indikator %in% c("wert_EPA", "wert_Risiko")) %>%
    dplyr::rename(gesamtwert = wert) %>%
    dplyr::select(-indikator)

  if(df$anforderung[1] == "Fachkräfte"){
    df <- df %>%
      dplyr::left_join(df_ges, by = c("beruf", "Anzahl Beschäftigte", "Geregelte Berufsausbildung", "jahr", "anforderung",
                                      "kategorie"),
                       relationship = "many-to-many")
  }else{
    df <- df %>%
      dplyr::left_join(df_ges, by = c("beruf", "Anzahl Beschäftigte", "jahr", "anforderung",
                                      "kategorie"),
                       relationship = "many-to-many")
  }

  df <- df %>% dplyr::filter(!(indikator %in% c("wert_EPA", "wert_Risiko")))

  # Anzahl vorliegender Indikatoren als extra Spalte - logischer zum weiterarbeiten damit
  df_anz <- df %>%
    dplyr::filter(grepl("Anzahl", indikator)) %>%
    dplyr::rename(indikator_anzahl = wert) %>%
    dplyr::select(-indikator)

  if(df$anforderung[1] == "Fachkräfte"){
    df <- df %>%
      dplyr::left_join(df_anz, by = c("beruf", "Anzahl Beschäftigte", "Geregelte Berufsausbildung", "jahr", "anforderung",
                                      "kategorie", "gesamtwert"),
                       relationship = "many-to-many")
  }else{
    df <- df %>%
      dplyr::left_join(df_anz, by = c("beruf", "Anzahl Beschäftigte", "jahr", "anforderung",
                                      "kategorie", "gesamtwert"),
                       relationship = "many-to-many")
  }

  df <- df %>% dplyr::filter(!(grepl("Anzahl", indikator)))

  return(df)
}

epa_de21_f <- epa_aufbereiten_21(epa_de21_f)
epa_de21_s <- epa_aufbereiten_21(epa_de21_s)
epa_de21_e <- epa_aufbereiten_21(epa_de21_e)

epa_aufbereiten_22 <- function(df){

  # anforderungsniveau und jahr zuweisen
  if(grepl("_f", deparse(substitute(df)))) df$anforderung <- "Fachkräfte"
  if(ncol(df) == 26){

  }else{
    if(grepl("_e", deparse(substitute(df)))) df$anforderung <- "Expert*innen"
    if(ncol(df) == 26){

    }else{
      if(grepl("_s", deparse(substitute(df)))) df$anforderung <- "Spezialist*innen"
    }
  }

  # leere spalten löschen
  if(df$anforderung[1] == "Fachkräfte") {
    df <- df %>% dplyr::select(-c(`...2`, `...3`))
  }else{
    df <- df %>% dplyr::select(-c(`...21`, `...22`, `...23`, `...24`))
  }

  if(df$anforderung[1] == "Fachkräfte"){
    df <- df %>%
      rename(
        "beruf" = `...1`,
        "Anzahl Beschäftigte" = "sozialversicherungspflichtig Beschäftigte; ohne Arbeitnehmerüberlassung und Azubis (Juni 2022)",
        "Engpassindikator Vakanzzeit" = "Vakanzzeit (Median)",
        "Engpassindikator Arbeitsuchenden-Stellen-Relation" = "Arbeitsuchenden-Stellen-Relation",
        "Engpassindikator Berufssp. Arbeitslosenquote" = "Berufssp. Arbeitslosenquote",
        "Engpassindikator Veränderung des Anteils s.v. pfl. Beschäftigung von Ausländern" = "Veränderung des Anteils s.v. pfl. Beschäftigung von Ausländern",
        "Engpassindikator Abgangsrate aus Arbeitslosigkeit" = "Abgangsrate aus Arbeitslosigkeit",
        "Engpassindikator Entwicklung der mittleren Entgelte" = "Entwicklung der mittleren Entgelte",
        "Anzahl Indikatoren Engpassanalyse" = "Anzahl der bewerteten Indikatoren...12",
        "wert_EPA" = "Durchschnittliche Punktezahl...14",
        "Risikoindikator Veränderung des Anteils älterer Beschäftigter (60 Jahre und älter)" = "Veränderung des Anteils älterer Beschäftigter (60 Jahre und älter)",
        "Risikoindikator Anteil unb. Ausbildungsstellen an allen gem. Ausbildungsstellen" = "Anteil unb. Ausbildungsstellen an allen gem. Ausbildungsstellen",
        "Risikoindikator Absolventen-Beschäftigten-Relation" = "Absolventen-Beschäftigten-Relation",
        "Risikoindikator Substituierbarkeitspotenzial" = "Substituierbarkeitspotenzial",
        "Anzahl Indikatoren Risikoanalyse" = "Anzahl der bewerteten Indikatoren...19",
        "wert_Risiko" = "Durchschnittliche Punktezahl...21",
        "Ergänzungsindikator Berufliche Mobilität" = "Berufliche Mobilität",
        "Ergänzungsindikator Arbeitsstellenbestandsquote" = "Arbeitsstellenbestandsquote",
        "Ergänzungsindikator Teilzeitquote" = "Teilzeitquote" )

  }else{
    df <- df %>%
      rename(
        "beruf" = `...1`,
        "Anzahl Beschäftigte" = "sozialversicherungspflichtig Beschäftigte; ohne Arbeitnehmerüberlassung und Azubis (Juni 2022)",
        "Engpassindikator Vakanzzeit" = "Vakanzzeit (Median)",
        "Engpassindikator Arbeitsuchenden-Stellen-Relation" = "Arbeitsuchenden-Stellen-Relation",
        "Engpassindikator Berufssp. Arbeitslosenquote" = "Berufssp. Arbeitslosenquote",
        "Engpassindikator Veränderung des Anteils s.v. pfl. Beschäftigung von Ausländern" = "Veränderung des Anteils s.v. pfl. Beschäftigung von Ausländern",
        "Engpassindikator Abgangsrate aus Arbeitslosigkeit" = "Abgangsrate aus Arbeitslosigkeit",
        "Engpassindikator Entwicklung der mittleren Entgelte" = "Entwicklung der mittleren Entgelte",
        "Anzahl Indikatoren Engpassanalyse" = "Anzahl der bewerteten Indikatoren...9",
        "wert_EPA" = "Durchschnittliche Punktezahl...11",
        "Risikoindikator Veränderung des Anteils älterer Beschäftigter (60 Jahre und älter)" = "Veränderung des Anteils älterer Beschäftigter (60 Jahre und älter)",
        "Risikoindikator Substituierbarkeitspotenzial" = "Substituierbarkeitspotenzial",
        "Anzahl Indikatoren Risikoanalyse" = "Anzahl der bewerteten Indikatoren...14",
        "wert_Risiko" = "Durchschnittliche Punktezahl...16",
        "Ergänzungsindikator Berufliche Mobilität" = "Berufliche Mobilität",
        "Ergänzungsindikator Arbeitsstellenbestandsquote" = "Arbeitsstellenbestandsquote",
        "Ergänzungsindikator Teilzeitquote" = "Teilzeitquote",
        "Ergänzungsindikator Selbständigenanteil" = "Selbständigenanteil" )
  }

  # als numerisch speichern für pivote_longer
  if(df$anforderung[1] == "Fachkräfte") {
    df[,c(2, 4:23)] <- sapply(df[,c(2, 4:23)], as.numeric)
  }else{
    df[,c(2:20)] <- sapply(df[,c(2:20)], as.numeric)
  }

  # nicht interessante Spalten noch raus
  if(df$anforderung[1] == "Fachkräfte") {
    df <- df %>% dplyr::select(-c("Punktezahl...13", "Punktezahl...20"))
  }else{
    df <- df %>% dplyr::select(-c("Punktezahl...10", "Punktezahl...15"))
  }

  # in long-format bringen
  if(df$anforderung[1] == "Fachkräfte"){
    df <- tidyr::pivot_longer(df, cols = c("Engpassindikator Vakanzzeit":"Ergänzungsindikator Teilzeitquote"),
                              values_to = "wert", names_to = "indikator")
  }else{
    df <- tidyr::pivot_longer(df, cols = c("Engpassindikator Vakanzzeit":"Ergänzungsindikator Selbständigenanteil"),
                              values_to = "wert", names_to = "indikator")
  }

  # indikator ergänzen
  df <- df %>%
    dplyr::mutate(kategorie = dplyr::case_when(
      grepl("Engpass", df$indikator) ~ "Engpassanalyse",
      grepl("EPA", df$indikator) ~ "Engpassanalyse",
      grepl("Risiko", df$indikator) ~ "Risikoanalyse",
      grepl("Ergän", df$indikator) ~ "Ergänzungsindikatoren"
    )
    )

  # indikator kürzen
  df$indikator <- gsub(pattern = "Engpassindikator ", "", df$indikator)
  df$indikator <- gsub(pattern = "Risikoindikator ", "", df$indikator)
  df$indikator <- gsub(pattern = "Ergänzungsindikator ", "", df$indikator)

  # gesamtwert wieder als extra Spalte - logischer zum weiterarbeiten damit
  df_ges <- df %>%
    dplyr::filter(indikator %in% c("wert_EPA", "wert_Risiko")) %>%
    dplyr::rename(gesamtwert = wert) %>%
    dplyr::select(-indikator)

  if(df$anforderung[1] == "Fachkräfte"){
    df <- df %>%
      dplyr::left_join(df_ges, by = c("beruf", "Anzahl Beschäftigte", "Geregelte Berufsausbildung", "jahr", "anforderung",
                                      "kategorie"),
                       relationship = "many-to-many")
  }else{
    df <- df %>%
      dplyr::left_join(df_ges, by = c("beruf", "Anzahl Beschäftigte", "jahr", "anforderung",
                                      "kategorie"),
                       relationship = "many-to-many")
  }

  df <- df %>% dplyr::filter(!(indikator %in% c("wert_EPA", "wert_Risiko")))

  # Anzahl vorliegender Indikatoren als extra Spalte - logischer zum weiterarbeiten damit
  df_anz <- df %>%
    dplyr::filter(grepl("Anzahl", indikator)) %>%
    dplyr::rename(indikator_anzahl = wert) %>%
    dplyr::select(-indikator)

  if(df$anforderung[1] == "Fachkräfte"){
    df <- df %>%
      dplyr::left_join(df_anz, by = c("beruf", "Anzahl Beschäftigte", "Geregelte Berufsausbildung", "jahr", "anforderung",
                                      "kategorie", "gesamtwert"),
                       relationship = "many-to-many")
  }else{
    df <- df %>%
      dplyr::left_join(df_anz, by = c("beruf", "Anzahl Beschäftigte", "jahr", "anforderung",
                                      "kategorie", "gesamtwert"),
                       relationship = "many-to-many")
  }

  df <- df %>% dplyr::filter(!(grepl("Anzahl", indikator)))

  return(df)
}

epa_de22_f <- epa_aufbereiten_22(epa_de22_f)
epa_de22_s <- epa_aufbereiten_22(epa_de22_s)
epa_de22_e <- epa_aufbereiten_22(epa_de22_e)


# alles zusammenfügen und sortieren
## Spalten angleichen
sp_anhängen <- function(df){
  if(df$anforderung[1] != "Fachkräfte"){
    df$geregelte_ausbildung <- "ja"
  }else{
    df <- df %>%
      dplyr::rename(geregelte_ausbildung = `Geregelte Berufsausbildung`)
  }
  return(df)
}

epa_de19_f <- sp_anhängen(epa_de19_f)
epa_de19_s <- sp_anhängen(epa_de19_s)
epa_de19_e <- sp_anhängen(epa_de19_e)

epa_de20_f <- sp_anhängen(epa_de20_f)
epa_de20_s <- sp_anhängen(epa_de20_s)
epa_de20_e <- sp_anhängen(epa_de20_e)

epa_de21_f <- sp_anhängen(epa_de21_f)
epa_de21_s <- sp_anhängen(epa_de21_s)
epa_de21_e <- sp_anhängen(epa_de21_e)

epa_de22_f <- sp_anhängen(epa_de22_f)
epa_de22_s <- sp_anhängen(epa_de22_s)
epa_de22_e <- sp_anhängen(epa_de22_e)

## Zusammenbringen
epa_detail <- rbind(epa_de19_e, epa_de19_f, epa_de19_s,
                    epa_de20_e, epa_de20_f, epa_de20_s,
                    epa_de21_e, epa_de21_f, epa_de21_s,
                    epa_de22_e, epa_de22_f, epa_de22_s)
epa_detail <- subset(epa_detail, !(is.na(epa_detail$beruf)))

# Bezeichnungen von Berufen in Text und Code trennen
epa_detail$beruf_schlüssel <- stringr::str_extract(epa_detail$beruf, "[[:digit:]]+") #zahlen übertragen
epa_detail$beruf <- gsub("[[:digit:]]", " ", epa_detail$beruf) #zahlen entfernen
epa_detail$beruf <- stringr::str_trim(epa_detail$beruf) #Leerzeichen entfernen

# übergeordnete Berufsgruppen ergänzen
epa_detail$berufsgruppe_schlüssel <- substr(epa_detail$beruf_schlüssel, 1, 3)
fachgruppen <- epa[,c("beruf", "beruf_schlüssel")]
fachgruppen <- fachgruppen %>% dplyr::rename(berufsgruppe_schlüssel = beruf_schlüssel,
                                             berufsgruppe = beruf)
fachgruppen <- unique(fachgruppen)
epa_detail <- epa_detail %>%
  dplyr::left_join(fachgruppen, by = "berufsgruppe_schlüssel",
                   relationship = "many-to-many")


# MINT Aggregat zuweisen/berechnen
mint_f <- readxl::read_excel("C:/Users/kbr/Desktop/MINT-Berufe.xlsx", sheet = "Fachkräfte", col_names = TRUE)
mint_s <- readxl::read_excel("C:/Users/kbr/Desktop/MINT-Berufe.xlsx", sheet = "Spezialisten", col_names = TRUE)
mint_e <- readxl::read_excel("C:/Users/kbr/Desktop/MINT-Berufe.xlsx", sheet = "Experten", col_names = TRUE)
mint <- rbind(mint_f, mint_s, mint_e)
mint <- na.omit(mint)
mint$indikator <- mint$Code
mint$indikator <- ifelse(grepl("[[:digit:]]", mint$indikator), NA, mint$indikator)
mint$Code <- ifelse(grepl("[[:digit:]]", mint$Code), mint$Code, NA)
mint$indikator <- stats::ave(mint$indikator, cumsum(!is.na(mint$indikator)), FUN=function(x) x[1])
mint <- mint %>%
  dplyr::mutate(indikator = dplyr::case_when(
    indikator == "MN" ~ "Mathematik, Naturwissenschaften",
    indikator == "I" ~ "Informatik",
    indikator == "LT" ~ "Landtechnik",
    indikator == "PT" ~ "Prdokuktionstechnik",
    indikator == "BT" ~ "Bau- und Gebäudetechnik",
    indikator == "VT" ~ "Verkehrs-, Sicherheits- und Veranstaltungstechnik",
    indikator == "GT" ~ "Gesundheitstechnik",
    T ~ indikator
  ))
mint <- na.omit(mint)
mint$Code <- substr(mint$Code, 1, 4)

mint <- mint %>%
  dplyr::select(-`MINT-Tätigkeiten`) %>%
  dplyr::rename(mint_select = indikator)
mint <- unique(mint)
epa_detail <- epa_detail %>%
  dplyr::left_join(mint, by = join_by(beruf_schlüssel == Code), relationship = "many-to-many")
epa_detail$mint_select <- ifelse(is.na(epa_detail$mint_select), "Nicht MINT", epa_detail$mint_select)

## Sortieren
epa_detail$bereich <- "Arbeitsmarkt"
epa_detail$region <- "Deutschland"
epa_detail <- epa_detail[, c("bereich", "berufsgruppe", "berufsgruppe_schlüssel", "beruf",
                             "beruf_schlüssel", "geregelte_ausbildung", "Anzahl Beschäftigte",
                             "mint_select", "region", "anforderung", "jahr",  "kategorie",
                             "indikator_anzahl", "indikator", "wert", "gesamtwert"
                             )]

## Aggregate Berechnen
# Alle Berufe
alle <- epa_detail %>%
  dplyr::group_by(bereich, jahr, region, anforderung, kategorie, indikator) %>%
  dplyr::summarise(gesamtwert = mean(gesamtwert, na.rm = TRUE),
                   wert = mean(wert, na.rm = TRUE),
                   `Anzahl Beschäftigte` = sum(`Anzahl Beschäftigte`, na.rm = TRUE),
                   indikator_anzahl = mean(indikator_anzahl), na.rm = TRUE)
alle$beruf <- "Gesamt"
alle$beruf_schlüssel <- NA
alle$berufsgruppe <- "Gesamt"
alle$berufsgruppe_schlüssel <- NA
alle$mint_select <- "Gesamt"

# MINT Unterarten
t <- epa_detail %>%
  dplyr::group_by(bereich, mint_select, jahr, region, anforderung, kategorie, indikator) %>%
  dplyr::summarise(gesamtwert = mean(gesamtwert, na.rm = TRUE),
                   wert = mean(wert, na.rm = TRUE),
                   `Anzahl Beschäftigte` = sum(`Anzahl Beschäftigte`, na.rm = TRUE),
                   indikator_anzahl = mean(indikator_anzahl), na.rm = TRUE)

# MINT Berufe
m <- t
m$mint_select <- ifelse(m$mint_select == "Nicht MINT", "Nicht MINT", "MINT")
m <- m %>%
  dplyr::group_by(bereich, mint_select, jahr, region, anforderung, kategorie, indikator) %>%
  dplyr::summarise(gesamtwert = mean(gesamtwert, na.rm = TRUE),
                   wert = mean(wert, na.rm = TRUE),
                   `Anzahl Beschäftigte` = sum(`Anzahl Beschäftigte`, na.rm = TRUE),
                   indikator_anzahl = mean(indikator_anzahl), na.rm = TRUE)

t$beruf <- "Gesamt"
t$beruf_schlüssel <- NA
t$berufsgruppe <- "Gesamt"
t$berufsgruppe_schlüssel <- NA
t$mint_select <- "Gesamt"
t$geregelte_ausbildung <- "Gesamt"
t <- t %>% dplyr::select(-na.rm)

m$beruf <- "Gesamt"
m$beruf_schlüssel <- NA
m$berufsgruppe <- "Gesamt"
m$berufsgruppe_schlüssel <- NA
m$mint_select <- "Gesamt"
m$geregelte_ausbildung <- "Gesamt"
m <- m %>% dplyr::select(-na.rm)

## Filtern
epa_detail <- subset(epa_detail, epa_detail$`Anzahl Beschäftigte`>=500)
epa_detail$delete <- ifelse(epa_detail$kategorie == "Engpassanalyse" &
                              epa_detail$indikator_anzahl < 4, TRUE, FALSE)
epa_detail <- subset(epa_detail, epa_detail$delete == FALSE)
epa_detail <- subset(epa_detail, epa_detail$geregelte_ausbildung == "ja")
epa_detail <- epa_detail %>%
  dplyr::select(-delete, -geregelte_ausbildung) %>%
  dplyr::rename(anzahl_beschäftigte = `Anzahl Beschäftigte`)

## Kategorie-Variable für Engpassrisiko erstellen
epa_detail <- epa_detail %>%
  dplyr::mutate(epa_kat = dplyr::case_when(
    kategorie == "Engpassanalyse" & gesamtwert >= 2.0 ~ "Engpassberuf",
    kategorie == "Engpassanalyse" & gesamtwert < 2.0 & gesamtwert >= 1.5 ~ "Anzeichen eines Engpassberufs",
    kategorie == "Engpassanalyse" & gesamtwert < 1.5 ~ "kein Engpassberuf"
  ))
# epa_detail <- epa_detail %>%
#   dplyr::mutate(risiko_kat = dplyr::case_when(
#     kategorie == "Risikoanalyse" & gesamtwert >= 2.0 ~ "hohes Risiko",
#     kategorie == "Risikoanalyse" & gesamtwert < 2.0 & gesamtwert >= 1.5 ~ "mittleres Risiko",
#     kategorie == "Risikoanalyse" & gesamtwert < 1.5 ~ "geringes/kein Risiko"
#   ))

# in shinyapp:
usethis::use_data(epa_detail, overwrite = T)
