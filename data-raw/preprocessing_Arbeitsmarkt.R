################################################################################
#
# Data Lab
# Vorbereitung Datensatz: Arbeitsmarkt Zeitreihe (Beschäftigte und Auszubildende)
# Author: Katharina Brunner, Januar 2023
# Quelle: C:\Users\kbr\OneDrive - Stifterverband\MINTvernetzt (SV)\MINTv_SV_AP7 MINT-DataLab\02 Datenmaterial\02_Prozess\Datenaufbereitung 2023\Arbeitsmarkt
#
################################################################################

##################### Arbeitsmarkt Zeitreihe ####################################

# Alten/neuen Datensatz einlesen ---------------------------------------------------

# Alternative:
# data_z <- readxl::read_excel(system.file(package="datalab", "data-raw/Arbeitsmarkt.xlsx"), col_names = T)

data_z <- readxl::read_excel("data-raw/Arbeitsmarkt.xlsx", col_names = T)

# Alternative:
# data <- readxl::read_excel(system.file(package="datalab",
#                                        "data-raw/BA006_221123_Besch_MINT.xlsx"),
#                            sheet = "Auswertung", col_names = F, range = "A17:AK7576")

data <- readxl::read_excel("data-raw/BA006_221123_Besch_MINT.xlsx",
                           sheet = "Auswertung", col_names = F, range = "A17:AK7576")

# Spalten zusammenfassen/löschen
data$...1 <- dplyr::coalesce(data$...4, data$...3, data$...2, data$...1) # Regionen in eine Spalte
data$...5 <- dplyr::coalesce(data$...8, data$...7, data$...6, data$...5) # MINT/Niveau in eine Spalte
data <- data[,-c(2,3,4,6,7,8,9)] # nun überflüssige Spalten löschen

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


# Aufbereiten in gewünschte DF-Struktur ---------------------------------

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


# nötige Variablen auswählen/ergänzen -----------------------------------------------

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
    jahr = 2021,
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


# Datensätze zu zusammenlegen vorbereiten ---------------------------------

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

# Spalten in gleiche Reihenfolge bringen
data_z <- data_z[,c("bereich", "indikator", "fachbereich", "geschlecht", "region", "jahr", "anforderung", "wert"
                    #, "hinweise", "quelle"
)]


# Datensatz in data speichern  ------------------------------------------------------

arbeitsmarkt <- rbind(data_z, data_k)
arbeitsmarkt$wert <- as.numeric(arbeitsmarkt$wert)

usethis::use_data(arbeitsmarkt, overwrite = T)
