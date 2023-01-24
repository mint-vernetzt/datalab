
########### Arbeitsmarkt detailliert - Haupt-Datensatz ########################

# Rohdaten einlesen ---------------------------------------------------

# Einlesen Hauptdatensatz
# data <- readxl::read_excel(system.file(package="datalab",
#                                        "data-raw/BA006_221123_Besch_MINT.xlsx"),
#                            sheet = "Auswertung", col_names = F, range = "A17:AK7576")

# läuft seit neuem Laptop mit Code drüber nicht mehr druch, daher umgeschrieben (kbr)
data <- readxl::read_excel("data-raw/BA006_221123_Besch_MINT.xlsx",
                           sheet = "Auswertung", col_names = F, range = "A17:AK7576")

#[c(17:7576),c(1:37)]

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


######## Arbeitsmarkt detailliert - Azubi-Datensatz #############################

# Rohdaten einlesen ---------------------------------------------------

#Einlesen Auzubi extra Datensatz
# data_a <- readxl::read_excel(system.file(package = "datalab", "data-raw/BA007_221205_AusbV_MINT.xlsx"),
#                            sheet = "Auswertung2", col_names = F, range = "A12:L4201")

# läuft seit neuem Laptop mit Code drüber nicht mehr druch, daher umgeschrieben (kbr)
data_a <- readxl::read_excel("data-raw/BA007_221205_AusbV_MINT.xlsx",
                             sheet = "Auswertung2", col_names = F, range = "A12:L4201")
#[c(12:4201),c(1:12)]

# Spalten zusammenfassen/löschen
data_a$...1 <- dplyr::coalesce(data_a$...4, data_a$...3, data_a$...2, data_a$...1) # Regionen in eine Spalte
data_a$...5 <- dplyr::coalesce(data_a$...8, data_a$...7, data_a$...6, data_a$...5) # MINT in eine Spalte
data_a <- data_a[,-c(2,3,4,6,7,8,9)] # nun überflüssige Spalten löschen

# Header ergänzen
header_a <- c("region", "fachbereich", "gesamt", "männer", "frauen")
colnames(data_a) <- header_a


# Aufbereiten in gewünschte DF-Struktur ---------------------------------

#NA definieren anstelle */0
data_a <- data_a %>%
  dplyr::mutate_all(~replace(., . %in% c(0, "*"), NA))

# Lücken füllen, die durch Zellverbünde entstanden sind
data_a$region <- stats::ave(data_a$region, cumsum(!is.na(data_a$region)), FUN=function(x) x[1])

# Männer entfernen (haben gesamt und frauen)
data_a <- data_a %>%
  dplyr::select(-männer)

# ins long-Format bringen
data_a <- data_a %>%
  tidyr::pivot_longer(cols = "gesamt":"frauen")


# Variablen ergänzen/benennen -----------------------------------------------

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
              wert = value)

data_a$geschlecht[data_a$geschlecht == "frauen"]<-"Frauen"

# Spalten in logische Reihenfolge bringen
data_a <- data_a[,c("bereich","kategorie", "indikator", "fachbereich", "geschlecht", "region", "jahr", "anforderung", "wert"
                    #, "hinweise", "quelle"
)]

######## Arbeitsmarkt detailliert - insgesamt ###################################

arbeitsmarkt_detail <- rbind(data, data_a)


#Wert als numerisch definieren
arbeitsmarkt_detail$wert <- as.numeric(arbeitsmarkt_detail$wert)

usethis::use_data(arbeitsmarkt_detail, overwrite = T)
