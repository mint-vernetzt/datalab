
########### Arbeitsmarkt detailliert - Haupt-Datensatz ########################

# Rohdaten einlesen ---------------------------------------------------

# Einlesen Hauptdatensatz
# data <- readxl::read_excel(system.file(package="datalab",
#                                        "data-raw/BA006_221123_Besch_MINT.xlsx"),
#                            sheet = "Auswertung", col_names = F, range = "A17:AK7576")

# läuft seit neuem Laptop mit Code drüber nicht mehr druch, daher umgeschrieben (kbr)

library(magrittr)

setwd("C:/Users/kab/Downloads/datalab/datalab/data-raw")
path<-"C:/Users/kab/Downloads/datalab/datalab/data-raw/BA006_221123_Besch_MINT.xlsx"
data <- readxl::read_excel("BA006_221123_Besch_MINT.xlsx",
                           sheet = "Auswertung", col_names = F, range = "A17:AK7576")
#
# data <- readxl::read_excel("data-raw/BA006_221123_Besch_MINT.xlsx",
#                            sheet = "Auswertung", col_names = F, range = "A17:AK7576")

# Spalten zusammenfassen/löschen
data$...1 <- dplyr::coalesce(data$...4, data$...3, data$...2, data$...1) # Regionen in eine Spalte
data$...5 <- dplyr::coalesce(data$...8, data$...7, data$...6, data$...5) # MINT/Niveau in eine Spalte

# region formattieren (kab)

data1 <- data %>%
  dplyr::mutate(bundesland=dplyr::case_when(
    ...1 == "Deutschland" ~ "Deutschland",
    ...1 == "Westdeutschland (o. Berlin)" ~"Westdeutschland (o. Berlin)",
    ...1 == "Baden-Württemberg" ~"Baden-Württemberg",
    ...1 == "Bayern" ~"Bayern",
    ...1 == "Berlin" ~"Berlin",
    ...1 == "Brandenburg" ~"Bremen",
    ...1 == "Hessen" ~"Hessen",
    ...1 == "Mecklenburg-Vorpommern" ~"Mecklenburg-Vorpommern",
    ...1 == "Niedersachsen" ~"Niedersachsen",
    ...1 == "Nordrhein-Westfalen" ~"Nordrhein-Westfalen",
    ...1 == "Rheinland-Pfalz" ~"Rheinland-Pfalz",
    ...1 == "Saarland" ~"Saarland",
    ...1 == "Sachsen-Anhalt" ~"Sachsen-Anhalt",
    ...1 == "Sachsen" ~"Sachsen",
    ...1 == "Schleswig-Holstein" ~"Schleswig-Holstein",
    ...1 == "Thüringen" ~"Thüringen",
    ...1 == "Ostdeutschland (einschl. Berlin)" ~"Ostdeutschland (einschl. Berlin)"
    ))%>% tidyr::separate(...4, c("a","b","c"), sep = ",")%>%
      dplyr::rename(ort = a)

data1$bundesland <- zoo::na.locf(data1$bundesland)


data1$c <- ifelse(!grepl("[^A-Za-z]", data1$c), data1$b, data1$c)
data1$b <- ifelse(data1$b == data1$c, NA, data1$b)

colnames(data1)[6] <- "schluesselnummer"
colnames(data1)[5] <- "zusatz"

data <- data1[,-c(2,3,7,8,9,11)] # nun überflüssige Spalten löschen

# Header ergänzen
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


# Aufbereiten in gewünschte DF-Struktur ---------------------------------


# Anpassungen (kab)

#NA definieren anstelle */0
# data <- data %>%
#   dplyr::mutate_all(~dplyr::replace(., . %in% c(0, "*"), NA))

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
# data$fachbereich <- stats::ave(data$fachbereich, cumsum(!is.na(data$fachbereich)), FUN=function(x) x[1])

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
data <- data[,c("bereich", "kategorie", "indikator", "fachbereich", "geschlecht", "region", "bundesland", "schluesselnummer", "zusatz", "jahr", "anforderung", "wert"
                #, "hinweise", "quelle"
)]


######## Arbeitsmarkt detailliert - Azubi-Datensatz #############################

# Rohdaten einlesen ---------------------------------------------------

#Einlesen Auzubi extra Datensatz
# data_a <- readxl::read_excel(system.file(package = "datalab", "data-raw/BA007_221205_AusbV_MINT.xlsx"),
#                            sheet = "Auswertung2", col_names = F, range = "A12:L4201")

# läuft seit neuem Laptop mit Code drüber nicht mehr druch, daher umgeschrieben (kbr)
data_a <- readxl::read_excel("BA007_221205_AusbV_MINT.xlsx",
                             sheet = "Auswertung2", col_names = F, range = "A12:L4201")
# data_a <- readxl::read_excel("data-raw/BA007_221205_AusbV_MINT.xlsx",
#                              sheet = "Auswertung2", col_names = F, range = "A12:L4201")

# Spalten zusammenfassen/löschen
data_a$...1 <- dplyr::coalesce(data_a$...4, data_a$...3, data_a$...2, data_a$...1) # Regionen in eine Spalte
data_a$...5 <- dplyr::coalesce(data_a$...8, data_a$...7, data_a$...6, data_a$...5) # MINT in eine Spalte

data_a1 <- data_a %>%
  dplyr::mutate(bundesland=dplyr::case_when(
    ...1 == "Deutschland" ~ "Deutschland",
    ...1 == "Westdeutschland (o. Berlin)" ~"Westdeutschland (o. Berlin)",
    ...1 == "Baden-Württemberg" ~"Baden-Württemberg",
    ...1 == "Bayern" ~"Bayern",
    ...1 == "Berlin" ~"Berlin",
    ...1 == "Brandenburg" ~"Bremen",
    ...1 == "Hessen" ~"Hessen",
    ...1 == "Mecklenburg-Vorpommern" ~"Mecklenburg-Vorpommern",
    ...1 == "Niedersachsen" ~"Niedersachsen",
    ...1 == "Nordrhein-Westfalen" ~"Nordrhein-Westfalen",
    ...1 == "Rheinland-Pfalz" ~"Rheinland-Pfalz",
    ...1 == "Saarland" ~"Saarland",
    ...1 == "Sachsen-Anhalt" ~"Sachsen-Anhalt",
    ...1 == "Sachsen" ~"Sachsen",
    ...1 == "Schleswig-Holstein" ~"Schleswig-Holstein",
    ...1 == "Thüringen" ~"Thüringen",
    ...1 == "Ostdeutschland (einschl. Berlin)" ~"Ostdeutschland (einschl. Berlin)"
  ))%>% tidyr::separate(...4, c("a","b","c"), sep = ",")%>%
  dplyr::rename(ort = a)

data_a1$bundesland <- zoo::na.locf(data_a1$bundesland)


data_a1$c <- ifelse(!grepl("[^A-Za-z]", data_a1$c), data_a1$b, data_a1$c)
data_a1$b <- ifelse(data_a1$b == data_a1$c, NA, data_a1$b)

colnames(data_a1)[6] <- "schluesselnummer"
colnames(data_a1)[5] <- "zusatz"

data_a1 <- data_a1[,-c(2,3,8:11)] # nun überflüssige Spalten löschen

# Header ergänzen
header_a <- c("region", "ort", "zusatz", "schluesselnummer", "fachbereich", "gesamt", "männer", "frauen", "bundesland")
colnames(data_a1) <- header_a


# Aufbereiten in gewünschte DF-Struktur ---------------------------------

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
data_a <- data_a[,c("bereich","kategorie", "indikator", "fachbereich", "geschlecht", "region", "bundesland", "zusatz", "schluesselnummer", "jahr", "anforderung", "wert"
                    #, "hinweise", "quelle"
)]

######## Arbeitsmarkt detailliert - insgesamt ###################################

arbeitsmarkt_detail <- rbind(data, data_a)

arbeitsmarkt_detail$bundesland <- ifelse(arbeitsmarkt_detail$bundesland==arbeitsmarkt_detail$region, NA, arbeitsmarkt_detail$bundesland)



#Wert als numerisch definieren
arbeitsmarkt_detail$wert <- as.numeric(arbeitsmarkt_detail$wert)

usethis::use_data(arbeitsmarkt_detail, overwrite = T)
