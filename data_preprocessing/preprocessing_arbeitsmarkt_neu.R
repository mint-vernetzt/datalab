################################################################################
#
# Vorbereitung Datensätze für Unterseite "Ausbildung und Beruf" (neue Version)
# Author: Katharina Brunner, August 2025
# Quelle: Projekt datalab - Ordner data_preprocessing
#
################################################################################

library(dplyr)
library(tidyr)
library(DBI)

#pfad <- "C:/Users/kbr/OneDrive - Stifterverband/MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/02 Datenmaterial/01_Rohdaten/02_Alle Daten/"
pfad <- "C:/Users/mis/OneDrive - Stifterverband/Dateiablage - MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/02 Datenmaterial/01_Rohdaten/02_Alle Daten/"

# arbeitsmarkt_detail -----------------------------------------------------

## Daten Beschäftigte ####

#### 1. Einlesen und Cleaning ####

data <- readxl::read_excel(paste0(pfad, "/BA069_Beschaeftigtendaten_2025.xlsx"),
                               sheet = "Auswertung", col_names = F, range = "A12:AG7540")

data$`...2` <- dplyr::coalesce(
  as.character(data$`...4`),
  as.character(data$`...3`),
  as.character(data$`...2`)
)

#data <- data %>% select(-`...3`, -`...4`, -`...5`)
data$`...3` <- data$`...1`

data <- data %>%
  dplyr::mutate(
    bundesland = dplyr::case_when(
      `...1` == "Deutschland" ~ "Deutschland",
      `...1` == "Westdeutschland" ~ "Westdeutschland (o. Berlin)",
      `...1` == "Ostdeutschland" ~ "Ostdeutschland (inkl. Berlin)",
      `...1` == "Baden-Württemberg" ~ "Baden-Württemberg",
      `...1` == "Bayern" ~ "Bayern",
      `...1` == "Berlin" ~ "Berlin",
      `...1` == "Brandenburg" ~ "Brandenburg",
      `...1` == "Bremen" ~ "Bremen",
      `...1` == "Hamburg" ~ "Hamburg",
      `...1` == "Hessen" ~ "Hessen",
      `...1` == "Mecklenburg-Vorpommern" ~ "Mecklenburg-Vorpommern",
      `...1` == "Niedersachsen" ~ "Niedersachsen",
      `...1` == "Nordrhein-Westfalen" ~ "Nordrhein-Westfalen",
      `...1` == "Rheinland-Pfalz" ~ "Rheinland-Pfalz",
      `...1` == "Saarland" ~ "Saarland",
      `...1` == "Sachsen-Anhalt" ~ "Sachsen-Anhalt",
      `...1` == "Sachsen" ~ "Sachsen",
      `...1` == "Schleswig-Holstein" ~ "Schleswig-Holstein",
      `...1` == "Thüringen" ~ "Thüringen",
      TRUE ~ NA_character_
    )
  ) %>%
  tidyr::separate(`...3`, c("a", "b"), sep = ",", fill = "right") %>%  # falls kein Komma vorhanden
  dplyr::rename(ort = a)

# Schlüsselnummer extrahieren & Ort bereinigen
data$`...4` <- stringr::str_extract(data$ort, "[[:digit:]]+")
data$ort    <- gsub("[[:digit:]]", "", data$ort)
data$ort    <- stringr::str_trim(data$ort)

# Stadt-/Landkreis unterscheiden
help <- as.data.frame(table(data$ort)) %>% dplyr::filter(Freq != 1)
data$ort <- ifelse(data$ort %in% help$Var1 & grepl("tadt", data$b),
                      stringr::str_c("Stadt ", data$ort), data$ort)
data$ort <- ifelse(data$ort %in% help$Var1 & !grepl("tadt", data$b),
                      stringr::str_c("Landkreis ", data$ort), data$ort)

# Oldenburg-Korrektur (Schreibfehler in Originalcode korrigiert)
data$ort <- ifelse(grepl("Olde", data$ort), "Stadt Oldenburg", data$ort)
data$ort <- ifelse(grepl("Olde", data$ort) & is.na(data$b),
                      "Landkreis Oldenburg", data$ort)

# Spalten bennenen
data <- data[,-6]
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
colnames(data) <- header

# leere Zeilen oben löschen
data <- data[-1*1:5,]

# bundesland nach unten auffüllen
data$bundesland <- zoo::na.locf(data$bundesland)

#### 2. Datensatz aufbereiten ####

# Zahlen formatieren und NAs definieren
data <- data %>%
  dplyr::mutate(dplyr::across(c(6:33), as.numeric))

data[data == 0] <- NA

# Ort - Schlüsselnummer - Zusatz ordnen
data$ort <- ifelse(is.na(data$ort), data$region, data$ort)
data$zusatz <- ifelse(is.na(data$zusatz), data$region, data$zusatz)
data$schluesselnummer <- ifelse(is.na(data$schluesselnummer), data$region, data$schluesselnummer)

data$ort <- zoo::na.locf(data$ort)
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

data$fachbereich <- zoo::na.locf(data$fachbereich)


# ins long-Format bringen
data <- data %>%
  tidyr::pivot_longer(cols = "Beschäftigte":"ausländische Beschäftigte ü55 (nur GFB)")

# Entferen von Variablen die (aktuell) nicht analysiert werden
data <- subset(data, !(name %in% c("Beschäftigte", "weibliche Beschäftigte", "Beschäftigte u25", "Beschäftigte ü55",
                                   "Beschäftigte u25 (nur GFB)", "Beschäftigte ü55 (nur GFB)",
                                   "ausländische Beschäftigte", "ausländische weibliche Beschäftigte", "ausländische Beschäftigte u25",
                                   "ausländische Beschäftigte ü55", "ausländische Beschäftigte u25 (nur GFB)",
                                   "ausländische Beschäftigte ü55 (nur GFB)")))
data <- data %>%
  dplyr::mutate(
    bereich = "Arbeitsmarkt",
    jahr = 2025,
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

# bundeslandebene in landkreise "alle Landkreise" nennen
data$landkreis <- ifelse(
  data$landkreis == data$bundesland |
    (data$landkreis == "Westdeutschland" &
       data$bundesland == "Westdeutschland (o. Berlin)") |
    (data$landkreis == "Ostdeutschland" &
       data$bundesland == "Ostdeutschland (inkl. Berlin)"),
  "alle Landkreise",
  data$landkreis
)

data <- data[, c("bereich", "kategorie", "indikator", "fachbereich", "geschlecht", "bundesland",
                 "landkreis", "landkreis_zusatz", "landkreis_nummer", "jahr", "anforderung", "wert")]

#### 3. Berechnungen ####

# Calculate Beschäftigte 25-55
data_alter <- data %>% dplyr::filter(indikator %in% c("Beschäftigte", "Beschäftigte u25", "Beschäftigte ü55"),
                                     geschlecht == "Gesamt")

data_alter <- data_alter %>% dplyr::group_by(bereich, kategorie, fachbereich, geschlecht, bundesland, landkreis,
                                             landkreis_zusatz, landkreis_nummer, jahr, anforderung) %>%
  dplyr::summarise(wert = wert - dplyr::lead(wert, 1) - dplyr::lead(wert, 2)) %>%
  dplyr::mutate(indikator = "Beschäftigte 25-55") %>%
  dplyr::filter(!is.na(wert))


# Calculate ausländische Beschäftigte 25-55
data_ausl_alter <- data %>% dplyr::filter(indikator %in% c("ausländische Beschäftigte", "ausländische Beschäftigte u25", "ausländische Beschäftigte ü55"),
                                          geschlecht == "Gesamt")

data_ausl_alter <- data_ausl_alter %>% dplyr::group_by(bereich, kategorie, fachbereich, geschlecht, bundesland, landkreis,
                                                       landkreis_zusatz, landkreis_nummer, jahr, anforderung) %>%
  dplyr::summarise(wert = wert - dplyr::lead(wert, 1) - dplyr::lead(wert, 2)) %>%
  dplyr::mutate(indikator = "ausländische Beschäftigte 25-55") %>%
  dplyr::filter(!is.na(wert))


# Calculate males
data_geschlecht <- data %>% dplyr::filter(!indikator %in% c("Beschäftigte u25", "Beschäftigte ü55",
                                                            "ausländische Beschäftigte u25", "ausländische Beschäftigte ü55")) %>%
  dplyr::group_by(bereich, kategorie, indikator, fachbereich, bundesland, landkreis,
                  landkreis_zusatz, landkreis_nummer, jahr, anforderung) %>%
  dplyr::summarise(wert = wert - dplyr::lead(wert, 1)) %>%
  dplyr::mutate(geschlecht = "Männer") %>%
  dplyr::filter(!is.na(wert))


data <- rbind(data, data_geschlecht, data_alter, data_ausl_alter)


## Daten Auszubildende ####


#### 1. Einlesen und Cleaning ####

data_a <- readxl::read_excel(paste0(pfad, "/BA070_Ausbildung_2026.xlsx"),
                           sheet = "Auswertung", col_names = F, range = "A10:I4191")


data_a$`...2` <- dplyr::coalesce(
  as.character(data_a$`...5`),
  as.character(data_a$`...4`),
  as.character(data_a$`...3`),
  as.character(data_a$`...2`)
)


data_a$`...3` <- data_a$`...1`


data_a <- data_a %>%
  dplyr::mutate(
    bundesland = dplyr::case_when(
      `...1` == "Deutschland" ~ "Deutschland",
      `...1` == "Westdeutschland" ~ "Westdeutschland (o. Berlin)",
      `...1` == "Ostdeutschland" ~ "Ostdeutschland (inkl. Berlin)",
      `...1` == "Baden-Württemberg" ~ "Baden-Württemberg",
      `...1` == "Bayern" ~ "Bayern",
      `...1` == "Berlin" ~ "Berlin",
      `...1` == "Brandenburg" ~ "Brandenburg",
      `...1` == "Bremen" ~ "Bremen",
      `...1` == "Hamburg" ~ "Hamburg",
      `...1` == "Hessen" ~ "Hessen",
      `...1` == "Mecklenburg-Vorpommern" ~ "Mecklenburg-Vorpommern",
      `...1` == "Niedersachsen" ~ "Niedersachsen",
      `...1` == "Nordrhein-Westfalen" ~ "Nordrhein-Westfalen",
      `...1` == "Rheinland-Pfalz" ~ "Rheinland-Pfalz",
      `...1` == "Saarland" ~ "Saarland",
      `...1` == "Sachsen-Anhalt" ~ "Sachsen-Anhalt",
      `...1` == "Sachsen" ~ "Sachsen",
      `...1` == "Schleswig-Holstein" ~ "Schleswig-Holstein",
      `...1` == "Thüringen" ~ "Thüringen",
      TRUE ~ NA_character_
    )
  ) %>%
  tidyr::separate(`...3`, c("a", "b"), sep = ",", fill = "right") %>%  # falls kein Komma vorhanden
  dplyr::rename(ort = a)




# Schlüsselnummer extrahieren & Ort bereinigen
data_a$`...4` <- stringr::str_extract(data_a$ort, "[[:digit:]]+")
data_a$ort    <- gsub("[[:digit:]]", "", data_a$ort)
data_a$ort    <- stringr::str_trim(data_a$ort)

# Stadt-/Landkreis unterscheiden
help <- as.data.frame(table(data_a$ort)) %>% dplyr::filter(Freq != 1)
data_a$ort <- ifelse(data_a$ort %in% help$Var1 & grepl("tadt", data_a$b),
                   stringr::str_c("Stadt ", data_a$ort), data_a$ort)
data_a$ort <- ifelse(data_a$ort %in% help$Var1 & !grepl("tadt", data_a$b),
                   stringr::str_c("Landkreis ", data_a$ort), data_a$ort)

# Städtenamen korrigieren
data_a$ort <- ifelse(grepl("Olde", data_a$ort), "Stadt Oldenburg", data_a$ort)
data_a$ort <- ifelse(grepl("Olde", data_a$ort) & is.na(data_a$b),
                   "Landkreis Oldenburg", data_a$ort)



# Spalten bennenen
data_a <- data_a[, -c(6, 7)]
header <- c("region", "fachbereich", "ort", "zusatz", "schluesselnummer",
            #
            "Auszubildende im 1. Lehrjahr Gesamt",
            "männliche Auszubildende im 1. Lehrjahr",
            "weibliche Auszubildende im 1. Lehrjahr",
            #
            "bundesland"
)
colnames(data_a) <- header

# leere Zeilen oben löschen
data_a <- data_a[-1*1:2,]

# bundesland nach unten auffüllen
data_a$bundesland <- zoo::na.locf(data_a$bundesland)




# Ortsnamen richtig benennen

# orte <- unique(data_a$ort)
# orte_sonderz <- orte[grepl("-", orte)]
# An KI geben, aussortieren lassen, welche fälschlich Trennung mit - enthalten

orte_sonderz <- readxl::read_excel(paste0(pfad, "/BA071_Ortsnamen_Klassifizierung.xlsx"))


data_a <- data_a %>%
  left_join(
    orte_sonderz %>%
      filter(!is.na(`Falsch mit Bindestrich`)) %>%
      select(
        ort = `Falsch mit Bindestrich`,
        ort_neu = `Sollte ohne Bindestrich (korrigiert)`
      ),
    by = "ort"
  ) %>%
  mutate(
    ort = coalesce(ort_neu, ort)
  ) %>%
  select(-ort_neu)



# spalten auffüllen
data_a <- data_a %>%
  mutate(gruppe = cumsum(!is.na(region))) %>%
  group_by(gruppe) %>%
  fill(ort, zusatz, schluesselnummer, .direction = "down") %>%
  ungroup() %>%
  select(-gruppe)


### 2. Datensatz aufbereiten ####


# Zahlen formatieren und NAs definieren
data_a <- data_a %>%
  dplyr::mutate(dplyr::across(c(6:8), as.numeric))

data_a[data_a == 0] <- NA


# region ordnen
data_a <- data_a %>%
  dplyr::filter(!is.na(fachbereich))%>%
  dplyr::select(-region)%>%
  dplyr::rename(region = ort)


# ins long-Format bringen
data_a <- data_a %>%
  tidyr::pivot_longer(cols = "Auszubildende im 1. Lehrjahr Gesamt":"weibliche Auszubildende im 1. Lehrjahr")


# umbenennen
data_a$name[data_a$name == "Auszubildende im 1. Lehrjahr Gesamt"]<-"Gesamt"
data_a$name[data_a$name == "weibliche Auszubildende im 1. Lehrjahr"]<-"Frauen"
data_a$name[data_a$name == "männliche Auszubildende im 1. Lehrjahr"]<-"Männer"


# notwendige spalten erzeugen
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
      bereich = "Arbeitsmarkt",
      jahr = "2025",
      anforderung = "Gesamt") %>%
    dplyr::rename(geschlecht = name,
                  wert = value,
                  landkreis = region,
                  landkreis_zusatz = zusatz,
                  landkreis_nummer = schluesselnummer
    )


   # bundeslandebene in landkreise "alle Landkreise" nennen
  data_a$landkreis <- ifelse(
    data_a$landkreis == data_a$bundesland |
      (data_a$landkreis == "Westdeutschland" &
         data_a$bundesland == "Westdeutschland (o. Berlin)") |
      (data_a$landkreis == "Ostdeutschland" &
         data_a$bundesland == "Ostdeutschland (inkl. Berlin)"),
    "alle Landkreise",
    data_a$landkreis
  )



  # Spalten in logische Reihenfolge bringen
  data_a <- data_a[,c("bereich","kategorie", "indikator", "fachbereich", "geschlecht",
                      "bundesland", "landkreis", "landkreis_zusatz", "landkreis_nummer",
                      "jahr", "anforderung", "wert"  )]


#### Zusammenfügen arbeitsmarkt_detail -----

data <- rbind(data, data_a)


## Datensatz aktualisieren und in Datenbank spielen

  library(DBI)
  con <- DBI::dbConnect(duckdb::duckdb(), "data/mint_db.duckdb")

  arbeitsmarkt_detail <- dbGetQuery(con, "SELECT * FROM arbeitsmarkt_detail")

  arbeitsmarkt_detail <- arbeitsmarkt_detail %>%
    dplyr::filter(jahr != 2025)

  arbeitsmarkt_detail <- rbind(arbeitsmarkt_detail, data)

  save(arbeitsmarkt_detail, file = "arbeitsmarkt_detail.rda")

  dbWriteTable(con, "arbeitsmarkt_detail", arbeitsmarkt_detail, append = FALSE, overwrite = TRUE)

  dbDisconnect(con, shutdown= TRUE)











# epa / epa_detail -------------------------------------------------------

akro <- "kbr"
pfad <- paste0("C:/Users/", akro,
               "/OneDrive - Stifterverband/MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/02 Datenmaterial/01_Rohdaten/02_Alle Daten/")

# Daten einlesen, bennennen, filtern
epa <- utils::read.csv2(file = paste0(pfad, "/BA068_epa_2025.csv"))

colnames(epa) <- c("jahr",
                   "beruf_schlüssel",
                   "beruf",
                   "anforderung_id",
                   "anforderung",
                   "bundeslland_id",
                   "region",
                   "wert_ges",
                   "kl_vakanz",
                   "vakanz",
                   "kl_asr",
                   "asr",
                   "kl_aquote",
                   "aquote",
                   "kl_zuw",
                   "zuw",
                   "kl_aba",
                   "aba",
                   "kl_entgl",
                   "entgl",
                   "kategorie",
                   "X")

epa <- epa %>%
  select(jahr, beruf_schlüssel, beruf, anforderung, region, kl_vakanz, kl_asr, kl_aquote, kl_zuw,
         kl_aba, kl_entgl, kategorie, wert_ges) %>%
  filter(kategorie != "keine Bewertung möglich") %>%
  mutate(across(all_of(c("kl_vakanz", "kl_asr", "kl_aquote",
                         "kl_zuw", "kl_aba", "kl_entgl")), ~ na_if(.x, "X")))

# Variablen ergänzen und Format anpassen
epa <- epa %>%
  mutate(indikator_anzahl = rowSums(!(is.na(epa[c("kl_vakanz", "kl_asr", "kl_aquote",
                                                  "kl_zuw", "kl_aba", "kl_entgl")]))),
         bereich = "Arbeitsmarkt")

epa <- epa[,c("bereich", "beruf_schlüssel", "beruf", "region", "anforderung", "jahr", "kategorie", "indikator_anzahl",
              "kl_vakanz", "kl_asr", "kl_aquote", "kl_zuw", "kl_aba", "kl_entgl", "wert_ges")]

epa <- epa %>%
  tidyr::pivot_longer(cols = c("kl_vakanz", "kl_asr", "kl_aquote", "kl_zuw", "kl_aba", "kl_entgl", "wert_ges"),
                      values_to = "wert",
                      names_to = "indikator") %>%
  mutate(indikator = case_when(
    indikator == "kl_vakanz" ~ "Vakanzzeit",
    indikator == "kl_asr" ~ "Arbeitssuchenden-Sellen-Relation",
    indikator == "kl_aquote" ~ "Berufssp. Arbeitslosenquote",
    indikator == "kl_zuw"~ "Veränderung des Anteils s.v. pfl. Beschäftigung von Ausländern",
    indikator == "kl_aba" ~ "Abgangsrate aus Arbeitslosigkeit",
    indikator ==  "kl_entgl" ~ "Entwicklung der mittleren Entgelte",
    indikator ==  "wert_ges" ~ "Engpassindikator"
  ))

# Datensätze trennen und MINT-Kategorisierung ergänzen

epa_detail <- epa %>%
  filter(region == "Deutschland - Berufsuntergruppen")

epa <- epa %>%
  filter(region != "Deutschland - Berufsuntergruppen",
         region != "Deutschland")

epa$region[epa$region == "Deutschland - Berufsgruppen"] <- "Deutschland"

# Schlüssel für MINT-Zuordnung einlesen
mint_f <- readxl::read_excel(paste0(pfad, "BA018_MINT-Berufe.xlsx"), sheet = "Fachkräfte", col_names = TRUE)
mint_s <- readxl::read_excel(paste0(pfad, "BA018_MINT-Berufe.xlsx"), sheet = "Spezialisten", col_names = TRUE)
mint_e <- readxl::read_excel(paste0(pfad, "BA018_MINT-Berufe.xlsx"), sheet = "Experten", col_names = TRUE)
mint <- rbind(mint_f, mint_s, mint_e)
mint <- na.omit(mint)

mint$Code <- ifelse(grepl("[[:digit:]]", mint$Code), mint$Code, NA)

mint <- mint %>%
  rename(indikator = Bereich) %>%
  dplyr::mutate(indikator = dplyr::case_when(
    indikator == "MN" ~ "Mathematik, Naturwissenschaften",
    indikator == "I" ~ "Informatik",
    indikator == "LT" ~ "Landtechnik",
    indikator == "PT" ~ "Produktionstechnik",
    indikator == "BT" ~ "Bau- und Gebäudetechnik",
    indikator == "VT" ~ "Verkehrs-, Sicherheits- und Veranstaltungstechnik",
    indikator == "GT" ~ "Gesundheitstechnik",
    T ~ indikator
  ))
mint <- na.omit(mint)
mint <- mint %>%
  mutate(anforderung = case_when(
    substr(Code, 5, 5) == "2" ~ "Fachkräfte",
    substr(Code, 5, 5) == "3" ~ "Spezialisten",
    substr(Code, 5, 5) == "4" ~ "Experten",
  ))
mint$Code <- substr(mint$Code, 1, 4)

mint <- mint %>%
  dplyr::select(-`MINT-Tätigkeiten`) %>%
  dplyr::rename(mint_zuordnung = indikator)
mint <- unique(mint)

#### epa_detail ----

epa_detail <- epa_detail %>%
  dplyr::left_join(mint, by = join_by(beruf_schlüssel == Code, anforderung), relationship = "many-to-many")
epa_detail$mint_zuordnung <- ifelse(is.na(epa_detail$mint_zuordnung), "Nicht MINT", epa_detail$mint_zuordnung)
epa_detail <- epa_detail %>%
  mutate(jahr = as.numeric(jahr),
         wert = as.numeric(sub(",", ".", wert)),
         indikator_anzahl = as.numeric(indikator_anzahl))

## Aggregate Berechnen
# Alle Berufe
alle <- epa_detail %>%
  dplyr::group_by(bereich, jahr, region, anforderung, indikator) %>%
  dplyr::summarise(wert = mean(wert, na.rm = TRUE),
                   indikator_anzahl = mean(indikator_anzahl, na.rm = TRUE)) %>%
  ungroup()

alle <- alle %>%
  group_by(bereich, jahr, region, anforderung) %>%
  mutate(
    kategorie = case_when(
      wert[indikator == "Engpassindikator"] > 1.9 ~ "Engpassberuf",
      wert[indikator == "Engpassindikator"] > 1.4 & wert[indikator == "Engpassindikator"] < 2 ~ "unter Beobachtung",
      T ~ "kein Engpassberuf"
    )
  ) %>%
  ungroup()

alle$beruf <- "Gesamt"
alle$beruf_schlüssel <- NA
alle$mint_zuordnung <- "Gesamt"
alle <- alle[, c("bereich", "beruf_schlüssel", "beruf",
                 "mint_zuordnung", "region", "anforderung", "jahr",  "kategorie",
                 "indikator_anzahl", "indikator", "wert")]


epa_detail <- epa_detail[, c("bereich", "beruf_schlüssel", "beruf",
                             "mint_zuordnung", "region", "anforderung", "jahr",  "kategorie",
                             "indikator_anzahl", "indikator", "wert")]

epa_detail <- rbind(epa_detail, alle)

epa_detail <- epa_detail %>%
  mutate(berufsgruppe = NA,
         berufsgruppe_schlüssel = NA,
         epa_kat = kategorie,
         kategorie = "Engpassanalyse")

epa_detail <- epa_detail[, c("bereich", "berufsgruppe", "berufsgruppe_schlüssel",
                             "beruf",  "beruf_schlüssel",
                             "mint_zuordnung", "region", "anforderung", "jahr",  "kategorie",
                             "indikator_anzahl", "indikator", "wert", "epa_kat")]

epa_detail$epa_kat[epa_detail$epa_kat == "unter Beobachtung"] <- "Anzeichen eines Engpassberufs"
epa_detail$epa_kat[epa_detail$epa_kat == "kein Engpassberuf"] <- "Kein Engpassberuf"
epa_detail$region <- "Deutschland"
epa_detail <- epa_detail %>% filter(jahr > 2024)

## Datensatz aktualisieren und in Datenbank spielen

library(DBI)
con <- DBI::dbConnect(duckdb::duckdb(), "data/mint_db.duckdb")

arbeitsmarkt_epa_detail <- dbGetQuery(con, "SELECT * FROM arbeitsmarkt_epa_detail")

arbeitsmarkt_epa_detail <- arbeitsmarkt_epa_detail %>% select(-anzahl_beschäftigte)
arbeitsmarkt_epa_detail <- arbeitsmarkt_epa_detail %>%
  mutate(anforderung = case_when(
    anforderung %in% c("Spezialist*innen", "Spezialisten") ~ "Spezialist:innen",
    anforderung %in% c("Expert*innen", "Experten") ~ "Expert:innen",
    T ~ anforderung
  ))

arbeitsmarkt_epa_detail <- rbind(arbeitsmarkt_epa_detail, epa_detail)

save(arbeitsmarkt_epa_detail, file = "arbeitsmarkt_epa_detail.rda")

dbWriteTable(con, "arbeitsmarkt_epa_detail", arbeitsmarkt_epa_detail, append = FALSE, overwrite = TRUE)

dbDisconnect(con, shutdown= TRUE)

#### epa ----

epa <- epa %>%
  rename(berufsgruppe = beruf,
         berufsgruppe_schlüssel = beruf_schlüssel,
         epa_kat = kategorie) %>%
  mutate(kategorie = "Engpassanalyse",
         wert = as.numeric(sub(",", ".", wert)))

mint$Code <- substr(mint$Code, 1, 3)
mint <- unique(mint)

epa <- epa %>%
  dplyr::left_join(mint, by = join_by(berufsgruppe_schlüssel == Code, anforderung), relationship = "many-to-many")
epa$mint_zuordnung <- ifelse(is.na(epa$mint_zuordnung), "Nicht MINT", epa$mint_zuordnung)


## Datensatz aktualisieren und in Datenbank spielen

library(DBI)
con <- DBI::dbConnect(duckdb::duckdb(), "data/mint_db.duckdb")

arbeitsmarkt_epa <- dbGetQuery(con, "SELECT * FROM arbeitsmarkt_epa")

arbeitsmarkt_epa <- arbeitsmarkt_epa %>% select(-anzahl_beschäftigte,
                                                -beruf,
                                                -beruf_schlüssel)
arbeitsmarkt_epa <- arbeitsmarkt_epa %>%
  mutate(anforderung = case_when(
    anforderung %in% c("Spezialist*innen", "Spezialisten") ~ "Spezialist:innen",
    anforderung %in% c("Expert*innen", "Experten") ~ "Expert:innen",
    T ~ anforderung
  ))

epa <- epa %>%
  mutate(anforderung = case_when(
    anforderung %in% c("Spezialist*innen", "Spezialisten") ~ "Spezialist:innen",
    anforderung %in% c("Expert*innen", "Experten") ~ "Expert:innen",
    T ~ anforderung
  ),
  epa_kat = case_when(
    epa_kat == "unter Beobachtung" ~ "Anzeichen eines Engpassberufs",
    epa_kat == "kein Engpassberuf" ~ "Kein Engpassberuf",
    T ~ epa_kat
  )) %>%
  filter(indikator == "Engpassindikator" & jahr == 2025)

epa <- epa[,c("bereich", "berufsgruppe", "berufsgruppe_schlüssel", "mint_zuordnung", "region",
              "anforderung", "jahr", "kategorie", "indikator_anzahl", "indikator", "wert", "epa_kat")]

arbeitsmarkt_epa <- rbind(arbeitsmarkt_epa, epa)

save(arbeitsmarkt_epa, file = "arbeitsmarkt_epa.rda")

dbWriteTable(con, "arbeitsmarkt_epa", arbeitsmarkt_epa, append = FALSE, overwrite = TRUE)

dbDisconnect(con, shutdown= TRUE)







# arbeitsmarkt_fachkraefte    -----------------------------
# Arbeitslosten-Stellen-Relation + Vakanzzeit

## Daten einlesen

pfad <- "C:/Users/mis/OneDrive - Stifterverband/Dateiablage - MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/02 Datenmaterial/01_Rohdaten/01_Eingang/Bundesagentur für Arbeit/2026/"



sheets <- c("Deutschland", "01 Schleswig-Holstein", "02 Hamburg", "03 Niedersachsen",
            "04 Bremen", "05 NRW", "06 Hessen", "07 Rheinland-Pfalz", "08 Baden-Württemberg",
            "09 Bayern", "10 Saarland", "11 Berlin", "12 Brandenburg", "13 Mecklenburg-Vorpommern",
            "14 Sachsen", "15 Sachsen-Anhalt", "16 Thüringen")


de <- readxl::read_excel(paste0(pfad, "EA_408228_MINT_Berufe_ALO_ZR.xlsx"), sheet = sheets[1])
sh <- readxl::read_excel(paste0(pfad, "EA_408228_MINT_Berufe_ALO_ZR.xlsx"), sheet = sheets[2])
ha <- readxl::read_excel(paste0(pfad, "EA_408228_MINT_Berufe_ALO_ZR.xlsx"), sheet = sheets[3])
ni <- readxl::read_excel(paste0(pfad, "EA_408228_MINT_Berufe_ALO_ZR.xlsx"), sheet = sheets[4])
br <- readxl::read_excel(paste0(pfad, "EA_408228_MINT_Berufe_ALO_ZR.xlsx"), sheet = sheets[5])
nr <- readxl::read_excel(paste0(pfad, "EA_408228_MINT_Berufe_ALO_ZR.xlsx"), sheet = sheets[6])
he <- readxl::read_excel(paste0(pfad, "EA_408228_MINT_Berufe_ALO_ZR.xlsx"), sheet = sheets[7])
rp <- readxl::read_excel(paste0(pfad, "EA_408228_MINT_Berufe_ALO_ZR.xlsx"), sheet = sheets[8])
bw <- readxl::read_excel(paste0(pfad, "EA_408228_MINT_Berufe_ALO_ZR.xlsx"), sheet = sheets[9])
by <- readxl::read_excel(paste0(pfad, "EA_408228_MINT_Berufe_ALO_ZR.xlsx"), sheet = sheets[10])
sr <- readxl::read_excel(paste0(pfad, "EA_408228_MINT_Berufe_ALO_ZR.xlsx"), sheet = sheets[11])
be <- readxl::read_excel(paste0(pfad, "EA_408228_MINT_Berufe_ALO_ZR.xlsx"), sheet = sheets[12])
ba <- readxl::read_excel(paste0(pfad, "EA_408228_MINT_Berufe_ALO_ZR.xlsx"), sheet = sheets[13])
mv <- readxl::read_excel(paste0(pfad, "EA_408228_MINT_Berufe_ALO_ZR.xlsx"), sheet = sheets[14])
sa <- readxl::read_excel(paste0(pfad, "EA_408228_MINT_Berufe_ALO_ZR.xlsx"), sheet = sheets[15])
sn <- readxl::read_excel(paste0(pfad, "EA_408228_MINT_Berufe_ALO_ZR.xlsx"), sheet = sheets[16])
th <- readxl::read_excel(paste0(pfad, "EA_408228_MINT_Berufe_ALO_ZR.xlsx"), sheet = sheets[17])


## Daten zusammenfügen und region ergänzen

de$region <- "Deutschland"
sh$region <- "Schleswig-Holstein"
ha$region <- "Hamburg"
ni$region <- "Niedersachsen"
br$region <- "Bremen"
nr$region <- "Nordrhein-Westfalen"
he$region <- "Hessen"
rp$region <- "Rheinland-Pfalz"
bw$region <- "Baden-Württemberg"
by$region <- "Bayern"
sr$region <- "Saarland"
be$region <- "Berlin"
ba$region <- "Brandenburg"
mv$region <- "Mecklenburg-Vorpommern"
sa$region <- "Sachsen"
sn$region <- "Sachsen-Anhalt"
th$region <- "Thüringen"



bulas_aufbereiten <- function(dat){

  dat <- dat[-c(1:9, 511:523),]

  colnames(dat) <- c(
    "fachbereich",
    "Gemeldete Arbeitslose",
    "Gemeldete Stellen",
    "Arbeitslosen-Stellen-Relation",
    "Abgang (Jahressumme)",
    "Abgeschlossene Vakanzzeit",
    "region"
  )

  dat <- tidyr::pivot_longer(
    dat,
    cols = "Gemeldete Arbeitslose":"Abgeschlossene Vakanzzeit",
    values_to = "wert",
    names_to = "indikator"
  )
  dat$beruf <- dat$fachbereich
  dat$jahr <- 2025



  dat <- dat %>%
    dplyr::mutate(
      bereich = "Arbeitsmarkt",
      anforderung = "Gesamt",
      anforderung = dplyr::case_when(
        grepl("Fachk", fachbereich) ~ "Fachkräfte",
        grepl("Aufsic", fachbereich) ~ "Spezialist*innen",
        grepl("Spezialis", fachbereich) ~ "Spezialist*innen",
        grepl("Expert", fachbereich) ~ "Expert*innen",
        grepl("Führung", fachbereich) ~ "Expert*innen",
        T ~ anforderung
      ),
      fachbereich = dplyr::case_when(
        grepl("[[:digit:]]", fachbereich) ~ NA,
        T ~ fachbereich
      ))

  dat$fachbereich <- stats::ave(dat$fachbereich, cumsum(!is.na(dat$fachbereich)), FUN = function(x) x[1])
  dat$fachbereich <- gsub(pattern = " - Experten", "", dat$fachbereich)
  dat$fachbereich <- gsub(pattern = " - Fachkräfte", "", dat$fachbereich)
  dat$fachbereich <- gsub(pattern = " - Spezialisten", "", dat$fachbereich)

  dat$beruf <- gsub(pattern = " - Experten", "", dat$beruf)
  dat$beruf <- gsub(pattern = " - Fachkräfte", "", dat$beruf)
  dat$beruf <- gsub(pattern = " - Spezialisten", "", dat$beruf)
  dat$beruf <- gsub(pattern = " - Experte", "", dat$beruf)
  dat$beruf <- gsub(pattern = " - Fachkraft", "", dat$beruf)
  dat$beruf <- gsub(pattern = " - Spezialist", "", dat$beruf)
  dat$beruf <- gsub(pattern = "-Experte", "", dat$beruf)
  dat$beruf <- gsub(pattern = "-Fachkraft", "", dat$beruf)
  dat$beruf <- gsub(pattern = "-Spezialist", "", dat$beruf)
  dat$beruf <- gsub(pattern = " -Experte", "", dat$beruf)
  dat$beruf <- gsub(pattern = " -Fachkraft", "", dat$beruf)
  dat$beruf <- gsub(pattern = " -Spezialist", "", dat$beruf)
  dat$beruf <- gsub("[[:digit:]] ", "", dat$beruf)
  dat$beruf <- gsub("[[:digit:]]", "", dat$beruf)

  dat$indikator <- gsub("[[:digit:]]", "", dat$indikator)
  dat$indikator <- gsub("_", "", dat$indikator)

  dat <- dat %>%
    dplyr::mutate(beruf = dplyr::case_when(
      beruf == "MINT-Berufe" ~ "MINT-Berufe (gesamt)",
      beruf == "Mathematik, Naturwissenschaften" ~ "Mathematik, Naturwissenschaften (gesamt)",
      beruf == "Bau- und Gebäudetechnik" ~ "Bau- und Gebäudetechnik (gesamt)",
      beruf == "Gesundheitstechnik" ~ "Gesundheitstechnik (gesamt)",
      beruf == "Informatik" ~ "Informatik (gesamt)",
      beruf == "Keine MINT-Berufe" ~ "Keine MINT-Berufe (gesamt)",
      beruf == "Landtechnik" ~ "Landtechnik (gesamt)",
      beruf == "Produktionstechnik" ~ "Produktionstechnik (gesamt)",
      beruf == "Verkehrs-, Sicherheits- und Veranstaltungstechnik" ~ "Verkehrs-, Sicherheits- und Veranstaltungstechnik (gesamt)",
      T ~ beruf
    ))

  return(dat)

}


de <- bulas_aufbereiten(de)
sh <- bulas_aufbereiten(sh)
ha <- bulas_aufbereiten(ha)
ni <- bulas_aufbereiten(ni)
br <- bulas_aufbereiten(br)
nr <- bulas_aufbereiten(nr)
he <- bulas_aufbereiten(he)
rp <- bulas_aufbereiten(rp)
bw <- bulas_aufbereiten(bw)
by <- bulas_aufbereiten(by)
sr <- bulas_aufbereiten(sr)
be <- bulas_aufbereiten(be)
ba <- bulas_aufbereiten(ba)
mv <- bulas_aufbereiten(mv)
sa <- bulas_aufbereiten(sa)
sn <- bulas_aufbereiten(sn)
th <- bulas_aufbereiten(th)

dat <- rbind(de, sh, ha, ni, br, nr, he, rp, bw, by, sr, be, ba, mv, sa, sn, th)

## Spalten sortieren
dat <- dat[, c("bereich", "indikator", "fachbereich", "beruf", "anforderung",
               "region", "jahr", "wert")]

dat <- dat %>%
  dplyr::mutate(dplyr::across(c(8), as.numeric))

## speichern
arbeitsmarkt_fachkraefte <- dat
usethis::use_data(arbeitsmarkt_fachkraefte , overwrite = T)

setwd("C:/Users/tko/OneDrive - Stifterverband/2_MINT-Lücke schließen/MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/02 Datenmaterial/02_data/data/")

save(arbeitsmarkt_fachkraefte, file = "arbeitsmarkt_fachkraefte.rda")




