################################################################################
#
# Preprocessing Data Lab
# Vorbereitung Datensatz: SKF001
# Author: Katharina Brunner, Juni 2023
#
################################################################################

library(dplyr)

# kbr
pfad <- "C:/Users/kbr/OneDrive - Stifterverband/MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/02 Datenmaterial/01_Rohdaten/02_Alle Daten"

# Community Plattform Daten -----------------------------------------------

## Organisationen ----

orgas <- read.csv(paste0(pfad,"/CP001_Organisationen.csv"))
orgas <- orgas %>% select(-X)

# Datenaufbereitung
## Anzahl Organisationen
orgas$total_N <- length(unique(orgas$name))
orgas <- subset(orgas, !(is.na(orgas$area)) | !(is.na(orgas$organizationType)))
t <- subset(orgas, !(is.na(orgas$organizationType)))
t$N_types <- length(unique(t$name))
r <- subset(orgas, !(is.na(orgas$area)))
r$N_regio<- length(unique(r$name))


## Long-Format
cp <- tidyr::pivot_longer(orgas,cols = c("area", "organizationType"),
                             names_to = "kategorie", values_to = "indikator")

## Anzahl/Anteil Ausprägungen
types <- cp %>%
  filter(kategorie == "organizationType") %>%
  unique()
types <- types %>%
  count(indikator) %>%
  na.omit() %>%
  rename(wert = n)

types <- types %>%
  mutate(typ = "Organisation",
         kategorie = "Organisationstyp",
         ebene = NA)
#cp <- left_join(cp, types, by = join_by(indikator), keep = FALSE)
types$N <- t$N_types[1]
types$N_total <- cp$total_N[1]

areas <- cp %>%
  filter(kategorie == "area") %>%
  unique()
areas <- areas %>%
  count(indikator) %>%
  na.omit() %>%
  rename(wert = n)

areas_DE <- areas %>%
  filter(indikator == "Bundesweit")
areas_DE$ebene <- "Bundesweit"

bundeslaender_string <- c(
  "Baden-Württemberg", "Bayern", "Berlin", "Brandenburg",
  "Bremen", "Hamburg", "Hessen", "Mecklenburg-Vorpommern",
  "Niedersachsen", "Nordrhein-Westfalen", "Rheinland-Pfalz",
  "Saarland", "Sachsen", "Sachsen-Anhalt", "Schleswig-Holstein", "Thüringen"
)
areas_BULA <- areas %>%
  filter(indikator %in% bundeslaender_string)
areas_BULA$ebene <- "Bundesländer"

areas_LKs <- areas %>%
  filter(!(indikator %in% c("Bundesweit", bundeslaender_string)))
areas_LKs$ebene <- "Landkreise"

areas <- rbind(areas_DE, areas_BULA, areas_LKs)

areas <- areas %>%
  group_by(ebene) %>%
  mutate(
         typ = "Organisation",
         kategorie = "Region") %>%
  ungroup()

areas$N <- r$N_regio[1]
areas$N_total <- cp$total_N[1]

# cp <- left_join(cp, areas, by = join_by(indikator), keep = FALSE) %>%
#   mutate(wert = coalesce(wert.x, wert.y)) %>%
#   mutate(N = coalesce(N.x, N.y)) %>%
#   select(-wert.x, -wert.y, -N.x, -N.y)

# cp <- fill(o, ebene)
# orgas <- orgas %>%
#   mutate(kategorie = case_when(
#     kategorie == "area" ~ "Region",
#     kategorie == "organizationType" ~ "Organisationstyp",
#     TRUE ~ kategorie
#   )) %>%
#   unique()

## zusammenhängen
areas <- areas[, c("typ", "N_total", "N", "kategorie", "ebene", "indikator", "wert")]
types <- types[, c("typ", "N_total", "N", "kategorie", "ebene", "indikator", "wert")]

cp <- rbind(areas, types)

## Envir. aufräumen
all <- ls()
keep <- c("orgas", "pros", "profs", "pfad", "cp")
delete <- setdiff(all, keep)
rm(list=delete)


## Projekte ----

pros <- read.csv(paste0(pfad,"/CP002_Projekte.csv"))
pros <- pros %>% select(-X)

# Anzahl Projekte
pros$total_N <- length(unique(pros$name))
pros <- subset(pros, !(is.na(pros$discipline)) | !(is.na(pros$targetGroup)))
z <- subset(pros, !(is.na(pros$targetGroup)))
z$N_target <- length(unique(z$name))
b <- subset(pros, !(is.na(pros$discipline)))
b$N_bereich<- length(unique(b$name))


# Longformat
cp_n <- tidyr::pivot_longer(pros, cols = c("discipline", "targetGroup"),
                            names_to = "kategorie", values_to = "indikator")

cp_n <- cp_n %>%
  mutate(kategorie = case_when(
           kategorie == "discipline" ~ "MINT-Bereich",
           kategorie == "targetGroup" ~ "Zielgruppe",
           TRUE ~ kategorie
         )) %>%
  unique()

cp_n <- cp_n %>%
  mutate(indikator = case_when(
    indikator %in% c("Erzieher:innen (Kita)",
                     "Erzieher:innen (Schule)",
                     "Pädagogische Fachkräfte") ~ "Erzieher:innen",
    indikator %in% c("Frühkindliche Bildung",
                     "Kinder in der Kita oder Vorschule") ~ "Frühkindliche Bildung",
    indikator %in% c("Kinder Primarstufe",
                     "Primarbereich") ~ "Primarbereich",
    indikator %in% c("Lehrkräfte",
                     "Lehrkräfte Sek. I",
                     "Lehrkräfte Sek. II",
                     "Lehrkräfte Vorschule und Primarstufe") ~ "Lehrkräfte",
    indikator %in% c("Schüler:innen Sek. I",
                     "Schüler:innen Sek. II",
                     "Sek 1",
                     "Sek 2") ~ "Schüler:innen",
    indikator %in% c("Hochschulbildung",
                     "Dozierende Hochschule",
                     "Wissenschaftler:innen") ~ "Hochschulbildung/ Hochschulpersonal",
    TRUE ~ indikator
  )) %>%
  unique()

zielgr <- cp_n %>%
  filter(kategorie == "Zielgruppe") %>%
  select(name, indikator) %>%
  na.omit() %>%
  unique() %>%
  count(indikator) %>%
  rename(wert = n) %>%
  mutate(
    typ = "Projekt",
    kategorie = "Zielgruppe",
    ebene = NA
  )
zielgr$N_total <- cp_n$total_N[1]
zielgr$N <- z$N_target[1]
#pros <- left_join(pros, zielgr, by = join_by("indikator"), keep = FALSE)

mint <- cp_n %>%
  filter(kategorie == "MINT-Bereich") %>%
  select(name, indikator) %>%
  na.omit() %>%
  unique() %>%
  count(indikator) %>%
  rename(wert = n) %>%
  mutate(
    typ = "Projekt",
    kategorie = "MINT-Bereich",
    ebene = NA
  )
mint$N_total <- cp_n$total_N[1]
mint$N <- b$N_bereich[1]
# pros <- left_join(pros, mint, by = join_by("indikator"), keep = FALSE) %>%
#   mutate(wert = coalesce(wert.x, wert.y)) %>%
#   select(-wert.x, -wert.y)

## zusammenhängen
zielgr <- zielgr[, c("typ", "N_total", "N", "kategorie", "ebene", "indikator", "wert")]
mint <- mint[, c("typ", "N_total", "N", "kategorie", "ebene", "indikator", "wert")]

cp<- rbind(cp, zielgr, mint)

## Envir. aufräumen
all <- ls()
keep <- c("orgas", "pros", "profs", "pfad", "cp")
delete <- setdiff(all, keep)
rm(list=delete)


## Profile ----

profs <- read.csv(paste0(pfad,"/CP003_Profile.csv"))
profs <- profs %>% select(-X)

profs <- profs %>%
  rename(Region = area,
         Angebot = offer,
         Interesse = seekingsseeking_offer)

# Anzahl Projekte
profs$total_N <- length(unique(profs$id))
profs <- subset(profs, !(is.na(profs$Region)) | !(is.na(profs$Angebot)) | !(is.na(profs$Interesse)))
r <- subset(profs, !(is.na(profs$Region)))
r$N_region <- length(unique(r$id))
a <- subset(profs, !(is.na(profs$Angebot)))
a$N_angeb<- length(unique(a$id))
i <- subset(profs, !(is.na(profs$Interesse)))
i$N_interesse<- length(unique(i$id))

cp_n <- tidyr::pivot_longer(profs, cols = "Region":"Interesse",
                             names_to = "kategorie", values_to = "indikator")

cp_n <- unique(cp_n)

region <- cp_n %>%
  filter(kategorie == "Region") %>%
  select(id, indikator) %>%
  na.omit() %>%
  count(indikator) %>%
  rename(wert = n)

# nur Bundesweit
region_DE <- region %>%
  filter(indikator == "Bundesweit")
region_DE$ebene <- "Bundesweit"

# nur auf Bundesland ebene
bundeslaender_string <- c(
  "Baden-Württemberg", "Bayern", "Berlin", "Brandenburg",
  "Bremen", "Hamburg", "Hessen", "Mecklenburg-Vorpommern",
  "Niedersachsen", "Nordrhein-Westfalen", "Rheinland-Pfalz",
  "Saarland", "Sachsen", "Sachsen-Anhalt", "Schleswig-Holstein", "Thüringen"
)
region_BULA <- region %>%
  filter(indikator %in% bundeslaender_string)
region_BULA$ebene <- "Bundesländer"

# Landkeis-Ebene
region_LKs <- region %>%
  filter(!(indikator %in% c("Bundesweit", bundeslaender_string)))
region_LKs$ebene <- "Landkreise"

region <- rbind(region_DE, region_BULA, region_LKs)
#profs <- left_join(profs, region, by = join_by("indikator"), keep = FALSE)

region <- region %>%
  mutate(
    typ = "Profile",
    kategorie = "Region"
  )
region$N_total <- cp_n$total_N[1]
region$N <- r$N_region[1]

angeb <- cp_n %>%
  filter(kategorie == "Angebot") %>%
  select(id, indikator) %>%
  na.omit() %>%
  count(indikator) %>%
  rename(wert = n) %>%
  filter(indikator != "") %>%
  mutate(
    typ = "Profile",
    kategorie = "Angebot",
    ebene = NA
  )
angeb$N_total <- cp_n$total_N[1]
angeb$N <- a$N_angeb[1]
# profs <- left_join(profs, angeb, by = join_by("indikator"), keep = FALSE) %>%
#   mutate(wert = coalesce(wert.x, wert.y)) %>%
#   select(-wert.x, -wert.y)

seek <- cp_n %>%
  filter(kategorie == "Interesse") %>%
  select(id, indikator) %>%
  na.omit() %>%
  count(indikator) %>%
  rename(wert = n) %>%
  filter(indikator != "") %>%
  mutate(
    typ = "Profile",
    kategorie = "Gesucht",
    ebene = NA
  )
seek$N_total <- cp_n$total_N[1]
seek$N <- i$N_interesse[1]
# profs <- left_join(profs, seek, by = join_by("indikator"), keep = FALSE) %>%
#   mutate(wert = coalesce(wert.x, wert.y)) %>%
#   select(-wert.x, -wert.y)
#
# profs <- profs %>%
#   group_by(id) %>%
#   fill(ebene) %>%
#   ungroup() %>%
#   na.omit(wert)
#

## zusammenhängen
region <- region[, c("typ", "N_total", "N", "kategorie", "ebene", "indikator", "wert")]
angeb <- angeb[, c("typ", "N_total", "N", "kategorie", "ebene", "indikator", "wert")]
seek <- seek[, c("typ", "N_total", "N", "kategorie", "ebene", "indikator", "wert")]

cp<- rbind(cp, region, angeb, seek)

## Envir. aufräumen
all <- ls()
keep <- c("orgas", "pros", "profs", "pfad", "cp")
delete <- setdiff(all, keep)
rm(list=delete)

## Datensatz CP speichern ----

ausserschulisch_cp <- cp

# Datensatz speichern
usethis::use_data(ausserschulisch_cp, overwrite = T)


# Stiftung Kinder Forschen ------------------------------------------------

## Rohdatensatz einlesen ----

wd <- getwd()
setwd(wd)

# pfad analgen

pfad_kab <- "C:/Users/kab/OneDrive - Stifterverband/AP7 MINT-DataLab/02 Datenmaterial/01_Rohdaten/02_Alle Daten/"

pfad <- pfad_kab

data <- readxl::read_excel(paste0(pfad,"SKF001_230130.xlsx"),
                           sheet = "Datentabelle", col_names = FALSE)


## Datensatz ins passende Format bringen ----

## header erzeugen
i <- 2
header <- character(ncol(data))

for(i in i:10){
  header[1] <- "jahr"
  header[i] <- paste0(data[1, i], " ", data[2,i])
}

# header übertragen
colnames(data)<-header

# überflüssige Zeilen entfernen
data <- data[c(-1,-2),]

#jahr korrigieren - bei Einlesen verzerrt
data$jahr <- 2012:2022

#in long format speichern
data <- tidyr::pivot_longer(data, cols = "Kitas Anzahl aktive Einrichtungen":"Grundschulen Schätzung teilnehmende Fach- / Lehrkräfte")


## Datensatz zur Nutzung aufbereiten ----

# Spalte für Indikator und Ort erstellen
data <- data %>%
  dplyr::mutate(einrichtung = dplyr::case_when(
    grepl("Kita", name) ~ "Kita",
    grepl("Hort", name) ~ "Hort",
    grepl("Gru", name) ~ "Grundschule"
  ),
  indikator = dplyr::case_when(
    grepl("aktive", name) ~ "aktive Einrichtungen gesamt",
    grepl("zertif", name) ~ "zertifizierte Einrichtungen",
    grepl("Schät", name) ~ "insgesamt fortgebildete Fach- / Lehrkräfte"
  )) %>%
  dplyr::rename(wert = value) %>%
  dplyr::select(-name)

# aktive Einrichtungen gesamt = zerfitizierte Einrichtungen + Einrichtungen mit Fortbildung
# Einrichtungen mit Fortbildung bereichnen
data$wert <- ifelse(grepl("keine", data$wert), NA, data$wert)
data$wert <- as.numeric(data$wert)

emf <- data %>%
  dplyr::filter(indikator == "aktive Einrichtungen gesamt")
ze <- data %>%
  dplyr::filter(indikator == "zertifizierte Einrichtungen")

emf <- emf %>%
  dplyr::left_join(ze, c("jahr", "einrichtung")) %>%
  dplyr::mutate(wert = wert.x - wert.y) %>%
  dplyr::select(c(-indikator.x, -indikator.y, -wert.x, -wert.y))
emf$indikator <- "Einrichtungen mit SKf-Fortbildung"

data <- rbind(data, emf)

# Anzahl neuer forgebildeter Fach- / Lehrkräfte berechnen
nf <- data %>%
  dplyr::filter(indikator == "insgesamt fortgebildete Fach- / Lehrkräfte")

i <- 2022
for(i in 2022:2013){
    nf$wert[nf$jahr == i] <- nf$wert[nf$jahr == i] - nf$wert[nf$jahr == i-1]
}
nf$wert[nf$jahr == 2012] <- NA

nf$indikator <- "neu fortgebildete Fach- / Lehrkräfte"

data <- rbind(data, nf)

# bereich Spalte ergänzen
data$bereich <- "Außerschulisch"

# Spalten in logische Reihenfolge bringen
data <- data[,c("bereich", "einrichtung", "indikator", "jahr", "wert")]


## Datensatz abspeichern ----

ausserschulisch_skf <- data

# Datensatz speichern
usethis::use_data(ausserschulisch_skf, overwrite = T)
