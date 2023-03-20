
# nur 2022 einlesen, bei gleichzeitigem Einlesen von 2017 und 2022 führen 699 abweichende Spalten zu Fehlern im Code (insgesamt > frauen)

# data_naa_a <- readxl::read_excel(system.file(package="datalab",
#                                             "data-raw/BA008_Ausbildungsmarkt-MINT-Frauenanteil-2022.xlsx"),
#                                 sheet  = "Verträge_Daten", range = "A4:D238") #nur Spalten von Anfang
# data_naa <- readxl::read_excel(system.file(package="datalab",
#                                            "data-raw/BA008_Ausbildungsmarkt-MINT-Frauenanteil-2022.xlsx"),
#                                sheet  = "Verträge_Daten", range = "TA4:AMS238") #alles aus 2022, auch Regionen

# läuft mit neuem Laptop oben nicht mehr durch, deshalb umgeschrieben (kbr)
data_naa_a <- readxl::read_excel("data-raw/BA008_Ausbildungsmarkt-MINT-Frauenanteil-2022.xlsx",
                                 sheet  = "Verträge_Daten", range = "A4:D238") #nur Spalten von Anfang

data_naa <- readxl::read_excel("data-raw/BA008_Ausbildungsmarkt-MINT-Frauenanteil-2022.xlsx",
                                sheet  = "Verträge_Daten", range = "TA4:AMS238") #alles aus 2022, auch Regionen

data_naa <- cbind(data_naa_a, data_naa)

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


# nur 2017 einlesen, bei gleichzeitigem Einlesen von 2017 und 2022 führen 699 abweichende Spalten zu Fehlern im Code (insgesamt > frauen)
#
# data_naa <- readxl::read_excel(system.file(package="datalab",
#                                             "data-raw/BA008_Ausbildungsmarkt-MINT-Frauenanteil-2022.xlsx"),
#                                 sheet  = "Verträge_Daten", range = "A4:SW238") #alle Daten aus 2017 incl Regionen

# funktioniert drüber seit neuem Laptop nicht mehr, deshalb umgeschrieben (kbr)
data_naa <- readxl::read_excel("data-raw/BA008_Ausbildungsmarkt-MINT-Frauenanteil-2022.xlsx",
                               sheet  = "Verträge_Daten", range = "A4:SW238") #alle Daten aus 2017 incl Regionen

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


# 2020 einlesen

# data_naa_a <- readxlsb::read_xlsb(system.file(package="datalab",
#                                             "data-raw/BA002_Ausbildungsmarkt-MINT-Frauenanteil-2020.xlsb"),
#                                 sheet  = "Vertraege_Daten", range = "A4:D218")
# data_naa <- readxlsb::read_xlsb(system.file(package="datalab",
#                                             "data-raw/BA002_Ausbildungsmarkt-MINT-Frauenanteil-2020.xlsb"),
#                                 sheet  = "Vertraege_Daten", range = "TA4:AMV218")

# funktioniert seit neuem Laptop nicht mehr so, deshalb umgeschrieben (kbr)
data_naa_a <- readxlsb::read_xlsb("data-raw/BA002_Ausbildungsmarkt-MINT-Frauenanteil-2020.xlsb",
                                  sheet  = "Vertraege_Daten", range = "A4:D218")
data_naa <- readxlsb::read_xlsb("data-raw/BA002_Ausbildungsmarkt-MINT-Frauenanteil-2020.xlsb",
                                sheet  = "Vertraege_Daten", range = "TA4:AMV218")

data_naa <- cbind(data_naa_a, data_naa)

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


