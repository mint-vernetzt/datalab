
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

names(data_naa)[6] <- "geschlecht"
names(data_naa)[2] <- "fachbereich"
names(data_naa)[5] <- "wert"

data_naa[data_naa$geschlecht == "weiblich", "geschlecht"] <- "Frauen"
data_naa[data_naa$geschlecht == "Männer", "geschlecht"] <- "Männer"
data_naa[data_naa$geschlecht == "insgesamt", "geschlecht"] <- "Gesamt"

data_naa[data_naa$region == "Ost", "region"] <- "Osten"
data_naa[data_naa$region == "West", "region"] <- "Westen"

# sort data_naa
data_naa <- data_naa[with(data_naa, order(fachbereich, region, jahr)), ]

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
data_naa <- data_naa %>% subset(select = -c(code, Bezeichnung))

# clean column with job titles
# rename column
data_naa <- data_naa %>% dplyr::rename(ebene = "Ebene",
                                       fachrichtung = "Bezeichnung BIBB modifiziert")
# remove row with sum over all jobs
data_naa <- data_naa %>% dplyr::filter(!grepl('Referenzzeile', fachrichtung)) %>%
  # remove numbers from job title
  dplyr::mutate(fachrichtung = gsub('[[:digit:]]+', '', fachrichtung),
                # remove white space
                fachrichtung = gsub(' ', '', fachrichtung)) %>%
  # remove all column which provide information about the "Frauenanteil"
  dplyr::select(-contains("anteil"))

# reshape data_naa in long format
data_naa <- data_naa %>% tidyr::gather(region, anzahl, -ebene, -fachrichtung)

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

names(data_naa)[6] <- "geschlecht"
names(data_naa)[2] <- "fachbereich"
names(data_naa)[5] <- "wert"

data_naa[data_naa$geschlecht == "weiblich", "geschlecht"] <- "Frauen"
data_naa[data_naa$geschlecht == "Männer", "geschlecht"] <- "Männer"
data_naa[data_naa$geschlecht == "insgesamt", "geschlecht"] <- "Gesamt"

data_naa[data_naa$region == "Ost", "region"] <- "Osten"
data_naa[data_naa$region == "West", "region"] <- "Westen"

# sort data_naa
data_naa <- data_naa[with(data_naa, order(fachbereich, region, jahr)), ]

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
data_naa <- data_naa %>% subset(select = -c(code, Bezeichnung))

# clean column with job titles
# rename column
data_naa <- data_naa %>% dplyr::rename(ebene = "Ebene",
                                       fachrichtung = "Bezeichnung.BIBB.modifiziert")
# remove row with sum over all jobs
data_naa <- data_naa %>% dplyr::filter(!grepl('Referenzzeile', fachrichtung)) %>%
  # remove numbers from job title
  dplyr::mutate(fachrichtung = gsub('[[:digit:]]+', '', fachrichtung),
                # remove white space
                fachrichtung = gsub(' ', '', fachrichtung)) %>%
  # remove all column which provide information about the "Frauenanteil"
  dplyr::select(-contains("anteil"))

# reshape data_naa in long format
data_naa <- data_naa %>% tidyr::gather(region, anzahl, -ebene, -fachrichtung)

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

names(data_naa)[6] <- "geschlecht"
names(data_naa)[2] <- "fachbereich"
names(data_naa)[5] <- "wert"

data_naa[data_naa$geschlecht == "weiblich", "geschlecht"] <- "Frauen"
data_naa[data_naa$geschlecht == "Männer", "geschlecht"] <- "Männer"
data_naa[data_naa$geschlecht == "insgesamt", "geschlecht"] <- "Gesamt"

data_naa[data_naa$region == "Ost", "region"] <- "Osten"
data_naa[data_naa$region == "West", "region"] <- "Westen"

# sort data_naa
data_naa <- data_naa[with(data_naa, order(fachbereich, region, jahr)), ]

rm(data_naa_a, data_naa_insgesamt, data_naa_maennlich, data_naa_weiblich)

data_naa_20 <- data_naa


# Jahre kombinieren
data_naa <- rbind(data_naa_17, data_naa_20, data_naa_22)

# Ebene 1 = Fachbereiche (vgl. Arbeitsmarkt_detailliert: MINT, Inform., Mathe/Nawi, Technik + Unterformen)
# Ebene 2 = Berufsgruppen
# Ebene 3 = Berufe

#alt, würde Fachbereiche Filtern, aber wollen für top 10 der Berufe ja Berufs-Ebene
# data_naa <- data_naa %>% dplyr::filter(ebene == "Ebene 1")

#Berufsebene ausgewählt
data_naa <- data_naa %>% dplyr::filter(ebene == "Ebene 3")

# für shinyapp:

usethis::use_data(data_naa, overwrite = T)


