# Ein preporcessing-Skript, das sämtliche Datensätze für die Studiumsseite erstellt und lädt

# Juni 23
# kab

library(rio)
library(dplyr)
library(tidyr)
library(magrittr)
library(stringr)
library(readxl)



# Studierende2 ---------

    #-----------------------------------------------------------------------------#
    # Ein Datensatz für die Pies auf der Studiumsseite, der das Jahr 2021 ergänzt #
    #-----------------------------------------------------------------------------#

## Creating and Cleaning ----

setwd("C:/Users/kab/Downloads/datalab/datalab/data-raw/raw")

studi_alt <- import("Studierende_alt_13_01.xlsx")

createdata <- function(data, Jahr, Fachbereich) {

  # add headlines
  header <- c("region", "insgesamt_stud","insgesamt_1hs", "insgesamt_1fs", "uni_stud", "uni_1hs", "uni_1fs",
              "lehramt_stud","lehramt_1hs", "lehramt_1fs", "fh_stud", "fh_1hs" , "fh_1fs")
  colnames(data) <- header

  # create columns/variables

  ### Anzeige !!!Bei neuen Daten anpasen!!!
  data$anzeige_geschlecht <- "gesamt"
  data$index <- 1:nrow(data)
  data$anzeige_geschlecht[data$index >20] <- "frauen"
  # überflüssiges löschen:
  data <- data[-c(1,19, 20, 36,37),]
  data <- data %>%
    select(-index)

  # ins long-Format bringen
  data <- data %>%
    pivot_longer(cols = insgesamt_stud:fh_1fs, values_to = "wert")

  ### indikator, lehramt, hochschulform
  data <- data %>%
    mutate(indikator=case_when(
      str_detect(data$name, "stud") ~ "Studierende",
      str_detect(data$name, "1hs") ~ "Studienanfänger_1hs",
      str_detect(data$name, "1fs") ~ "Studienanfänger_1fs"),
      nur_lehramt=case_when(
        str_detect(data$name, "lehramt") ~ "Ja",
        T ~ "Nein"),
      hochschulform=case_when(
        str_detect(data$name, "insgesamt") ~ "insgesamt",
        str_detect(data$name, "fh_") ~ "FH",
        T ~ "Uni")
    )

  ### fachbereich
  data$fachbereich <- Fachbereich
  data$Jahr <- Jahr
  data$quelle <- "Statistisches Bundesamt (Destatis), 2021: Auf Anfrage"
  data$hinweise <- "Studienanfänger im ersten Hochschulsemester"

  ### bereich (für alle gleich)
  data$bereich <- "Hochschule"

  # überflüssige Spalte "name" entfernen
  data <- data %>%
    select(-name)

  return(data)

}

stu2021_alleFG <- read_excel("DES064_bmbfstu1_2021.xlsx", sheet = "Insgesamt", col_names = F)[c(14:33,36:52), c(1:4,5:7, 11:13, 14:16)]
stu2021_Mathe <-  read_excel("DES064_bmbfstu1_2021.xlsx", sheet = "Mathe", col_names = F)[c(14:33,36:52), c(1:4,5:7, 11:13, 14:16)]
stu2021_Ing <-    read_excel("DES064_bmbfstu1_2021.xlsx", sheet = "Ingenieur", col_names = F)[c(14:33,36:52), c(1:4,5:7, 11:13, 14:16)]

stu2021_alleFG <- createdata(stu2021_alleFG, "2021", "Alle")
stu2021_Mathe <- createdata(stu2021_Mathe, "2021", "Mathe")
stu2021_Ing <- createdata(stu2021_Ing, "2021", "Ingenieur")

data_studi_neu <- bind_rows(stu2021_Ing,stu2021_Mathe,stu2021_alleFG
                            , studi_alt
)

data_studi_neu$indikator <- gsub("Studienanfänger", "Studienanfänger:innen", data_studi_neu$indikator)

data_studi_neu <- data_studi_neu %>%
  # mutate(indikator=case_when(
  #   .$indikator == "Studienanfänger:innen:innen"~ "Studienanfänger:innen",
  #   T ~ .$indikator
  # ))%>%
  mutate(indikator= case_when(
    str_detect(.$nur_lehramt, "Ja") ~ paste0(.$indikator, "_lehramt"),
    T ~ .$indikator
  )) %>%
  select(-nur_lehramt)




data_studi_neu$fachbereich <- gsub("Ingenieur", "Ingenieurwissenschaften", data_studi_neu$fachbereich)
data_studi_neu$fachbereich <- gsub("Mathe", "Mathematik_Naturwissenschaften", data_studi_neu$fachbereich)
data_studi_neu$region <- gsub("	Deutschland ...", "Deutschland", data_studi_neu$region)

colnames(data_studi_neu)[2] <- "geschlecht"



data_studi_neu1 <- data_studi_neu %>%
  mutate(indikator = case_when(hochschulform == "Uni" ~ paste0(.$indikator, "_Uni"),
                               hochschulform == "FH" ~ paste0(.$indikator, "_FH"),
                               T ~ .$indikator))%>%
  mutate(
    label= case_when(.$indikator == "Studienanfänger:innen_1fs" ~ "Studienanfänger:innen (1.Fachsemester)"
                     ,.$indikator == "Studienanfänger:innen_1fs_FH" ~ "Studienanfänger:innen (Fachhochschulen, 1.Fachsemester)"
                     ,.$indikator == "Studienanfänger:innen_1fs_Uni" ~ "Studienanfänger:innen (Universität, 1.Fachsemester)",


                     .$indikator == "Studienanfänger:innen_1fs_lehramt_Uni" ~ "Studienanfänger:innen (Lehramt, Universität, 1.Fachsemester)",

                     .$indikator == "Studienanfänger:innen_1hs" ~ "Studienanfänger:innen (1.Hochschulsemester)",
                     .$indikator == "Studienanfänger:innen_1hs_FH" ~ "Studienanfänger:innen (Fachhochschulen, 1.Hochschulsemester)",
                     .$indikator == "Studienanfänger:innen_1hs_lehramt_Uni" ~ "Studienanfänger:innen (Lehramt, Universität, 1.Hochschulsemester)",

                     .$indikator == "Studienanfänger:innen_1hs_Uni" ~ "Studienanfänger:innen (Universität, 1.Hochschulsemester)",
                     .$indikator == "Studierende" ~ "Studierende",
                     .$indikator == "Studierende_FH" ~ "Studierende (Fachhochschulen)",
                     .$indikator == "Studierende_lehramt_Uni" ~ "Studierende (Lehramt, Universität)",
                     .$indikator == "Studierende_Uni" ~ "Studierende (Universität)",
    ))

# Creating new Subjects

data_studi_neu2 <- data_studi_neu1 %>%
  select(-indikator, -hochschulform)%>%
  rename(indikator = label)%>%
  pivot_wider(names_from = fachbereich, values_from = wert)%>%
  mutate(across(c("Ingenieurwissenschaften", "Mathematik_Naturwissenschaften", "Alle" ), as.numeric))%>%
  mutate("MINT"= Ingenieurwissenschaften+Mathematik_Naturwissenschaften)%>%
  mutate("Nicht MINT"= Alle - `MINT`)%>%
  rename("Mathematik, Naturwissenschaften" = Mathematik_Naturwissenschaften)%>%
  pivot_longer(c("Ingenieurwissenschaften", "Mathematik, Naturwissenschaften", "Alle", "Nicht MINT", "MINT" ), values_to = "wert", names_to = "fachbereich")

# Creating missing regions
data_studi_neu3 <- data_studi_neu2 %>%
  pivot_wider(names_from = region, values_from = wert)%>%
  mutate("Ostdeutschland (inkl. Berlin)" = rowSums(select(., c("Berlin", "Thüringen", "Sachsen", "Sachsen-Anhalt", "Brandenburg", "Mecklenburg-Vorpommern") ), na.rm =T ))%>%
  rename(Deutschland = `Deutschland ...`)%>%
  mutate("Westdeutschland (o. Berlin)"= Deutschland - `Ostdeutschland (inkl. Berlin)`)%>%
  pivot_longer(c(8:ncol(.)), values_to = "wert", names_to = "region")

# Creating males
data_studi_neu4 <- data_studi_neu3 %>%
  pivot_wider(names_from = geschlecht, values_from=wert)%>%
  mutate(männer = gesamt - frauen)%>%
  rename(Männer = männer, Frauen = frauen, Gesamt = gesamt, jahr = Jahr)%>%
  pivot_longer(c("Gesamt", "Männer", "Frauen"), values_to= "wert", names_to = "geschlecht")


data_studi_neu1$hinweise <- NA





# export(data_studi_neu1, "output/Studierende_2023.xlsx")
studierende_neu <-data_studi_neu4


## Export
setwd("C:/Users/kab/Downloads/datalab/datalab/data-raw")

usethis::use_data(Studierende, overwrite = T)


# Studierende_feacher ----

  #-----------------------------------------------------------------------------------------------------------------#
  # Ein Datensatz, der nur die Datensätze für mit mit Fächerunterscheidung beinhaltet für alle Indikatoren          #
  #-----------------------------------------------------------------------------------------------------------------#

## Creating and Cleaning ----

setwd("C:/Users/kab/Downloads/datalab/datalab/data-raw/raw")


# Funktion zur Extrahierung von Rohdatensätzen

clean_des <- function (dat,year){

  raw <- read_excel(dat, col_types = "text")

  raw <- raw[-c(1:6),-c(1,3,5)]

  colnames(raw) <- c("region", "fachgruppe", "fach", "gesamt", "weiblich",
                     "auslaender", "lehramt", "lehramt_weiblich",
                     "gesamt_1hs", "weiblich_1hs", "auslaender_1hs", "gesamt_1fs",
                     "weiblich_1fs")

  raw$jahr <- year
  raw$bereich <- "hochschule"
  raw$hinweise <- NA
  raw$quelle <- "Statistisches Bundesamt (Destatis), 2022: Auf Anfrage"

  return(raw)

}

# Creating master

raw2018 <- clean_des(dat= "C:\\Users\\kab\\Downloads\\datalab\\datalab\\data-raw\\raw\\DES060_Kroeger_Stud_Land_FG_STB_2018.xlsx", year="2018")
raw2019 <- clean_des(dat= "C:\\Users\\kab\\Downloads\\datalab\\datalab\\data-raw\\raw\\DES061_Kroeger_Stud_Land_FG_STB_2019.xlsx", year="2019")
raw2020 <- clean_des(dat= "C:\\Users\\kab\\Downloads\\datalab\\datalab\\data-raw\\raw\\DES062_Kroeger_Stud_Land_FG_STB_2020.xlsx", year="2020")
raw2021 <- clean_des(dat= "C:\\Users\\kab\\Downloads\\datalab\\datalab\\data-raw\\raw\\DES063_Kroeger_Stud_Land_FG_STB_2021.xlsx", year="2021")

master <- bind_rows(raw2018,raw2019,raw2020, raw2021)



# Cleaning

## Filtering relevant subjects

master_natwi_ing_unique <- master %>%  filter(fachgruppe =="Ingenieurwissenschaften" |
                                                fachgruppe=="Mathematik, Naturwissenschaften")%>%
  distinct(fach)%>%
  as.vector()%>%
  unlist()


master_tada <- master  %>%
  filter(fach %in% master_natwi_ing_unique)

master_tada_long <- master_tada %>%
  pivot_longer(c(4:13), names_to = "indikator", values_to = "wert")%>%
  unite(dummy,2:3, sep="_")%>%
  pivot_wider(names_from = dummy, values_from = wert)


## changing allgemein to weitere
colnames(master_tada_long)[10] <- "Mathematik, Naturwissenschaften_Weitere naturwissenschaftliche und mathematische Fächer"
colnames(master_tada_long)[21] <- "Ingenieurwissenschaft_Weitere ingenieurwissenschaftliche Fächer"

## Turning character columns into numerics
master_tada_long[,c(7:36)] <- purrr::map_dfr(.x = master_tada_long[,c(7:36)], .f =as.numeric )

## Creating aggregates and missing regions
master_tada_long_1 <- master_tada_long %>%
  mutate(`Mathematik, Naturwissenschaften_Geowissenschaften und Geographie` =
           `Mathematik, Naturwissenschaften_Geowissenschaften (ohne Geographie)`+
           `Mathematik, Naturwissenschaften_Geographie`,
         `Mathematik, Naturwissenschaften_Naturwissenschaften` =
           `Mathematik, Naturwissenschaften_Physik, Astronomie`+
           `Mathematik, Naturwissenschaften_Chemie`+
           `Mathematik, Naturwissenschaften_Biologie`,
         `Ingenieurwissenschaften_Ingenieurwissenschaften ohne Informatik`=
           `Ingenieurwissenschaften_Zusammen`-
           `Ingenieurwissenschaften_Informatik`,
         MINT_MINT = `Ingenieurwissenschaften_Zusammen` +
           `Mathematik, Naturwissenschaften_Zusammen`)%>%
  mutate("Nicht MINT_Nicht MINT" = Zusammen_Zusammen - MINT_MINT)%>%
  select(-`Mathematik, Naturwissenschaften_Geowissenschaften (ohne Geographie)`,
         -`Mathematik, Naturwissenschaften_Geographie`)%>%
  pivot_longer(c(7:39), names_to = "dummy", values_to ="wert")%>%
  pivot_wider(names_from=region, values_from = wert)%>%
  mutate(Deutschland = rowSums(dplyr::select(., c(7:ncol(.))),na.rm = T))%>%
  mutate("Ostdeutschland (inkl. Berlin)"= rowSums(select(., c("Thüringen", "Berlin", "Mecklenburg-Vorpommern", "Sachsen", "Sachsen-Anhalt", "Brandenburg")),na.rm = T))%>%
  mutate("Westdeutschland (o. Berlin)"=Deutschland - `Ostdeutschland (inkl. Berlin)`)%>%
  pivot_longer(c(7:ncol(.)), names_to = "region", values_to = "wert")%>%
  separate(dummy, c("fachbereich", "fach"), sep="_")

master_faecher_output <- master_tada_long_1 %>%
  select(bereich, indikator, region, jahr, fachbereich, fach, wert, quelle, hinweise)


master_faecher_output$fach <- gsub("Zusammen", "Alle Fächer", master_faecher_output$fach )
master_faecher_output$fach <- gsub("MINT", "Alle MINT-Fächer", master_faecher_output$fach )
master_faecher_output$fach <- gsub("Nicht Alle MINT-Fächer", "Alle Nicht MINT-Fächer", master_faecher_output$fach )

master_faecher_output <- master_faecher_output %>%
  mutate(
    geschlecht = case_when(
      str_detect(.$indikator, "weiblich") ~ "Frauen",
      T ~ "Gesamt"),
    dummy_indi=case_when(
      str_detect(.$indikator, "1hs")~ "Studienanfänger:innen_1hs",
      str_detect(.$indikator, "1fs")~ "Studienanfänger:innen_1fs",
      T~ "Studierende")
    # ,
    # lehramt = case_when(
    #   str_detect(.$indikator, "lehramt")~ "Ja",
    #   T ~ "Nein")
  )

master_faecher_output$dummy_indi <- ifelse(grepl("lehramt", master_faecher_output$indikator),
                                           paste0(master_faecher_output$dummy_indi, "_Lehramt"),
                                           master_faecher_output$dummy_indi)

master_faecher_output$dummy_indi <- ifelse(grepl("auslaender", master_faecher_output$indikator),
                                           paste0(master_faecher_output$dummy_indi, "_Ausländisch"),
                                           master_faecher_output$dummy_indi)


master_faecher_output1 <- master_faecher_output %>%
  select(bereich, dummy_indi,  region, jahr, fachbereich, fach, geschlecht, wert, quelle, hinweise
  )%>%
  rename(indikator=dummy_indi)%>%
  mutate(indikator=case_when(
    indikator=="Studienanfänger:innen_1fs" ~ "Studienanfänger:innen (1. Fachsemester)",
    indikator=="Studienanfänger:innen_1hs" ~ "Studienanfänger:innen (1. Hochschulsemester)",
    indikator=="Studienanfänger:innen_1hs_Ausländisch"~ "Internationale Studienanfänger:innen (1. Hochschulsemester)",
    indikator=="Studierende" ~ "Studierende",
    indikator=="Studierende_Ausländisch" ~ "Internationale Studierende",
    indikator=="Studierende_Lehramt" ~ "Studierende (Lehramt)"
  ))%>%
  select(- hinweise, -quelle)%>%
  mutate(fachbereich =case_when (
    fachbereich== "Ingenieurwissenschaft"~ "Ingenieurwissenschaften",
    T~.$fachbereich
  ))%>%
  mutate(mint_select=case_when(
    fachbereich== "Mathematik, Naturwissenschaften" ~"MINT",
    fachbereich== "Ingenieurwissenschaften" ~ "MINT",
    T~ "Nicht MINT"
  ))%>%
  mutate(typ=case_when(fach %in% c("Alle Fächer", "Naturwissenschaften", "Ingenieurwissenschaften ohne Informatik",
                                   "Alle MINT-Fächer", "Alle Nicht MINT-Fächer")  ~ "Aggregat",
                       T ~ "Einzelauswahl"))

master_faecher_output1$fach <- ifelse(master_faecher_output1$fach == "Alle Fächer", master_faecher_output1$fachbereich, master_faecher_output1$fach)

master_faecher_output1$fach <- ifelse(master_faecher_output1$fach == "Zusammen", "Alle Fächer", master_faecher_output1$fach)

studierende_faecher2 <- master_faecher_output1 %>%
  filter(!fach %in% c("Weitere naturwissenschaftliche und mathematische Fächer",
                      "Außerhalb der Studienbereichsgliederung/Sonstige Fächer",
                      "Weitere ingenieurwissenschaftliche Fächer"))

studierende_faecher3 <- studierende_faecher2%>%
  pivot_wider(names_from = geschlecht, values_from =wert)%>%
  mutate(Männer= Gesamt - Frauen)%>%
  pivot_longer(c("Männer", "Frauen", "Gesamt"), names_to= "geschlecht", values_to = "wert")%>%
  mutate(fach=case_when(
    fach == "Ingenieurwissenschaften" ~ "Ingenieurwissenschaften (inkl. Informatik)",
    T~ .$fach
  ))



## Export

# export(master_faecher_output2, "data_raw/studierende_faecher_08_06_23.xlsx")

setwd("C:/Users/kab/Downloads/datalab/datalab/data-raw")

usethis::use_data(Studierende_detailliert, overwrite = T)

## Preprocessing ---

# studierende_faecher <-
#   readxl::read_xlsx(
#     system.file(package = "datalab", "data-raw/studierende_faecher.xlsx")
#   ) %>%
#   janitor::clean_names() %>%
#   janitor::remove_empty() %>%
#   dplyr::rename(anzeige_geschlecht = geschlecht) %>%
#   dplyr::filter(indikator == "Studierende",
#                 !(fachbereich %in% c("MINT", "Zusammen"))
#   ) %>%
#   dplyr::select(-c("label", "bereich")) %>%
#   dplyr::mutate(fachbereich = dplyr::case_when(fachbereich == "Ingenieurwissenschaft" ~ "Ingenieurwissenschaften",
#                                                TRUE ~ fachbereich)) %>%
#   dplyr::mutate(fach = dplyr::case_when(fachbereich %in% c("Mathematik, Naturwissenschaften", "Ingenieurwissenschaften") ~ fach,
#                                         TRUE ~ fachbereich)) %>%
#   dplyr::filter(!(fach %in% c("Alle Fächer", "Alle MINT-Fächer")))





# Studierende_alle_indi----

# !!!!!!Redundant!!!!!!!!

  # Ein Datensatz, der nur die Datensätze für mit mit Fächerunterscheidung beinhaltetfür alle Inidkatoren (nicht nur Studierende)
# wd <- getwd()
# setwd(wd)
# studierende_faecher_alle_indi <- readxl::read_xlsx("studierende_faecher.xlsx")
#
# studierende_faecher_alle_indi <- studierende_faecher_alle_indi %>%
#   janitor::clean_names() %>%
#   janitor::remove_empty() %>%
#   dplyr::rename(anzeige_geschlecht = geschlecht) %>%
#   dplyr::filter(                !(fachbereich %in% c("MINT", "Zusammen"))
#   ) %>%
#   dplyr::select(-c("indikator", "bereich")) %>%
#   dplyr::mutate(fachbereich = dplyr::case_when(fachbereich == "Ingenieurwissenschaft" ~ "Ingenieurwissenschaften",
#                                                TRUE ~ fachbereich)) %>%
#   dplyr::mutate(fach = dplyr::case_when(fachbereich %in% c("Mathematik, Naturwissenschaften", "Ingenieurwissenschaften") ~ fach,
#                                         TRUE ~ fachbereich)) %>%
#   dplyr::filter(!(fach %in% c("Alle Fächer", "Alle MINT-Fächer")))
#
#
# # create region Deutschland
# deutschland_gesamt <- studierende_faecher %>% dplyr::group_by(indikator,
#                                                               jahr,
#                                                               fachbereich,
#                                                               fach,
#                                                               anzeige_geschlecht) %>%
#   dplyr::summarise(wert = sum(wert, na.rm = T)) %>%
#   dplyr::ungroup() %>%
#   dplyr::mutate(region = "Deutschland")
#
# # create region Osten
# osten <- studierende_faecher %>%
#   dplyr::filter(region %in% c("Brandenburg",
#                               "Mecklenburg-Vorpommern",
#                               "Sachsen",
#                               "Sachsen-Anhalt",
#                               "Thüringen",
#                               "Berlin")) %>%
#   dplyr::group_by(indikator,
#                   jahr,
#                   fachbereich,
#                   fach,
#                   anzeige_geschlecht) %>%
#   dplyr::summarise(wert = sum(wert, na.rm = T)) %>%
#   dplyr::ungroup() %>%
#   dplyr::mutate(region = "Osten")
#
# # create region Westen
# westen <- studierende_faecher %>%
#   dplyr::filter(!(region %in% c("Brandenburg",
#                                 "Mecklenburg-Vorpommern",
#                                 "Sachsen",
#                                 "Sachsen-Anhalt",
#                                 "Thüringen",
#                                 "Berlin"))) %>%
#   dplyr::group_by(indikator,
#                   jahr,
#                   fachbereich,
#                   fach,
#                   anzeige_geschlecht) %>%
#   dplyr::summarise(wert = sum(wert, na.rm = T)) %>%
#   dplyr::ungroup() %>%
#   dplyr::mutate(region = "Westen")
#
#
# studierende_faecher <- dplyr::bind_rows(studierende_faecher, deutschland_gesamt, westen, osten)
#
# usethis::use_data(studierende_faecher_alle_indi, overwrite = T)
#
# # Studierende ----

# !!!!!!!!! Redundant!!!!!!!
#
# # Ein Legacy-Datansatz, der veraltet ist.
# studierende_read <-
#   readxl::read_xlsx(
#     system.file(package = "datalab", "data-raw/Studierende.xlsx")
#   ) %>%
#   janitor::clean_names() %>%
#   janitor::remove_empty()
#
# # remove dots from strings in column "region"
# studierende_read$region <- gsub("\\.", "", studierende_read$region, perl=TRUE)
#
# studierende_read$region <- gsub(' ', '', studierende_read$region)
#
# studierende_read <- studierende_read %>%
#   dplyr::mutate(wert = as.numeric(wert)) %>%
#   dplyr::mutate(anzeige_geschlecht = replace(anzeige_geschlecht,
#                                              anzeige_geschlecht == "frauen",
#                                              "Frauen"),
#                 anzeige_geschlecht = replace(anzeige_geschlecht,
#                                              anzeige_geschlecht == "gesamt",
#                                              "Gesamt"),
#                 fachbereich = replace(fachbereich,
#                                       fachbereich == "Mathe",
#                                       "Mathematik/Naturwissenschaften"),
#                 fachbereich = replace(fachbereich,
#                                       fachbereich == "Ingenieur",
#                                       "Ingenieurwissenschaften"))
#
# studierende <- studierende_read
#
# usethis::use_data(studierende, overwrite = T)

