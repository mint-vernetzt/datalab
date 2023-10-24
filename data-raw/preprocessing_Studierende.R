# kab
# Jan 2023

library(rio)
library(dplyr)
library(tidyr)
library(magrittr)
library(stringr)
library(readxl)
library(janitor)
library(purrr)
library(readr)
library(countrycode)


# hier pathen

akro <- "kbr"
pfad <- paste0("C:/Users/", akro,
               "/OneDrive - Stifterverband/MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/02 Datenmaterial/01_Rohdaten/02_Alle Daten/")

path_kek <- "C:/Users/kab/OneDrive - Stifterverband/AP7 MINT-DataLab/02 Datenmaterial/01_Rohdaten/02_Alle Daten/"

pfad <- path_kek

# Studierende Domestisch ----

## Studierende ----


#setwd("C:/Users/kab/Downloads/datalab/datalab/data-raw/raw")

get_data <- function(file_list, fach_list){

  test<-  read_excel( file_list, sheet =fach_list, col_names = F)%>%
    data.table::transpose()

  jahreszahl1 <-str_extract_all(test$V1, "\\d+")[[1]]

  jahreszahl <- jahreszahl1[2]

  # irrelevante cols löschen und cleaning
  test1 <- test %>%
    janitor::remove_empty(., "cols")

  test1[test1=="Deutschland ..."] <- "Deutschland"


  # weiblich suffix an bls anfügen
  weiblich_cindex <- which(str_detect(test1[,everything(test1)], "Weiblich"))

  weiblich_rindex <- which(test1[,weiblich_cindex]=="Weiblich")

  test1[weiblich_rindex, weiblich_cindex:ncol(test1)] <- paste0(test1[weiblich_rindex, weiblich_cindex:ncol(test1)], "_weiblich")


  # insgesamt suffix an bls anfügen
  insgesamt_cindex <- which(str_detect(test1[,everything(test1)], "Insgesamt"))

  insgesamt_rindex <- which(test1[,insgesamt_cindex]=="Insgesamt")

  test1[insgesamt_rindex, insgesamt_cindex:(weiblich_cindex-1)] <- paste0(test1[insgesamt_rindex, insgesamt_cindex:ncol(test1)], "_insgesamt")


  # indikator variable erstellen und colnames vorbereiten
  test2 <- test1%>%
    mutate(Indikator=case_when(
      #if_any(everything(),~ str_detect(., "^Land$")) ~ "indikator",
      if_any(everything(),~ str_detect(., "^Studierende$")) ~ "Insgesamt",
      if_any(everything(),~ str_detect(., "^einschl. Lehramtsstudierende$")) ~ "Universität",
      if_any(everything(),~ str_detect(., "^Lehramtsstudierende$")) ~ "Lehramt",
      if_any(everything(),~ str_detect(., "^Fachhochschulen$")) ~ "Fachhochschulen"))%>%
    mutate(indi_counter=cumsum(!is.na(Indikator)))%>%
    group_by(indi_counter)%>%
    mutate(ID = row_number() - 1)%>%
    mutate(Indikator = ifelse(ID <= 2, first(Indikator), NA_character_))%>%
    ungroup()%>%
    mutate(Indikator=case_when(
      if_any(everything(),~ str_detect(., "^Land$")) ~ "indikator",
      T ~ .$Indikator
    ))

  # unnötige Rows raus
  test3 <- test2 %>%
    filter(Indikator!="NA")

  # richtige labels
  test4 <- test3 %>%
    mutate(hochschulform = case_when(
      Indikator=="indikator" ~ "hochschulform",
      Indikator=="Fachhochschulen" ~ "Fachhochschulen",
      Indikator=="Insgesamt" ~ NA,
      T ~ "Universität"
    ))%>%
    mutate(status = case_when(
      Indikator== "indikator" ~ "status",
      if_any(everything(), ~ str_detect(., "^1. HS$|^1. FS$"))~ "Studienanfänger:innen",
      if_any(everything(), ~ str_detect(., "^St$"))~ "Studierende",
    ))%>%
    mutate(lehramt = case_when(
      Indikator=="indikator" ~ "lehramt",
      Indikator=="Lehramt" ~ "ja",
      T~NA
    ))%>% mutate(semster =
                   case_when(Indikator == "indikator" ~ "semester",
                             if_any(everything(), ~ str_detect(., "^1. HS$"))~ "1. Hochschulsemester",
                             if_any(everything(), ~ str_detect(., "^1. FS$"))~ "1. Fachsemester",
                             T ~ NA))


  #richtige colnames und indikatoren
  test5 <- test4[,insgesamt_cindex:ncol(test4) ]

  test6 <- test5 %>%
    janitor::row_to_names(1)%>%
    select("Baden-Württemberg_insgesamt",
           "Bayern_insgesamt",
           "Berlin_insgesamt",
           "Brandenburg_insgesamt",
           "Bremen_insgesamt",
           "Hamburg_insgesamt",
           "Hessen_insgesamt",
           "Mecklenburg-Vorpommern_insgesamt",
           "Niedersachsen_insgesamt",
           "Nordrhein-Westfalen_insgesamt",
           "Rheinland-Pfalz_insgesamt",
           "Saarland_insgesamt",
           "Sachsen_insgesamt",
           "Sachsen-Anhalt_insgesamt" ,
           "Schleswig-Holstein_insgesamt",
           "Thüringen_insgesamt" ,
           "Deutschland_insgesamt" ,
           "Baden-Württemberg_weiblich" ,
           "Bayern_weiblich",
           "Berlin_weiblich" ,
           "Brandenburg_weiblich",
           "Bremen_weiblich" ,
           "Hamburg_weiblich" ,
           "Hessen_weiblich" ,
           "Mecklenburg-Vorpommern_weiblich" ,
           "Niedersachsen_weiblich"  ,
           "Nordrhein-Westfalen_weiblich" ,
           "Rheinland-Pfalz_weiblich" ,
           "Saarland_weiblich",
           "Sachsen_weiblich" ,
           "Sachsen-Anhalt_weiblich",
           "Schleswig-Holstein_weiblich",
           "Thüringen_weiblich" ,
           "Deutschland_weiblich",
           "hochschulform" ,
           "status" ,
           "lehramt" ,
           "semester" )

  # pivoting, clean indikatoren
  test7 <- test6 %>%
    pivot_longer(contains(c("weiblich", "insgesamt")), names_to = "platzhalter", values_to = "wert")%>%
    separate(platzhalter, c("region", "geschlecht"), sep = "_") %>%
    mutate(label = paste0 (.$status, .$lehramt, .$hochschulform, .$semester))%>%
    mutate(label =case_when(
      label == "Studienanfänger:innenjaUniversität1. Fachsemester" ~ "Studienanfänger:innen (Lehramt, Universität, 1.Fachsemester)",
      label == "Studienanfänger:innenjaUniversität1. Hochschulsemester" ~ "Studienanfänger:innen (Lehramt, Universität, 1.Hochschulsemester)",
      label == "Studienanfänger:innenNAFachhochschulen1. Fachsemester" ~ "Studienanfänger:innen (Fachhochschulen, 1.Fachsemester)",
      label == "Studienanfänger:innenNAFachhochschulen1. Hochschulsemester" ~ "Studienanfänger:innen (Fachhochschulen, 1.Hochschulsemester)",
      label == "Studienanfänger:innenNANA1. Fachsemester" ~ "Studienanfänger:innen (1.Fachsemester)",
      label == "Studienanfänger:innenNANA1. Hochschulsemester" ~ "Studienanfänger:innen (1.Hochschulsemester)",
      label == "Studienanfänger:innenNAUniversität1. Fachsemester" ~ "Studienanfänger:innen (Universität, 1.Fachsemester)",
      label == "Studienanfänger:innenNAUniversität1. Hochschulsemester" ~ "Studienanfänger:innen (Universität, 1.Hochschulsemester)",
      label == "StudierendejaUniversitätNA" ~ "Studierende (Lehramt, Universität)",
      label == "StudierendeNAFachhochschulenNA" ~ "Studierende (Fachhochschulen)",
      label == "StudierendeNANANA" ~ "Studierende",
      label == "StudierendeNAUniversitätNA" ~ "Studierende (Universität)"
    ))%>%
    select(region, wert, label, geschlecht)


  test8 <- test7 %>%
    pivot_wider(values_from =wert, names_from = geschlecht)%>%
    mutate(across(c("insgesamt", "weiblich"), ~ as.numeric(.)))%>%
    mutate(männlich = insgesamt- weiblich)%>%
    pivot_longer(c(männlich, weiblich, insgesamt), names_to = "geschlecht", values_to = "wert")

  test8$jahr <- jahreszahl
  test8$fach <- fach_list

  return(test8)

}

# Bei neuen Datensätzen, hier einfügen:
file_list <- rep(c(
                   paste0(pfad, "DES066_bmbfstu1_2022.xlsx"),
                   paste0(pfad, "DES064_bmbfstu1_2021.xlsx"),
                   paste0(pfad, "DES050_bmbfstu1_2020.xlsx"),
                   paste0(pfad, "DES049_bmbfstu1_2019.xlsx"),
                   paste0(pfad, "DES048_bmbfstu1_2018.xlsx"),
                   paste0(pfad, "DES047_bmbfstu1_2017.xlsx"),
                   paste0(pfad, "DES046_bmbfstu1_2016.xlsx"),
                   paste0(pfad, "DES045_bmbfstu1_2015.xlsx"),
                   paste0(pfad, "DES044_bmbfstu1_2014.xlsx"),
                   paste0(pfad, "DES043_bmbfstu1_2013.xlsx"),
                   paste0(pfad, "DES042_bmbfstu1_2012.xlsx"),
                   paste0(pfad, "DES041_bmbfstu1_2011.xlsx"),
                   paste0(pfad,  "DES040_bmbfstu1_2010.xlsx")),each=3)

# Hier je neuer hinzugefügter datei, counter um 1 erhöhen
fach_list<- rep(c("Insgesamt", "Mathe", "Ingenieur"), times= 13)


# Lese-Loop
k <- purrr::map2(.x = file_list, .y = fach_list, .f =get_data)

k1 <- purrr::list_rbind(k)


# letzte Anpassungen
k2 <- k1 %>%
  rename(fachbereich = fach, indikator = label) %>%
  mutate(fachbereich=case_when(
    fachbereich == "Ingenieur" ~ "Ingenieurwissenschaften",
    fachbereich == "Mathe" ~ "Mathematik_Naturwissenschaften",
    fachbereich == "Insgesamt" ~ "Alle"
  ))

k3 <- k2 %>%
  mutate(geschlecht = case_when(
    geschlecht =="männlich" ~ "Männer",
    geschlecht =="weiblich" ~ "Frauen",
    geschlecht =="insgesamt" ~ "Gesamt"
  ))

# Creating new Subjects
data_studi_neu2 <- data.frame(k3)%>%
  pivot_wider(names_from = fachbereich, values_from = wert)%>%
  mutate(across(c("Ingenieurwissenschaften", "Mathematik_Naturwissenschaften", "Alle" ), as.numeric))%>%
  mutate("MINT (Gesamt)"= Ingenieurwissenschaften+Mathematik_Naturwissenschaften)%>%
  mutate("Nicht MINT"= Alle - `MINT (Gesamt)`)%>%
  rename("Mathematik, Naturwissenschaften" = Mathematik_Naturwissenschaften)%>%
  pivot_longer(c("Ingenieurwissenschaften", "Mathematik, Naturwissenschaften", "Alle", "Nicht MINT", "MINT (Gesamt)" ), values_to = "wert", names_to = "fachbereich")

# Creating missing regions
studierende <- data_studi_neu2 %>%
  pivot_wider(names_from = region, values_from = wert)%>%
  mutate("Ostdeutschland (inkl. Berlin)" = rowSums(select(., c("Berlin", "Thüringen", "Sachsen", "Sachsen-Anhalt", "Brandenburg", "Mecklenburg-Vorpommern") ), na.rm =T ))%>%
  #rename(Deutschland = `Deutschland ...`)%>%
  mutate("Westdeutschland (o. Berlin)"= Deutschland - `Ostdeutschland (inkl. Berlin)`)%>%
  pivot_longer(c("Berlin",
                 "Thüringen",
                 "Sachsen",
                 "Sachsen-Anhalt",
                 "Brandenburg",
                 "Mecklenburg-Vorpommern",
                 "Ostdeutschland (inkl. Berlin)",
                 "Westdeutschland (o. Berlin)",
                 "Baden-Württemberg",
                 "Bayern",
                 "Bremen",
                 "Hamburg",
                 "Hessen",
                 "Niedersachsen",
                 "Nordrhein-Westfalen",
                 "Rheinland-Pfalz",
                 "Saarland",
                 "Schleswig-Holstein",
                 "Deutschland"), values_to = "wert", names_to = "region")




usethis::use_data(studierende, overwrite = T)


duplika <- janitor::get_dupes(studierende, c(region, indikator, geschlecht, jahr, region, fachbereich))

## Studierende detailliert ----


#-----------------------------------------------------------------------------------------------------------------#
# Ein Datensatz, der nur die Datensätze für mit mit Fächerunterscheidung beinhaltet für alle Indikatoren          #
#-----------------------------------------------------------------------------------------------------------------#

### Creating and Cleaning ----

#setwd("C:/Users/kab/Downloads/datalab/datalab/data-raw/raw")
akro <- "kbr"
pfad <- paste0("C:/Users/", akro,
               "/OneDrive - Stifterverband/MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/02 Datenmaterial/01_Rohdaten/02_Alle Daten/")

pfad <- path_kek
# Funktion zur Extrahierung von Rohdatensätzen

clean_des <- function (dat,year){

  raw <- readxl::read_excel(dat, col_types = "text")

# Indexe prüfen!
  raw <- raw[-c(1:6),-c(1,3,5)]

  colnames(raw) <- c("region", "fachgruppe", "fach", "gesamt", "weiblich",
                     "auslaender", "lehramt", "lehramt_weiblich",
                     "gesamt_1hs", "weiblich_1hs", "auslaender_1hs", "gesamt_1fs",
                     "weiblich_1fs")

  raw$jahr <- year
  raw$bereich <- "hochschule"
  raw$hinweise <- NA
  raw$quelle <- ifelse(raw$jahr == 2022, "Statistisches Bundesamt (Destatis), 2023: Auf Anfrage", "Statistisches Bundesamt (Destatis), 2022: Auf Anfrage")

  return(raw)

}

# Creating master

raw2018 <- clean_des(dat= paste0(pfad, "DES060_Kroeger_Stud_Land_FG_STB_2018.xlsx"), year="2018")
raw2019 <- clean_des(dat= paste0(pfad, "DES061_Kroeger_Stud_Land_FG_STB_2019.xlsx"), year="2019")
raw2020 <- clean_des(dat= paste0(pfad, "DES062_Kroeger_Stud_Land_FG_STB_2020.xlsx"), year="2020")
raw2021 <- clean_des(dat= paste0(pfad, "DES063_Kroeger_Stud_Land_FG_STB_2021.xlsx"), year="2021")
raw2022 <- clean_des(dat= paste0(pfad, "DES065_Brunner_Stud_Land_FG_STB_2022.xlsx"), year="2022")

master <- dplyr::bind_rows(raw2018,raw2019,raw2020, raw2021, raw2022)


# Cleaning
library(dplyr)
library(tidyr)
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
         `Ingenieurwissenschaften_Ingenieurwissenschaften ohne Informatik`=
           `Ingenieurwissenschaften_Zusammen`-
           `Ingenieurwissenschaften_Informatik`,
         MINT_MINT = `Ingenieurwissenschaften_Zusammen` +
           `Mathematik, Naturwissenschaften_Zusammen`)%>%
  mutate(`Mathematik, Naturwissenschaften_Naturwissenschaften` =
           `Mathematik, Naturwissenschaften_Physik, Astronomie`+
           `Mathematik, Naturwissenschaften_Chemie`+
           `Mathematik, Naturwissenschaften_Biologie`+
           `Mathematik, Naturwissenschaften_Geowissenschaften und Geographie`+
           `Mathematik, Naturwissenschaften_Pharmazie`)%>%
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
      stringr::str_detect(.$indikator, "weiblich") ~ "Frauen",
      T ~ "Gesamt"),
    dummy_indi=case_when(
      stringr::str_detect(.$indikator, "1hs")~ "Studienanfänger:innen_1hs",
      stringr::str_detect(.$indikator, "1fs")~ "Studienanfänger:innen_1fs",
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
  #warum nicht "Alle MINT-Fächer"?
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

studierende_detailliert <- studierende_faecher2%>%
  pivot_wider(names_from = geschlecht, values_from =wert)%>%
  mutate(Männer= Gesamt - Frauen)%>%
  pivot_longer(c("Männer", "Frauen", "Gesamt"), names_to= "geschlecht", values_to = "wert")%>%
  mutate(fach=case_when(
    fach == "Ingenieurwissenschaften" ~ "Ingenieurwissenschaften (inkl. Informatik)",
    T~ .$fach
  ))



## Export

 # rio::export(studierende_detailliert, paste0(pfad, "studierende_detailliert.xlsx"))

# setwd("C:/Users/kab/Downloads/datalab/datalab/data-raw")

usethis::use_data(studierende_detailliert, overwrite = T)


# Studierende Int'l. ----

## EUROSTAT - Tertiatry Education Data (Anteil Studi nach Fach nach Gender) ----
# akro <- "kbr"
# file_path <- paste0("C:/Users/", akro,
#                "/OneDrive - Stifterverband/MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/02 Datenmaterial/01_Rohdaten/02_Alle Daten/")
pfad <- path_kek
# file_path <- paste0("C:/Users/", akro, "/OneDrive - Stifterverband/AP7 MINT-DataLab/02 Datenmaterial/01_Rohdaten/02_Alle Daten")

dat <- readr::read_csv(paste0(pfad, "EUROSTAT001_custom_Studi_Fach_Gender_original.csv.gz"))

dat1 <- dat %>%
  dplyr::select("iscedf13", "sex", "geo",
         "TIME_PERIOD", "OBS_VALUE")%>%
  dplyr::rename(fach = iscedf13, geschlecht = sex, land = geo, jahr= TIME_PERIOD, wert = OBS_VALUE)%>%
  dplyr::mutate(indikator = "Studierende",
         typ= "In Prozent")

dat_dupes<- dat1 %>%
  janitor::get_dupes()

dat1$land <- countrycode::countrycode(dat1$land, origin = "eurostat", destination="country.name.de", custom_match = c("EU28" = "EU (28)", "EU27_2020" = "EU (27), seit 2020"))

dat2 <- dat1 %>%
  dplyr::mutate(geschlecht= dplyr::case_when(
                   geschlecht == "F" ~ "Frauen",
                   geschlecht == "M" ~ "Männer",
                   T ~ "Gesamt"))%>%
  dplyr::mutate(fachbereich = dplyr::case_when(
                                 stringr::str_ends("F00", .$fach) ~ "Allgemeine Bildungsgänge und Qualifikationen",
                                 stringr::str_ends("F01", .$fach) ~ "Pädagogik",
                                 stringr::str_ends("F02", .$fach) ~ "Geisteswissenschaften und Künste",
                                 stringr::str_ends("F03", .$fach) ~ "Sozialwissenschaften, Journalismus und Informationswesen",
                                 stringr::str_ends("F04", .$fach) ~ "Wirtschaft, Verwaltung und Recht",
                                 stringr::str_ends("F05", .$fach) ~ "Naturwissenschaften, Mathematik und Statistik",
                                 stringr::str_ends("F06", .$fach) ~ "Informatik & Kommunikationstechnologie",
                                 stringr::str_ends("F07", .$fach) ~ "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                                 stringr::str_ends("F08", .$fach) ~ "Landwirtschaft, Forstwirtschaft, Fischerei und Tiermedizin",
                                 stringr::str_ends("F09", .$fach) ~ "Gesundheit, Medizin und Sozialwesen",
                                 stringr::str_ends("F10", .$fach) ~ "Dienstleistungen",
                                 stringr::str_detect("Total", .$fach) ~ "Insgesamt",
                                 stringr::str_ends("UNK", .$fach) ~ "Unbekannt"))%>%
  dplyr::mutate(fach = dplyr::case_when(
    stringr::str_ends("F00", .$fach)~ "Allgemeine Bildungsgänge und Qualifikationen",
    stringr::str_ends("F01", .$fach)~ "Pädagogik",
    stringr::str_ends("F02", .$fach) ~ "Geisteswissenschaften und Künste",
    stringr::str_ends("F03", .$fach) ~ "Sozialwissenschaften, Journalismus und Informationswesen",
    stringr::str_ends("F04", .$fach) ~ "Wirtschaft, Verwaltung und Recht",
    stringr::str_ends("F05", .$fach) ~ "Naturwissenschaften, Mathematik und Statistik",
    stringr::str_ends("F06", .$fach) ~ "Informatik & Kommunikationstechnologie",
    stringr::str_ends("F07", .$fach) ~ "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
    stringr::str_ends("F08", .$fach) ~ "Landwirtschaft, Forstwirtschaft, Fischerei und Tiermedizin",
    stringr::str_ends("F09", .$fach) ~ "Gesundheit, Medizin und Sozialwesen",
    stringr::str_ends("F10", .$fach) ~ "Dienstleistungen",
    stringr::str_ends("F050", .$fach) ~ "Naturwissenschaften, Mathematik und Statistik nicht näher definiert",
    stringr::str_ends("F051", .$fach) ~ "Biologie und verwandte Wissenschaften",
    stringr::str_ends("F052", .$fach) ~ "Umwelt",
    stringr::str_ends("F053", .$fach) ~ "Exakte Naturwissenschaften",
    stringr::str_ends("F054", .$fach) ~ "Mathematik und Statistik",
    stringr::str_ends("F058", .$fach) ~ "Interdisziplinäre Programme und Qualifikationen mit dem Schwerpunkt Naturwissenschaften,
Mathematik und Statistik",
    stringr::str_ends("F059", .$fach) ~ "Naturwissenschaften, Mathematik und Statistik nicht andernorts klassifiziert",
    stringr::str_ends("F070", .$fach) ~ "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe nicht näher definiert",
    stringr::str_ends("F071", .$fach) ~ "Ingenieurwesen und Technische Berufe",
    stringr::str_ends("F072", .$fach) ~ "Verarbeitendes Gewerbe und Bergbau",
    stringr::str_ends("F073", .$fach) ~ "Architektur und Baugewerbe",
    stringr::str_ends("F078", .$fach) ~ "Interdisziplinäre Programme und Qualifikationen mit dem Schwerpunkt Ingenieurwesen,
 verarbeitendes Gewerbe und Baugewerbe",
    stringr::str_ends("F079", .$fach) ~ "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe nicht andernorts klassifiziert",
    stringr::str_detect("TOTAL", .$fach) ~ "Insgesamt",
    stringr::str_ends("UNK", .$fach) ~ "Unbekannt",
    T~.$fach
                             ))%>%
  dplyr::select(-fachbereich)


# %>%
#   arrange(fach, fachbereich)
#
# # warum hat fach bitte NAs????
# # hat es aktuell nicht (vgl. length(dat2$fach[is.na(dat2$fach) == TRUE]))
#
#   dat2$fachbereich <- zoo::na.locf(dat2$fachbereich)

  # lel <- is.na(dat2$wert)

dat3 <- dat2 %>%
  tidyr::pivot_wider(names_from = fach, values_from = wert)%>%
  dplyr::mutate(
    "Weitere Ingenieurwesen, verarbeitendes Gewerbe, Baugewerbe" = `Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe nicht andernorts klassifiziert`+
           `Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe nicht näher definiert`,
    "Weitere Naturwissenschaften, Mathematik und Statistik"= `Naturwissenschaften, Mathematik und Statistik nicht andernorts klassifiziert`+
           `Naturwissenschaften, Mathematik und Statistik nicht näher definiert`) %>%
  dplyr::mutate("Alle MINT-Fächer" = rowSums(
    dplyr::select(.,c(`Naturwissenschaften, Mathematik und Statistik`,
                      `Informatik & Kommunikationstechnologie`,
                      `Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe`)), na.rm = T )) %>%
  dplyr::mutate("Alle Nicht MINT-Fächer" = Insgesamt - `Alle MINT-Fächer`) %>%
  dplyr::select(-c("Naturwissenschaften, Mathematik und Statistik nicht näher definiert",
            "Naturwissenschaften, Mathematik und Statistik nicht andernorts klassifiziert",
            "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe nicht näher definiert",
            "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe nicht andernorts klassifiziert"))%>%
  tidyr::pivot_longer(c("Allgemeine Bildungsgänge und Qualifikationen":ncol(.)), values_to = "wert", names_to="fach")


dat4<- dat3 %>%
  dplyr::mutate(bereich = "Studium",
         quelle = "Eurostat",
         )%>%
  dplyr::mutate(mint_select= dplyr::case_when(fach %in% c("Naturwissenschaften, Mathematik und Statistik",
                                 "Informatik & Kommunikationstechnologie",
                                  "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                                  "Weitere Ingenieurwesen, verarbeitendes Gewerbe, Baugewerbe",
                                  "Weitere Naturwissenschaften, Mathematik und Statistik",
                                  "Biologie und verwandte Wissenschaften",
                                  "Umwelt",
                                  "Exakte Naturwissenschaften",
                                  "Mathematik und Statistik",
                                  "Interdisziplinäre Programme und Qualifikationen mit dem Schwerpunkt Naturwissenschaften,
Mathematik und Statistik","Ingenieurwesen und Technische Berufe",
                                  "Verarbeitendes Gewerbe und Bergbau",
                                  "Architektur und Baugewerbe",
                                  "Interdisziplinäre Programme und Qualifikationen mit dem Schwerpunkt Ingenieurwesen,
 verarbeitendes Gewerbe und Baugewerbe")~ "mint",
                                T ~ "nicht mint"
                     ))%>%
  dplyr::mutate(ebene= dplyr::case_when(fach %in% c("Allgemeine Bildungsgänge und Qualifikationen",
                                      "Pädagogik",
                                      "Geisteswissenschaften und Künste",
                                      "Sozialwissenschaften, Journalismus und Informationswesen",
                                      "Wirtschaft, Verwaltung und Recht",
                                      "Naturwissenschaften, Mathematik und Statistik",
                                      "Informatik & Kommunikationstechnologie",
                                      "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                                      "Landwirtschaft, Forstwirtschaft, Fischerei und Tiermedizin",
                                      "Gesundheit, Medizin und Sozialwesen",
                                      "Dienstleistungen",
                                      "Insgesamt",
                                      "Unbekannt",
                                      "Alle MINT-Fächer",
                                      "Alle Nicht MINT-Fächer")~ "1",
                          T~"2"))%>%
  dplyr::mutate(fachbereich = dplyr::case_when(fach %in% c("Allgemeine Bildungsgänge und Qualifikationen",
                                             "Pädagogik",
                                             "Geisteswissenschaften und Künste",
                                             "Sozialwissenschaften, Journalismus und Informationswesen",
                                             "Wirtschaft, Verwaltung und Recht",
                                             "Naturwissenschaften, Mathematik und Statistik",
                                             "Informatik & Kommunikationstechnologie",
                                             "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                                             "Landwirtschaft, Forstwirtschaft, Fischerei und Tiermedizin",
                                             "Gesundheit, Medizin und Sozialwesen",
                                             "Dienstleistungen",
                                             "Insgesamt",
                                             "Unbekannt",
                                             "Alle MINT-Fächer",
                                             "Alle Nicht MINT-Fächer")~.$fach,
                                 fach == "Architektur und Baugewerbe" ~ "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                                 fach == "Biologie und verwandte Wissenschaften" ~ "Naturwissenschaften, Mathematik und Statistik",
                                 fach == "Exakte Naturwissenschaften" ~ "Naturwissenschaften, Mathematik und Statistik",
                                 fach == "Ingenieurwesen und Technische Berufe" ~ "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                                 fach == "Interdisziplinäre Programme und Qualifikationen mit dem Schwerpunkt Ingenieurwesen,\n verarbeitendes Gewerbe und Baugewerbe" ~ "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                                 fach == "Interdisziplinäre Programme und Qualifikationen mit dem Schwerpunkt Naturwissenschaften,\nMathematik und Statistik" ~ "Naturwissenschaften, Mathematik und Statistik",
                                 fach == "Mathematik und Statistik" ~ "Naturwissenschaften, Mathematik und Statistik",
                                 fach == "Umwelt" ~ "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                                 fach == "Verarbeitendes Gewerbe und Bergbau" ~ "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                                 fach == "Weitere Ingenieurwesen, verarbeitendes Gewerbe, Baugewerbe" ~ "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                                 fach == "Weitere Naturwissenschaften, Mathematik und Statistik" ~ "Naturwissenschaften, Mathematik und Statistik"))%>%
  dplyr::mutate(population = "Europäische Union+")

# Anteil nach Geschlecht berechnen
## ausfilter, da wo es kein "Gesamt" zur berechnung gibt
f <- dat4 %>%
  group_by(fach, land, jahr) %>%
  count()

### filtern über left_join und alle mit 2 raus
eu_gat_e1 <- dat4
eu_gat_e1 <- eu_gat_e1 %>%
  dplyr::left_join(f, by = c("fach", "land", "jahr")) %>%
  dplyr::filter(n == 3)

### dann berechnung - unelegant, nächste lösung wäre auch hier schöner (Selbstkritik kbr)

 eu_gat_e1_f <- eu_gat_e1 %>%
  dplyr::group_by(fach, land, jahr) %>%
  dplyr::mutate((FA = wert[geschlecht == "Frauen"]/wert[geschlecht == "Gesamt"]*100),
                indikator = "Frauen-/Männeranteil") %>%
  dplyr::filter(geschlecht == "Frauen") %>%
   dplyr::select(-c(n, wert)) %>%
   dplyr::rename("wert" = "(...)")

 eu_gat_e1_m <- eu_gat_e1 %>%
  dplyr::group_by(fach, land, jahr) %>%
  dplyr::mutate(Männer =wert[geschlecht == "Männer"]/wert[geschlecht == "Gesamt"]*100,
                indikator = "Frauen-/Männeranteil") %>%
  dplyr::filter(geschlecht == "Männer") %>%
  dplyr::select(-c(n, wert)) %>%
  dplyr::rename("wert" = "Männer")

  eu_gat_e1 <- eu_gat_e1 %>%
    dplyr::group_by(fach, land, jahr) %>%
    dplyr::mutate(Gesamt = wert[geschlecht == "Gesamt"]/wert[geschlecht == "Gesamt"]*100,
                  indikator = "Frauen-/Männeranteil") %>%
    dplyr::filter(geschlecht == "Gesamt") %>%
    dplyr::select(-c(n, wert)) %>%
    dplyr::rename("wert" = "Gesamt")

  eu_gat_e1 <- rbind(eu_gat_e1, eu_gat_e1_f, eu_gat_e1_m)

# Anteil wer wählt was berechnen

  eu_ges <- dat4 %>%
    dplyr::filter(fach == "Insgesamt")

  eu_ww <- dat4 %>%
    dplyr::left_join(eu_ges, by =c("geschlecht", "land", "jahr", "indikator", "typ",
                                    "bereich", "quelle",
                                    "population" )) %>%
    dplyr::mutate(wert = wert.x / wert.y * 100) %>%
    dplyr::select(-c(fach.y, fachbereich.y, wert.x, wert.y, mint_select.y, ebene.y)) %>%
    dplyr::rename(fach = fach.x,
                  fachbereich = fachbereich.x,
                  ebene = ebene.x,
                  mint_select = mint_select.x) %>%
    dplyr::mutate(indikator = "Fächerwahl")

# Zusammenfassen

studierende_europa <- rbind(eu_ww, eu_gat_e1)

# ordnen
studierende_europa <- studierende_europa[,c("bereich", "quelle", "population", "typ",
                                            "indikator", "ebene", "mint_select", "fachbereich", "fach",
                                            "geschlecht", "land", "jahr", "wert")]

# Anpassung mint_select
studierende_europa$mint_select <- ifelse(studierende_europa$fachbereich == "Alle MINT-Fächer", "mint", studierende_europa$mint_select)

usethis::use_data(studierende_europa, overwrite = T)

## EUROSTAT - EUROSTAT003_custom_intern_studis_educ_uoe_mobs01__custom_7521082_linear.csv ----
library(rio)
library(dplyr)
library(tidyr)
library(magrittr)
library(stringr)
library(readxl)
library(janitor)
library(purrr)
library(readr)
library(countrycode)

dat_eust <- read_csv(paste0(pfad,"EUROSTAT003_custom_intern_studis_educ_uoe_mobs01__custom_7521082_linear.csv.gz"))


dat_eust1 <- dat_eust %>%
  select(-c(1:4))%>%
  rename(geschlecht = sex, land = geo, jahr = TIME_PERIOD, wert = OBS_VALUE,
         kommentar = OBS_FLAG, fach = iscedf13, anforderung = isced11)%>%
  mutate(across(land, ~ countrycode(., origin = "eurostat", destination="country.name.de", custom_match = c("EU28" = "EU (28)", "EU27_2020" = "EU (27), seit 2020"))))%>%
  mutate(geschlecht=case_when(geschlecht == "M"~ "Männlich",
                              geschlecht == "F" ~ "Weiblich",
                              T ~ "Gesamt"))%>%
  dplyr::mutate(ebene= dplyr::case_when(stringr::str_ends(.$fach,"F00|F01|F02|F03|F04|F05|F06|F07|F08|F09|F10|TOTAL") ~ 1,
                                        T ~2))%>%
  dplyr::mutate(mint_select= dplyr::case_when(stringr::str_detect(.$fach,"F05|F07") ~ "mint",
                                              T ~"nicht mint"))%>%
  dplyr::mutate(fach = dplyr::case_when(
    stringr::str_ends("F00", .$fach)~ "Allgemeine Bildungsgänge und Qualifikationen",
    stringr::str_ends("F01", .$fach)~ "Pädagogik",
    stringr::str_ends("F02", .$fach) ~ "Geisteswissenschaften und Künste",
    stringr::str_ends("F03", .$fach) ~ "Sozialwissenschaften, Journalismus und Informationswesen",
    stringr::str_ends("F04", .$fach) ~ "Wirtschaft, Verwaltung und Recht",
    stringr::str_ends("F05", .$fach) ~ "Naturwissenschaften, Mathematik und Statistik",
    stringr::str_ends("F06", .$fach) ~ "Informatik & Kommunikationstechnologie",
    stringr::str_ends("F07", .$fach) ~ "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
    stringr::str_ends("F08", .$fach) ~ "Landwirtschaft, Forstwirtschaft, Fischerei und Tiermedizin",
    stringr::str_ends("F09", .$fach) ~ "Gesundheit, Medizin und Sozialwesen",
    stringr::str_ends("F10", .$fach) ~ "Dienstleistungen",
    stringr::str_ends("F050", .$fach) ~ "Naturwissenschaften, Mathematik und Statistik nicht näher definiert",
    stringr::str_ends("F051", .$fach) ~ "Biologie und verwandte Wissenschaften",
    stringr::str_ends("F052", .$fach) ~ "Umwelt",
    stringr::str_ends("F053", .$fach) ~ "Exakte Naturwissenschaften",
    stringr::str_ends("F054", .$fach) ~ "Mathematik und Statistik",
    stringr::str_ends("F058", .$fach) ~ "Interdisziplinäre Programme und Qualifikationen mit dem Schwerpunkt Naturwissenschaften,
Mathematik und Statistik",
    stringr::str_ends("F059", .$fach) ~ "Naturwissenschaften, Mathematik und Statistik nicht andernorts klassifiziert",
    stringr::str_ends("F070", .$fach) ~ "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe nicht näher definiert",
    stringr::str_ends("F071", .$fach) ~ "Ingenieurwesen und Technische Berufe",
    stringr::str_ends("F072", .$fach) ~ "Verarbeitendes Gewerbe und Bergbau",
    stringr::str_ends("F073", .$fach) ~ "Architektur und Baugewerbe",
    stringr::str_ends("F078", .$fach) ~ "Interdisziplinäre Programme und Qualifikationen mit dem Schwerpunkt Ingenieurwesen,
 verarbeitendes Gewerbe und Baugewerbe",
    stringr::str_ends("F079", .$fach) ~ "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe nicht andernorts klassifiziert",
    stringr::str_detect("TOTAL", .$fach) ~ "Insgesamt",
    stringr::str_ends("UNK", .$fach) ~ "Unbekannt",
    T~.$fach))%>%
  mutate(anforderung = dplyr::case_when(anforderung ==  "ED5" ~ "kurzes tertiäres Bildungsprogramm (ISCED 5)",
    anforderung ==  "ED6" ~ "Bachelor oder vergleichbar (ISCED 6)",
    anforderung ==  "ED7" ~ "Master oder vergleichbar (ISCED 7)",
    anforderung ==  "ED8" ~ "Promotion (ISCED 8)",
    anforderung == "ED5-8" ~ "Tertiäre Bildung (gesamt)",
    T ~ anforderung
  ))%>%
  mutate(kommentar=case_when(kommentar == "b" ~ "break in time series",
                             kommentar == "e" ~ "estimated",
                             kommentar == "d" ~ "definition differs (see metadata)",
                             kommentar == "z" ~ "not applicable",
                             T ~ kommentar))%>%
  mutate(bereich = "hochschule",
         indikator = "Ausländische mobile Studierende",
         typ = "Anzahl")

dat_eust1_1<- dat_eust1 %>%
  dplyr::filter( ebene == 1)%>%
  dplyr::group_by(land, anforderung, jahr, geschlecht, mint_select)%>%
  dplyr::summarise(land, anforderung, jahr, geschlecht, ebene, mint_select, bereich, indikator, wert= sum(wert, na.rm=T))%>%
  dplyr::ungroup()%>%
  unique()%>%
  dplyr::mutate(fach = case_when(mint_select == "mint" ~ "MINT",
                           T ~ "Nicht MINT",
                           ),typ= "Anzahl")


dat_eust1_2 <- dat_eust1 %>%
  bind_rows(dat_eust1_1)


# warum ist der kleinste wert < 0 ???

studierende_mobil_eu_absolut <- dat_eust1_2

usethis::use_data(ausländisch_mobil, overwrite = T)

## EUROSTAT004_educ_uoe_mobs04__custom_8027463_linear.csv.gz----

dat_euro4 <- readr::read_csv(paste0(pfad, "EUROSTAT004_educ_uoe_mobs04__custom_8027463_linear.csv.gz"))

dat_euro4_1 <- dat_euro4 %>%
  dplyr::select(- c("DATAFLOW", "freq"  , "LAST UPDATE", "unit"))%>%
  dplyr::rename(geschlecht = sex, land = geo, jahr = TIME_PERIOD, wert = OBS_VALUE,
         kommentar = OBS_FLAG, fach = iscedf13, anforderung = isced11)%>%
  mutate(across(land, ~ countrycode(., origin = "eurostat", destination="country.name.de", custom_match = c("EU28" = "EU (28)", "EU27_2020" = "EU (27), seit 2020"))))%>%
  mutate(geschlecht=case_when(geschlecht == "M"~ "Männlich",
                              geschlecht == "F" ~ "Weiblich",
                              T ~ "Gesamt"))%>%
  dplyr::mutate(ebene= dplyr::case_when(stringr::str_ends(.$fach,"F00|F01|F02|F03|F04|F05|F06|F07|F08|F09|F10|TOTAL") ~ 1,
                                        T ~2))%>%
  dplyr::mutate(mint_select= dplyr::case_when(stringr::str_detect(.$fach,"F05|F07") ~ "mint",
                                        T ~"nicht mint"))%>%
  dplyr::mutate(fach = dplyr::case_when(
    stringr::str_ends("F00", .$fach)~ "Allgemeine Bildungsgänge und Qualifikationen",
    stringr::str_ends("F01", .$fach)~ "Pädagogik",
    stringr::str_ends("F02", .$fach) ~ "Geisteswissenschaften und Künste",
    stringr::str_ends("F03", .$fach) ~ "Sozialwissenschaften, Journalismus und Informationswesen",
    stringr::str_ends("F04", .$fach) ~ "Wirtschaft, Verwaltung und Recht",
    stringr::str_ends("F05", .$fach) ~ "Naturwissenschaften, Mathematik und Statistik",
    stringr::str_ends("F06", .$fach) ~ "Informatik & Kommunikationstechnologie",
    stringr::str_ends("F07", .$fach) ~ "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
    stringr::str_ends("F08", .$fach) ~ "Landwirtschaft, Forstwirtschaft, Fischerei und Tiermedizin",
    stringr::str_ends("F09", .$fach) ~ "Gesundheit, Medizin und Sozialwesen",
    stringr::str_ends("F10", .$fach) ~ "Dienstleistungen",
    stringr::str_ends("F050", .$fach) ~ "Naturwissenschaften, Mathematik und Statistik nicht näher definiert",
    stringr::str_ends("F051", .$fach) ~ "Biologie und verwandte Wissenschaften",
    stringr::str_ends("F052", .$fach) ~ "Umwelt",
    stringr::str_ends("F053", .$fach) ~ "Exakte Naturwissenschaften",
    stringr::str_ends("F054", .$fach) ~ "Mathematik und Statistik",
    stringr::str_ends("F058", .$fach) ~ "Interdisziplinäre Programme und Qualifikationen mit dem Schwerpunkt Naturwissenschaften,
Mathematik und Statistik",
    stringr::str_ends("F059", .$fach) ~ "Naturwissenschaften, Mathematik und Statistik nicht andernorts klassifiziert",
    stringr::str_ends("F070", .$fach) ~ "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe nicht näher definiert",
    stringr::str_ends("F071", .$fach) ~ "Ingenieurwesen und Technische Berufe",
    stringr::str_ends("F072", .$fach) ~ "Verarbeitendes Gewerbe und Bergbau",
    stringr::str_ends("F073", .$fach) ~ "Architektur und Baugewerbe",
    stringr::str_ends("F078", .$fach) ~ "Interdisziplinäre Programme und Qualifikationen mit dem Schwerpunkt Ingenieurwesen,
 verarbeitendes Gewerbe und Baugewerbe",
    stringr::str_ends("F079", .$fach) ~ "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe nicht andernorts klassifiziert",
    stringr::str_detect("TOTAL", .$fach) ~ "Insgesamt",
    stringr::str_ends("UNK", .$fach) ~ "Unbekannt",
    T~.$fach))%>%
  mutate(anforderung = dplyr::case_when(anforderung ==  "ED5" ~ "kurzes tertiäres Bildungsprogramm (ISCED 5)",
                                        anforderung ==  "ED6" ~ "Bachelor oder vergleichbar (ISCED 6)",
                                        anforderung ==  "ED7" ~ "Master oder vergleichbar (ISCED 7)",
                                        anforderung ==  "ED8" ~ "Promotion (ISCED 8)",
                                        anforderung == "ED5-8" ~ "Tertiäre Bildung (gesamt)",
                                        T ~ anforderung
  ))%>%
  mutate(kommentar=case_when(kommentar == "b" ~ "break in time series",
                             kommentar == "e" ~ "estimated",
                             kommentar == "d" ~ "definition differs (see metadata)",
                             kommentar == "z" ~ "not applicable",
                             T ~ kommentar))%>%
  mutate(bereich = "hochschule",
         indikator = "Ausländische mobile Studierende",
         typ = "Anteil")

dat_euro4_2 <- dat_euro4_1 %>%
  dplyr::filter(mint_select == "mint" & ebene == 1)%>%
  dplyr::group_by(land, anforderung, jahr, geschlecht)%>%
  dplyr::summarise(land, anforderung, jahr, geschlecht, ebene, mint_select, bereich, indikator, MINT= sum(wert, na.rm=T))%>%
  dplyr::ungroup()%>%
  dplyr::mutate("Nicht MINT" = 100 - MINT,
                typ= "Anteil")%>%
  tidyr::pivot_longer(c("MINT", "Nicht MINT"), names_to = "fach", values_to = "wert" )%>%
  unique()

dat_euro4_3 <- dat_euro4_1 %>%
  dplyr::bind_rows(dat_euro4_2)



studierende_mobil_eu_share <- dat_euro4_3

# warum ist der kleinste wert < 0 ???

usethis::use_data(ausländisch_mobil_share, overwrite = T)

## UNESCO001_anteil_MINT_absolv_weltweit ----
# kbr bei mir ist der Pfad leicht anders ... unpraktisch

pfad <- path_kek

dat_unc <- readr::read_csv(paste0(pfad, "UNESCO001_anteil_MINT_absolv_weltweit.csv"))

dat_unc_1<- dat_unc %>%
  dplyr::select(Indicator, Value, Country, Time)%>%
  dplyr::rename( indikator = Indicator, wert = Value, land = Country, jahr = Time)%>%
  dplyr::mutate(typ = "In Prozent", geschlecht = "Insgesamt")%>%
  dplyr::mutate(fach =dplyr:: case_when(stringr::str_detect(.$indikator, "other than") ~ "Alle Nicht MINT-Fächer",
                          T~"Alle MINT-Fächer" ))%>%
  dplyr::mutate(mint_select = dplyr::case_when(fach == "Alle MINT-Fächer"~"mint",
                              T ~ "nicht mint" ))%>%
  dplyr::mutate(indikator = "Abslovent:innen", bereich = "Studium", quelle = "UNESCO", population= "Weltweit")

dat_unc_1$land <- countrycode::countrycode(dat_unc_1$land, origin = 'country.name' , destination = "country.name.de")


studierende_absolventen_weltweit <- dat_unc_1

# ordnen und speichern
studierende_absolventen_weltweit <- studierende_absolventen_weltweit[,c("bereich", "quelle", "population", "typ",
                                            "indikator", "mint_select", "fach",
                                            "geschlecht", "land", "jahr", "wert")]

usethis::use_data(studierende_absolventen_weltweit, overwrite = T)



## OECD Anteil internationaler Studis an allen Studis in Fach ----

file_path <- paste0("C:/Users/", akro, "/OneDrive - Stifterverband/AP7 MINT-DataLab/02 Datenmaterial/01_Rohdaten/02_Alle Daten")

dat_oecd <- readr::read_csv(paste0(pfad, "OECD006_Anteil_intern_Studis_in_Fach_2.csv"))


dat_oecd1 <- dat_oecd %>%
  dplyr::select(Country, EDUCATION_LEV, SEX, EDUCATION_FIELD,
                Year, Value, Indicator,
                Mobility) %>%
  dplyr::rename(land = Country,
                anforderung = EDUCATION_LEV,
                geschlecht = SEX,
                fach = EDUCATION_FIELD,
                jahr = Year,
                wert = Value,
                intl_selector = Mobility,
                indikator = Indicator
              )%>%
  dplyr::mutate(geschlecht = dplyr::case_when(geschlecht== "F" ~ "Frauen",
                              geschlecht== "M" ~ "Männer",
                              T ~ "Insgesamt"))

# share of intl students by field/total= verteilung der intl studis über fächer / distribution of

dat_oecd1$land <- countrycode::countrycode(dat_oecd1$land, origin = 'country.name', destination = "country.name.de",
                                           custom_match = c("OECD - Europe" = "OECD (Europa)", "OECD - Total" = "OECD (Total)",
                                                            "OECD - Average" = "OECD (Durchschnitt)"))

# Anforderungsniveau und fach zuweisen
dat_oecd2 <- dat_oecd1 %>%
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
  ))%>%
  dplyr::mutate(fach = dplyr::case_when(
    stringr::str_ends("F00", .$fach)~ "Allgemeine Bildungsgänge und Qualifikationen",
    stringr::str_ends("F01", .$fach)~ "Pädagogik",
    stringr::str_ends("F02", .$fach) ~ "Geisteswissenschaften und Künste",
    stringr::str_ends("F03", .$fach) ~ "Sozialwissenschaften, Journalismus und Informationswesen",
    stringr::str_ends("F04", .$fach) ~ "Wirtschaft, Verwaltung und Recht",
    stringr::str_ends("F05", .$fach) ~ "Naturwissenschaften, Mathematik und Statistik",
    stringr::str_ends("F06", .$fach) ~ "Informatik & Kommunikationstechnologie",
    stringr::str_ends("F07", .$fach) ~ "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
    stringr::str_ends("F08", .$fach) ~ "Landwirtschaft, Forstwirtschaft, Fischerei und Tiermedizin",
    stringr::str_ends("F09", .$fach) ~ "Gesundheit, Medizin und Sozialwesen",
    stringr::str_ends("F10", .$fach) ~ "Dienstleistungen",
    stringr::str_ends("F050", .$fach) ~ "Naturwissenschaften, Mathematik und Statistik nicht näher definiert",
    stringr::str_ends("F051", .$fach) ~ "Biologie und verwandte Wissenschaften",
    stringr::str_ends("F052", .$fach) ~ "Umwelt",
    stringr::str_ends("F053", .$fach) ~ "Exakte Naturwissenschaften",
    stringr::str_ends("F054", .$fach) ~ "Mathematik und Statistik",
    stringr::str_ends("F058", .$fach) ~ "Interdisziplinäre Programme und Qualifikationen mit dem Schwerpunkt Naturwissenschaften,
Mathematik und Statistik",
    stringr::str_ends("F059", .$fach) ~ "Naturwissenschaften, Mathematik und Statistik nicht andernorts klassifiziert",
    stringr::str_ends("F070", .$fach) ~ "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe nicht näher definiert",
    stringr::str_ends("F071", .$fach) ~ "Ingenieurwesen und Technische Berufe",
    stringr::str_ends("F072", .$fach) ~ "Verarbeitendes Gewerbe und Bergbau",
    stringr::str_ends("F073", .$fach) ~ "Architektur und Baugewerbe",
    stringr::str_ends("F078", .$fach) ~ "Interdisziplinäre Programme und Qualifikationen mit dem Schwerpunkt Ingenieurwesen,
 verarbeitendes Gewerbe und Baugewerbe",
    stringr::str_ends("F079", .$fach) ~ "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe nicht andernorts klassifiziert",
    stringr::str_detect("_T", .$fach) ~ "Insgesamt",
    stringr::str_ends("F05T07", .$fach) ~ "Alle MINT-Fächer"))%>%
  dplyr::rename(fachbereich = fach)


dat_oecd3 <- dat_oecd2 %>%
  dplyr::filter(indikator == "Share of students enrolled by field")%>%
  dplyr::mutate(indikator = dplyr::case_when( intl_selector=="Total"~ "Studierende",
                                intl_selector=="Mobile including homecoming nationals" ~ "Internationale Studierende",
                                T ~ "nicht-internationale Studierende"
                                ))%>%
  dplyr::select(- intl_selector)%>%
  dplyr::mutate(typ = "In Prozent",
         population= "OECD")

dat_oecd4 <- dat_oecd3%>%
  tidyr::pivot_wider(names_from=fachbereich, values_from =wert)%>%
  dplyr::mutate("Alle Nicht MINT-Fächer" = 100-`Alle MINT-Fächer`)%>%
  tidyr::pivot_longer(c("Alle Nicht MINT-Fächer", "Alle MINT-Fächer",
                 "Allgemeine Bildungsgänge und Qualifikationen" ,
                 "Dienstleistungen",
                 "Geisteswissenschaften und Künste" ,
                 "Gesundheit, Medizin und Sozialwesen" ,
                 "Informatik & Kommunikationstechnologie" ,
                 "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe" ,
                 "Landwirtschaft, Forstwirtschaft, Fischerei und Tiermedizin" ,
                 "Naturwissenschaften, Mathematik und Statistik" ,
                 "Pädagogik" ,
                 "Sozialwissenschaften, Journalismus und Informationswesen" ,
                 "Wirtschaft, Verwaltung und Recht" ), values_to = "wert", names_to = "fachbereich")

dat_oecd4$mint_select <- "nicht mint"
dat_oecd4 <- dat_oecd4 %>%
  dplyr::mutate(mint_select = dplyr::case_when(
    fachbereich == "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe" |
      fachbereich == "Naturwissenschaften, Mathematik und Statistik" |
      fachbereich == "Alle MINT-Fächer" ~ "mint",
    T ~ mint_select
  ))

dat_oecd4$geschlecht[dat_oecd4$geschlecht == "Insgesamt"] <- "Gesamt"
studierende_intern_oecd <- dat_oecd4

# ordnen
studierende_intern_oecd  <- studierende_intern_oecd[,c("population", "typ",
                                                      "indikator", "mint_select", "fachbereich",
                                                      "anforderung",
                                                      "geschlecht", "land", "jahr", "wert")]


# speichern
usethis::use_data(studierende_intern_oecd, overwrite = T)

## OECD_Mobile by gender ---

# Raus warum?
# file_path <- paste0("C:/Users/", akro, "/OneDrive - Stifterverband/AP7 MINT-DataLab/02 Datenmaterial/01_Rohdaten/02_Alle Daten")
#
# dat_oecd_bg <- read_csv(paste0(file_path, "/", "OECD007_mobile_by_gender_test.csv"))


## OECD - Anzahl Azubis / Studis nach Feld & Gender------------------------

### Rohdaten einlesen -------------------------------------------------------
akro <- "kbr"
dat <- read.csv(paste0(pfad,
                       "OECD005_Anzahl_Studi_Azubi_nach_Fach_Sex.csv"),
                header = TRUE, sep = ",", dec = ".")
pfad <- path_kek

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

dat <- dat %>%
  dplyr::mutate(fach = dplyr::case_when(stringr::str_ends("F00", dat$fach)~ "Allgemeine Bildungsgänge und Qualifikationen",
                                        stringr::str_ends("F01", dat$fach)~ "Pädagogik",
                                        stringr::str_ends("F02", dat$fach) ~ "Geisteswissenschaften und Künste",
                                        stringr::str_ends("F03", dat$fach) ~ "Sozialwissenschaften, Journalismus und Informationswesen",
                                        stringr::str_ends("F04", dat$fach) ~ "Wirtschaft, Verwaltung und Recht",
                                        stringr::str_ends("F05", dat$fach) ~ "Naturwissenschaften, Mathematik und Statistik",
                                        stringr::str_ends("F06", dat$fach) ~ "Informatik & Kommunikationstechnologie",
                                        stringr::str_ends("F061", dat$fach) ~ "Informatik & Kommunikationstechnologie allgemein",
                                        stringr::str_ends("F068", dat$fach) ~ "Interdisziplinäre Programme mit Schwerpunkt Informatik & Kommunikationstechnologie",
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
                                        T ~ dat$fach),
                fachbereich = dplyr::case_when(
                  stringr::str_ends("F00", dat$fach)~ "Allgemeine Bildungsgänge und Qualifikationen",
                                        stringr::str_ends("F01", dat$fach)~ "Pädagogik",
                                        stringr::str_ends("F02", dat$fach) ~ "Geisteswissenschaften und Künste",
                                        stringr::str_ends("F03", dat$fach) ~ "Sozialwissenschaften, Journalismus und Informationswesen",
                                        stringr::str_ends("F04", dat$fach) ~ "Wirtschaft, Verwaltung und Recht",
                                        stringr::str_ends("F05", dat$fach) ~ "Naturwissenschaften, Mathematik und Statistik",
                                        stringr::str_ends("F06", dat$fach) ~ "Informatik & Kommunikationstechnologie",
                                        stringr::str_ends("F061", dat$fach) ~ "Informatik & Kommunikationstechnologie",
                                        stringr::str_ends("F068", dat$fach) ~ "Informatik & Kommunikationstechnologie",
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
                                        stringr::str_ends("F99", dat$fach) ~ "Unbekannt"
                ))

# Fachbereich und mint_select ergänzen
dat$fachbereich <- dat$fach
dat <- dat %>%
  dplyr::mutate(fachbereich = dplyr::case_when(
    fach == "Biologie und verwandte Wissenschaften" ~ "Naturwissenschaften, Mathematik und Statistik",
    fach == "Umwelt" ~ "Naturwissenschaften, Mathematik und Statistik",
    fach == "Exakte Naturwissenschaften" ~ "Naturwissenschaften, Mathematik und Statistik",
    fach == "Mathematik und Statistik" ~ "Naturwissenschaften, Mathematik und Statistik",
    fach == "Interdisziplinäre Programme und Qualifikationen mit dem Schwerpunkt Naturwissenschaften,
Mathematik und Statistik" ~ "Naturwissenschaften, Mathematik und Statistik",
    fach == "Naturwissenschaften, Mathematik und Statistik nicht andernorts klassifiziert" ~ "Naturwissenschaften, Mathematik und Statistik",

    fach ==  "Informatik & Kommunikationstechnologie allgemein" ~ "Informatik & Kommunikationstechnologie",
    fach == "Interdisziplinäre Programme mit Schwerpunkt Informatik & Kommunikationstechnologie" ~ "Informatik & Kommunikationstechnologie",

    fach == "Ingenieurwesen und Technische Berufe" ~ "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
    fach == "Verarbeitendes Gewerbe und Bergbau" ~ "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
    fach == "Architektur und Baugewerbe" ~ "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
    fach == "Interdisziplinäre Programme und Qualifikationen mit dem Schwerpunkt Ingenieurwesen,
 verarbeitendes Gewerbe und Baugewerbe" ~ "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
    fach == "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe nicht andernorts klassifiziert" ~ "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",

    fach == "Weitere Naturwissenschaften, Mathematik und Statistik" ~ "Naturwissenschaften, Mathematik und Statistik",
    fach == "Weitere Ingenieurwesen, verarbeitendes Gewerbe, Baugewerbe" ~ "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
    T ~ fachbereich
  ))

dat$mint_select <- "nicht mint"
dat <- dat %>%
  dplyr::mutate(mint_select = dplyr::case_when(
    fachbereich == "Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe" |
      fachbereich == "Naturwissenschaften, Mathematik und Statistik" |
      fachbereich == "Informatik & Kommunikationstechnologie" |
      fachbereich == "MINT" ~ "mint",
    T ~ mint_select
  ),
  ebene = dplyr::case_when(
    fachbereich %in% c("Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                      "Naturwissenschaften, Mathematik und Statistik",
                      "Informatik & Kommunikationstechnologie") &
      !(fach %in% c("Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                    "Naturwissenschaften, Mathematik und Statistik",
                    "Informatik & Kommunikationstechnologie")) ~ 2,
    fachbereich %in% c("Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                      "Naturwissenschaften, Mathematik und Statistik",
                      "Informatik & Kommunikationstechnologie") &
      fach %in% c("Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                    "Naturwissenschaften, Mathematik und Statistik",
                  "Informatik & Kommunikationstechnologie") ~ 1,
    !(fachbereich %in% c("Ingenieurwesen, verarbeitendes Gewerbe und Baugewerbe",
                         "Naturwissenschaften, Mathematik und Statistik",
                         "Informatik & Kommunikationstechnologie")) ~ 1
  ))


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

# filtern nach akademisch
dat <- dat %>%
  dplyr::filter(indikator == "akademisch") %>%
  dplyr::select(-indikator)

# missings ausfiltern
dat <- na.omit(dat)
dat$wert <- round(dat$wert)

# bereich ergänze und in Reihenfolge bringen
dat$bereich <- "Studium"
dat$indikator <- "Anzahl Studierende"
dat$quelle <- "OECD"
dat$population <- "OECD"
dat$typ <- "Anzahl"


# Spalten in logische Reihenfolge bringen
dat<- dat[,c("bereich", "quelle", "typ", "indikator", "mint_select",
             "ebene", "fachbereich", "fach",
             "geschlecht", "population", "land_code", "land", "jahr", "anforderung", "wert")]
studierende_anzahl_oecd <- dat

# speichern
usethis::use_data(studierende_anzahl_oecd, overwrite = T)
