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

# Studierende ----

setwd("C:/Users/kab/Downloads/datalab/datalab/data-raw/raw")

get_data <- function(file_list, fach_list){

  test<-  read_excel(file_list, sheet =fach_list, col_names = F)%>%
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
file_list <- rep(c("DES064_bmbfstu1_2021.xlsx",
                   "DES050_bmbfstu1_2020.xlsx",
                   "DES049_bmbfstu1_2019.xlsx",
                   "DES048_bmbfstu1_2018.xlsx",
                   "DES047_bmbfstu1_2017.xlsx",
                   "DES046_bmbfstu1_2016.xlsx",
                   "DES045_bmbfstu1_2015.xlsx",
                   "DES044_bmbfstu1_2014.xlsx",
                   "DES043_bmbfstu1_2013.xlsx",
                   "DES042_bmbfstu1_2012.xlsx",
                   "DES041_bmbfstu1_2011.xlsx",
                   "DES040_bmbfstu1_2010.xlsx"),each=3)

# Hier je neuer hinzugefügter datei, counter um 3 erhöhen
fach_list<- rep(c("Insgesamt", "Mathe", "Ingenieur"), times= 12)


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




#usethis::use_data(studierende, overwrite = T)


duplika <- janitor::get_dupes(studierende, c(region, indikator, geschlecht))

# Studierende detailliert ----


#-----------------------------------------------------------------------------------------------------------------#
# Ein Datensatz, der nur die Datensätze für mit mit Fächerunterscheidung beinhaltet für alle Indikatoren          #
#-----------------------------------------------------------------------------------------------------------------#

## Creating and Cleaning ----

#setwd("C:/Users/kab/Downloads/datalab/datalab/data-raw/raw")


# Funktion zur Extrahierung von Rohdatensätzen

clean_des <- function (dat,year){

  raw <- read_excel(dat, col_types = "text")

# Indexe prüfen!
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

# export(master_faecher_output2, "data_raw/studierende_faecher_08_06_23.xlsx")

# setwd("C:/Users/kab/Downloads/datalab/datalab/data-raw")





usethis::use_data(studierende_detailliert, overwrite = T)


# cleaning facher, creating master ---------------

# clean_des <- function (dat,year){
#
# raw <- read_excel(dat, col_types = "text")
#
# raw <- raw[-c(1:6),-c(1,3,5)]
#
# colnames(raw) <- c("region", "fachgruppe", "fach", "gesamt", "weiblich",
#                      "auslaender", "lehramt", "lehramt_weiblich",
#                      "gesamt_1hs", "weiblich_1hs", "auslaender_1hs", "gesamt_1fs",
#                    "weiblich_1fs")
#
# raw$jahr <- year
# raw$bereich <- "hochschule"
# raw$hinweise <- NA
# raw$quelle <- "Statistisches Bundesamt (Destatis), 2022: Auf Anfrage"
#
# return(raw)
#
# }
#
# raw2018 <- clean_des(dat= "DES60_Kroeger_Stud_Land_FG_STB_2018.xlsx", year="2018")
# raw2019 <- clean_des(dat= "DES61_Kroeger_Stud_Land_FG_STB_2019.xlsx", year="2019")
# raw2020 <- clean_des(dat= "DES62_Kroeger_Stud_Land_FG_STB_2020.xlsx", year="2020")
# raw2021 <- clean_des(dat= "DES63_Kroeger_Stud_Land_FG_STB_2021.xlsx", year="2021")

# creating master
# master <- bind_rows(raw2018,raw2019,raw2020, raw2021)

# in altes format bringen für append

#  master_altes_format <- master %>%
#    pivot_longer(c(4:13), values_to="wert", names_to="dummy") %>%
#    mutate(
#      nur_lehramt=case_when(
#      str_detect(.$dummy, "lehramt") ~ "Ja",
#      T ~ "Nein"),
#      anzeige_geschlecht=case_when(
#        str_detect(.$dummy, "weiblich") ~ "frauen", T ~ "gesamt"),
#      typ=case_when(
# str_detect(.$dummy, "1hs") ~ "studienanfänger", T ~ "studierende"
#     ))
#
#  master_altes_format$typ2 <- ifelse(grepl("auslaender", master_altes_format$dummy),"auslaender",NA)
#
#  master_altes_format_2 <- master_altes_format%>%unite("indikator", 12:13,na.rm = TRUE)%>%
# select(-dummy)
#
# master_altes_format_2$indikator <- gsub("Studienanfänger", "studienanfänger:innen", master_altes_format_2$indikator)
#
# master_append <- master_altes_format_2

# export
#export(master_fn, "Studierende_inkl_auslaendern.xlsx")







# Neuer Datensatz frische Struktur-------


# if(master$fachgruppe == "Geisteswissenschaften"){
#   master_tada <- filter(master, fachgruppe == "Geisteswissenschaften" & fach == "Zusammen" )
# }

# master_tada <- master %>%
#   group_by(fachgruppe) %>%
#   filter(fach == "Zusammen"|fach == "Mathematik, Naturwissenschaften allgemein"| fach =="Mathematik"
#          |fach =="Physik, Astronomie"
#          |fach =="Chemie"
#          |fach =="Pharmazie"
#          |fach =="Biologie"
#          |fach =="Geowissenschaften (ohne Geographie)"
#          |fach =="Geographie"
#          |fach =="Ingenieurwesen allgemein"
#          |fach =="Maschinenbau/Verfahrenstechnik"
#          |fach =="Elektrotechnik und Informationstechnik"
#          |fach =="Verkehrstechnik, Nautik"
#          |fach =="Architektur, Innenarchitektur"
#          |fach =="Raumplanung"
#          |fach =="Bauingenieurwesen"
#          |fach =="Vermessungswesen"
#          |fach =="Wirtschaftsingenieurwesen mit ingenieurwissenschaftlichem Schwerpunkt"
#          |fach =="Informatik"
#          |fach =="Materialwissenschaft und Werkstofftechnik"
#          )
#
# master_tada_long <- master_tada %>%
#   pivot_longer(c(4:13), names_to = "indikator", values_to = "wert")%>%
#   unite(dummy,2:3, sep="_")%>%
#   pivot_wider(names_from = dummy, values_from = wert)
#
# colnames(master_tada_long)[10] <- "Mathematik, Naturwissenschaften_Weitere naturwissenschaftliche und mathematische Fächer"
# colnames(master_tada_long)[21] <- "Ingenieurwissenschaft_Weitere ingenieurwissenschaftliche Fächer"
#
#
# master_tada_long[,c(7:35)] <- purrr::map_dfr(.x = master_tada_long[,c(7:35)], .f =as.numeric )
#
# master_tada_long_1 <- master_tada_long %>%
#   mutate(`Mathematik, Naturwissenschaften_Geowissenschaften und Geographie` =
#            `Mathematik, Naturwissenschaften_Geowissenschaften (ohne Geographie)`+
#            `Mathematik, Naturwissenschaften_Geographie`,
#          `Mathematik, Naturwissenschaften_Naturwissenschaften` =
#            `Mathematik, Naturwissenschaften_Physik, Astronomie`+
#            `Mathematik, Naturwissenschaften_Chemie`+
#            `Mathematik, Naturwissenschaften_Biologie`,
#          `Ingenieurwissenschaften_Ingenieurwissenschaften ohne Informatik`=
#            `Ingenieurwissenschaften_Zusammen`-
#            `Ingenieurwissenschaften_Informatik`,
#          MINT_MINT = `Ingenieurwissenschaften_Zusammen` +
#            `Mathematik, Naturwissenschaften_Zusammen`)%>%
#   select(-`Mathematik, Naturwissenschaften_Geowissenschaften (ohne Geographie)`,
#          -`Mathematik, Naturwissenschaften_Geographie`)%>%
#   pivot_longer(c(7:37), names_to = "dummy", values_to ="wert")%>%
#   separate(dummy, c("fachbereich", "fach"), sep="_")
#
# master_faecher_output <- master_tada_long_1 %>%
#   select(bereich, indikator, region, jahr, fachbereich, fach, wert, quelle, hinweise)
#
#
# master_faecher_output$fach <- gsub("Zusammen", "Alle Fächer", master_faecher_output$fach )
# master_faecher_output$fach <- gsub("MINT", "Alle MINT-Fächer", master_faecher_output$fach )
#
#
# master_faecher_output <- master_faecher_output %>%
#   mutate(
#     geschlecht = case_when(
#     str_detect(.$indikator, "weiblich") ~ "Frauen",
#     T ~ "Gesamt"),
#     dummy_indi=case_when(
#     str_detect(.$indikator, "1hs")~ "Studienanfänger:innen_1hs",
#     str_detect(.$indikator, "1fs")~ "Studienanfänger:innen_1fs",
#     T~ "Studierende")
#     # ,
#     # lehramt = case_when(
#     #   str_detect(.$indikator, "lehramt")~ "Ja",
#     #   T ~ "Nein")
#     )
#
# master_faecher_output$dummy_indi <- ifelse(grepl("lehramt", master_faecher_output$indikator),
#                                            paste0(master_faecher_output$dummy_indi, "_Lehramt"),
#                                            master_faecher_output$dummy_indi)
#
# master_faecher_output$dummy_indi <- ifelse(grepl("auslaender", master_faecher_output$indikator),
#                                            paste0(master_faecher_output$dummy_indi, "_Ausländisch"),
#                                            master_faecher_output$dummy_indi)
#
#
# master_faecher_output1 <- master_faecher_output %>%
#   select(bereich, dummy_indi,  region, jahr, fachbereich, fach, geschlecht, wert, quelle, hinweise
#   )%>%
#   rename(indikator=dummy_indi)%>%
#   mutate(label=case_when(
#     indikator=="Studienanfänger:innen_1fs" ~ "Studienanfänger:innen (1. Fachsemester)",
#     indikator=="Studienanfänger:innen_1hs" ~ "Studienanfänger:innen (1. Hochschulsemester)",
#     indikator=="Studienanfänger:innen_1hs_Ausländisch"~ "Auländische Studienanfänger:innen (1. Hochschulsemester)",
#     indikator=="Studierende" ~ "Studierende",
#     indikator=="Studierende_Ausländisch" ~ "Ausländische Studierende",
#     indikator=="Studierende_Lehramt" ~ "Studierende (Nur Lehramt)"
#   ))%>%
# select(- hinweise, -quelle)%>%
#   mutate(fachbereich =case_when (
#     fachbereich== "Ingenieurwissenschaft"~ "Ingenieurwissenschaften",
#     T~.$fachbereich
#   ))%>%
#   mutate(mint_select=case_when(
#     fachbereich== "Mathematik, Naturwissenschaften" ~"MINT",
#     fachbereich== "Ingenieurwissenschaften" ~ "MINT",
#     T~ "NIcht MINT"
#   ))
#
#
# #export(master_faecher_output1, "output/studierende_faecher.xlsx")
#
#
# #
# #
# #
# # Alte Daten aktualiseren---------------
#
# studi_alt <- import("Studierende_alt_13_01.xlsx")
#
# createdata <- function(data, Jahr, Fachbereich) {
#
#   # add headlines
#   header <- c("region", "insgesamt_stud","insgesamt_1hs", "insgesamt_1fs", "uni_stud", "uni_1hs", "uni_1fs",
#               "lehramt_stud","lehramt_1hs", "lehramt_1fs", "fh_stud", "fh_1hs" , "fh_1fs")
#   colnames(data) <- header
#
#   # create columns/variables
#
#   ### Anzeige
#   data$anzeige_geschlecht <- "gesamt"
#   data$index <- 1:nrow(data)
#   data$anzeige_geschlecht[data$index >20] <- "frauen"
#   # überflüssiges löschen:
#   data <- data[-c(1,19, 20, 36,37),]
#   data <- data %>%
#     select(-index)
#
#   # ins long-Format bringen
#   data <- data %>%
#     pivot_longer(cols = insgesamt_stud:fh_1fs, values_to = "wert")
#
#   ### indikator, lehramt, hochschulform
#   data <- data %>%
#     mutate(indikator=case_when(
#       str_detect(data$name, "stud") ~ "Studierende",
#       str_detect(data$name, "1hs") ~ "Studienanfänger_1hs",
#       str_detect(data$name, "1fs") ~ "Studienanfänger_1fs"),
#       nur_lehramt=case_when(
#         str_detect(data$name, "lehramt") ~ "Ja",
#         T ~ "Nein"),
#       hochschulform=case_when(
#         str_detect(data$name, "insgesamt") ~ "insgesamt",
#         str_detect(data$name, "fh_") ~ "FH",
#         T ~ "Uni")
#     )
#
#   ### fachbereich
#   data$fachbereich <- Fachbereich
#   data$Jahr <- Jahr
#   data$quelle <- "Statistisches Bundesamt (Destatis), 2021: Auf Anfrage"
#   data$hinweise <- "Studienanfänger im ersten Hochschulsemester"
#
#   ### bereich (für alle gleich)
#   data$bereich <- "Hochschule"
#
#   # überflüssige Spalte "name" entfernen
#   data <- data %>%
#     select(-name)
#
#   return(data)
#
# }






# stu2021_alleFG <- read_excel("DES064_bmbfstu1_2021.xlsx", sheet = "Insgesamt", col_names = F)[c(14:33,36:52), c(1:4,5:7, 11:13, 14:16)]
# stu2021_Mathe <-  read_excel("DES064_bmbfstu1_2021.xlsx", sheet = "Mathe", col_names = F)[c(14:33,36:52), c(1:4,5:7, 11:13, 14:16)]
# stu2021_Ing <-    read_excel("DES064_bmbfstu1_2021.xlsx", sheet = "Ingenieur", col_names = F)[c(14:33,36:52), c(1:4,5:7, 11:13, 14:16)]
#
# stu2021_alleFG <- createdata(stu2021_alleFG, "2021", "Alle")
# stu2021_Mathe <- createdata(stu2021_Mathe, "2021", "Mathe")
# stu2021_Ing <- createdata(stu2021_Ing, "2021", "Ingenieur")
#
# data_studi_neu <- bind_rows(stu2021_Ing,stu2021_Mathe,stu2021_alleFG
#                             , studi_alt
#                             )
#
# data_studi_neu$indikator <- gsub("Studienanfänger", "Studienanfänger:innen", data_studi_neu$indikator)
#
# data_studi_neu <- data_studi_neu %>%
#   # mutate(indikator=case_when(
#   #   .$indikator == "Studienanfänger:innen:innen"~ "Studienanfänger:innen",
#   #   T ~ .$indikator
#   # ))%>%
#   mutate(indikator= case_when(
#     str_detect(.$nur_lehramt, "Ja") ~ paste0(.$indikator, "_lehramt"),
#     T ~ .$indikator
#   )) %>%
#  select(-nur_lehramt)
#
#
#
#
# data_studi_neu$fachbereich <- gsub("Ingenieur", "Ingenieurwissenschaften", data_studi_neu$fachbereich)
# data_studi_neu$fachbereich <- gsub("Mathe", "Mathematik_Naturwissenschaften", data_studi_neu$fachbereich)
# data_studi_neu$region <- gsub("	Deutschland ...", "Deutschland", data_studi_neu$region)
#
# colnames(data_studi_neu)[2] <- "geschlecht"
#
#
#
# data_studi_neu1 <- data_studi_neu %>%
#   mutate(indikator = case_when(hochschulform == "Uni" ~ paste0(.$indikator, "_Uni"),
#                                hochschulform == "FH" ~ paste0(.$indikator, "_FH"),
#                                T ~ .$indikator))%>%
#   mutate(
#          label= case_when(.$indikator == "Studienanfänger:innen_1fs" ~ "Studienanfänger:innen (1.Fachsemester)"
#                           ,.$indikator == "Studienanfänger:innen_1fs_FH" ~ "Studienanfänger:innen (Fachhochschulen, 1.Fachsemester)"
#                           ,.$indikator == "Studienanfänger:innen_1fs_Uni" ~ "Studienanfänger:innen (Universität, 1.Fachsemester)",
#
#
#                           .$indikator == "Studienanfänger:innen_1fs_lehramt_Uni" ~ "Studienanfänger:innen (Lehramt, Universität, 1.Fachsemester)",
#
#                           .$indikator == "Studienanfänger:innen_1hs" ~ "Studienanfänger:innen (1.Hochschulsemester)",
#                           .$indikator == "Studienanfänger:innen_1hs_FH" ~ "Studienanfänger:innen (Fachhochschulen, 1.Hochschulsemester)",
#                           .$indikator == "Studienanfänger:innen_1hs_lehramt_Uni" ~ "Studienanfänger:innen (Lehramt, Universität, 1.Hochschulsemester)",
#
#                           .$indikator == "Studienanfänger:innen_1hs_Uni" ~ "Studienanfänger:innen (Universität, 1.Hochschulsemester)",
#                           .$indikator == "Studierende" ~ "Studierende",
#                           .$indikator == "Studierende_FH" ~ "Studierende (Fachhochschulen)",
#                           .$indikator == "Studierende_lehramt_Uni" ~ "Studierende (Lehramt, Universität)",
#                           .$indikator == "Studierende_Uni" ~ "Studierende (Universität)",
#                           ))
#
# data_studi_neu1$hinweise <- NA



#export(data_studi_neu1, "output/Studierende_2023.xlsx")

#
#
#
# Hybrid: Studi + alle fächer ab 2018---------------
# colnames(data_studi_neu1) [7] <- "jahr"
#
# data_studi_neu1$wert <- as.numeric(data_studi_neu1$wert)
# master_faecher_output1$wert <- as.numeric(master_faecher_output1$wert)
#
# master_faecher_output2 <- master_faecher_output1 %>%
#   filter(fach!= "Alle Fächer")%>%
#   select(-fachbereich)%>%
#   rename(fachbereich = fach)
#
# master_faecher_output2$fachbereich <- gsub("Ingenieurwissenschaft","Ingenieurwissenschaften",
#                                            master_faecher_output2$fachbereich)
#
#
#
#
# lele <- bind_rows(data_studi_neu1, master_faecher_output2)


#export(lele, "output/studierende_und_alle_fächer.xlsx")


# # Studierende.xlsx Appendix 2021 alte daten + Fächer---------
#
# stud_appendix <- master_append %>%
#   filter(fach=="Zusammen")%>%
#   select(-fach,-hinweise,-quelle,-bereich)%>%
#   rename(fachbereich= fachgruppe)
#
# stud_appendix$fachbereich <- ifelse(grepl("Zusammen", stud_appendix$fachbereich),"alle",stud_appendix$fachbereich)
#
# stud_appendix_1 <- stud_appendix %>%
#    filter(fachbereich==
#             "alle"| fachbereich=="Mathematik, Naturwissenschaften"
#           | fachbereich=="Ingenieurwissenschaften")
#
#
# stud_appendix_1$fachbereich <- gsub("Ingenieurwissenschaften", "ingenieur",stud_appendix_1$fachbereich)
# stud_appendix_1$fachbereich <- gsub("Mathematik, Naturwissenschaften", "mathe",stud_appendix_1$fachbereich)
#
# studi_appendix_fn <- stud_appendix_1
#
# studi_appendix_fn$indikator <- gsub("studienanfänger", "Studienanfänger:innen", studi_appendix_fn$indikator)
# studi_appendix_fn$indikator <- gsub("ausländer", "Ausländer:innen", studi_appendix_fn$indikator)
# studi_appendix_fn$indikator <- gsub("studierende", "Studierende", studi_appendix_fn$indikator)
#
# studi_appendix_fn$indikator <- gsub("Mathe", "Mathematik und Naturwissenschaften", studi_appendix_fn$indikator)
# studi_appendix_fn$indikator <- gsub("Ingenieur", "Ingenieurwissenschaften", studi_appendix_fn$indikator)
#
#
#
# # alte daten to append to
#
# studi_alt <- import("Studierende_alt_13_01.xlsx")
# #
# studi_alt <- studi_alt %>%
#   filter (hochschulform!="Uni", hochschulform != "FH")%>%
#   select(-hochschulform)%>%
#   rename(jahr=Jahr)
#
# studi_2021 <- bind_rows(studi_alt, studi_appendix_fn)
#
#
#
# # Ausländer noch herausnehmen???
# # Groß-/Kleinschreibung beachten
#
# studi_2021$bereich <- "Hochschule"
#
# studi_2021$indikator <- stringr::str_replace(studi_2021$indikator, "([[:alpha:]])", toupper)
#
# studi_2021 <- studi_2021 %>%
#   select(bereich, indikator,region,  jahr, fachbereich, anzeige_geschlecht, nur_lehramt, wert )
#
# studi_2021$fachbereich <- gsub("mathe", "Mathematik und Naturwissenschaften", studi_2021$fachbereich)
#
# studi_2021$fachbereich <- gsub("ingenieur", "Ingenieurwissenschaften", studi_2021$fachbereich)
#
# studi_2021$fachbereich <- gsub("alle", "Alle", studi_2021$fachbereich)
#
# colnames(studi_2021) [6] <- "geschlecht"
#
# colnames(studi_2021) [7] <- "lehramt"


# export(studi_2021, "output/Studierende_2023.xlsx")
