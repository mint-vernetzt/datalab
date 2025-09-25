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

akro <- "tko"
# pfad <- paste0("C:/Users/", akro,
#                "/OneDrive - Stifterverband/MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/02 Datenmaterial/01_Rohdaten/02_Alle Daten/")
# oder optional bei mir ist es anders:
pfad <- paste0("C:/Users/", akro ,"/OneDrive - Stifterverband/2_MINT-Lücke schließen/MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/02 Datenmaterial/01_Rohdaten/02_Alle Daten/")
pfad <- paste0("C:/Users/tko/OneDrive - Stifterverband/2_MINT-Lücke schließen/MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/02 Datenmaterial/01_Rohdaten/02_Alle Daten/")
# Studierende Domestisch ----

## studierende ----

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

## studierende_absolventen ----

#akronym pathing - Pfad
#pfad <- paste0("C:/Users/tko/OneDrive - Stifterverband/2_MINT-Lücke schließen/MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/02 Datenmaterial/01_Rohdaten/02_Alle Daten/")
pfad <- paste0("C:/Users/kbr/OneDrive - Stifterverband/MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/02 Datenmaterial/01_Rohdaten/02_Alle Daten/")


#Daten einlesen
Jahre <- as.character(2013:2022)
rohe_data_df <- data.frame()

for(i in 1:length(Jahre)){
  temporar <- readxl::read_xlsx(paste0(pfad, "DES070_Brunner_Absolventen_Land_FG_STB_2022.xlsx"), sheet = Jahre[i])
  temporar$jahr <- as.numeric(Jahre[i])
  temporar <- temporar[-(1:8),]
  rohe_data_df <- rbind(rohe_data_df, temporar)
}

temporar <- readxl::read_xlsx(paste0(pfad, "DES071_Brunner_Absolventinnen_Land_FG_STB_2023.xlsx"))
temporar$jahr <- 2023
temporar <- temporar[-(1:8),]
rohe_data_df <- rbind(rohe_data_df, temporar)

temporar <- readxl::read_xlsx(paste0(pfad, "DES074_Absolventinnen_Land_FG_STB_2024.xlsx"))
temporar$jahr <- 2024
temporar <- temporar[-(1:8),]
rohe_data_df <- rbind(rohe_data_df, temporar)

rm(temporar)


rohe_data_df <- rohe_data_df %>% select(-c(`Statistisches Bundesamt`, `...3`, `...5`))

colnames(rohe_data_df) <- c("bundesland", "faechergruppe", "studienbereich", "bestandene pruefung", "weiblich",
                            "auslaender", "bildungsauslaender", "lehramtspruefung zusammen", "Bachelorabschluss (Lehramt)",
                            "Masterabschluss (Lehramt)", "jahr")

rohe_data_df$bereich <- "hochschule"

#neue darstellung
long_df <- rohe_data_df %>%
  pivot_longer(cols = "bestandene pruefung":"Masterabschluss (Lehramt)",
               values_to = "wert", names_to = "indikator") %>%
  mutate(wert = as.numeric(wert))

bulas <- c(
  "Baden-Württemberg",
  "Bayern",
  "Berlin",
  "Brandenburg",
  "Bremen",
  "Hamburg",
  "Hessen",
  "Mecklenburg-Vorpommern",
  "Niedersachsen",
  "Nordrhein-Westfalen",
  "Rheinland-Pfalz",
  "Saarland",
  "Sachsen",
  "Sachsen-Anhalt",
  "Schleswig-Holstein",
  "Thüringen")

df <- long_df %>%
  mutate(
    bundesland = case_when(
      bundesland  %in% c("Ingesamt", "Insgesamt") ~ "Deutschland",
      T ~ bundesland
    ),
    faechergruppe = case_when(
      faechergruppe == "Zusammen" ~ "Gesamt",
      faechergruppe %in% c("Ingesamt", "Insgesamt") ~ "Gesamt",
      T ~ faechergruppe
    ),
    studienbereich = case_when(
      studienbereich == "Zusammen" ~ "Alle Fächer",
      studienbereich %in% c("Ingesamt", "Insgesamt") ~ "Alle Fächer",
      T ~ studienbereich
    ),
    wert = as.numeric(wert),
    jahr = as.numeric(jahr),
    geschlecht = case_when(
      str_detect(pattern="weiblich", indikator) ~ "Frauen",
      T ~ "Gesamt"
    ),
    indikator = case_when(
      indikator %in% c("bestandene pruefung", "weiblich") ~ "Absolvent:innen",
      indikator == "auslaender" ~ "ausländische Absolvent:innen",
      indikator == "bildungsauslaender" ~ "internationale Absolvent:innen",
      indikator == "lehramtspruefung zusammen" ~ "Absolvent:innen (Lehramt)",
      indikator == "Bachelorabschluss (Lehramt)" ~ "Bachelorabsolvent:innen (Lehramt)",
      indikator == "Masterabschluss (Lehramt)" ~ "Masterabsolvent:innen (Lehramt)",
      T ~ indikator
    ),
    mint_select = case_when(
      faechergruppe %in% c("Mathematik, Naturwissenschaften", "Ingenieurwissenschaften") ~
        "MINT",
      T ~ "Nicht MINT"
    ),
    typ = case_when(
      studienbereich == "Alle Fächer" ~ "Aggregat",
      studienbereich != "Alle Fächer" ~ "Einzelauswahl"
    )
  ) %>%
  filter(faechergruppe %in% c("Mathematik, Naturwissenschaften",
                              "Ingenieurwissenschaften") |
           studienbereich == "Alle Fächer")


#Aggregate
#mint
mint_agg <- df %>%
  filter(faechergruppe %in% c("Mathematik, Naturwissenschaften",
                              "Ingenieurwissenschaften") &
           studienbereich == "Alle Fächer",
         bundesland != "Deutschland") %>%
  group_by(bundesland, jahr, geschlecht, indikator) %>%
  summarise(wert = sum(wert)) %>%
  ungroup() %>%
  mutate(bereich = "hochschule",
         typ = "Aggregat",
         mint_select = "Nicht MINT",
         studienbereich = "Alle MINT-Fächer",
         faechergruppe = "MINT")


#nicht mint
nicht_mint_agg <- df %>%
  filter(!(faechergruppe %in% c("Mathematik, Naturwissenschaften",
                                "Ingenieurwissenschaften",
                                "Gesamt")) &
           studienbereich == "Alle Fächer",
         bundesland != "Deutschland") %>%
  group_by(bundesland, jahr, geschlecht, indikator) %>%
  summarise(wert = sum(wert)) %>%
  ungroup() %>%
  mutate(bereich = "hochschule",
         typ = "Aggregat",
         mint_select = "Nicht MINT",
         studienbereich = "Alle Nicht MINT-Fächer",
         faechergruppe = "Nicht MINT")

df_all <- rbind(df, mint_agg, nicht_mint_agg)

#deutschland
de_all <- df_all %>%
  filter(faechergruppe != "Gesamt") %>%
  group_by(jahr, faechergruppe, geschlecht, indikator, studienbereich, typ, mint_select) %>%
  summarise(wert = sum(wert)) %>%
  ungroup() %>%
  mutate(bereich = "hochschule",
         bundesland = "Deutschland")


df_all <- rbind(df_all, de_all)

#in Spalte fach statt "Alle Fächer" Fachbereich schreiben:
df_all$studienbereich <- ifelse(!(df_all$faechergruppe %in% c("Gesamt")) & df_all$studienbereich == "Alle Fächer",
                                df_all$faechergruppe, df_all$studienbereich)

# Korrektur in Bennenung
df_all <- df_all %>%
  mutate(studienbereich =case_when(
    studienbereich == "Mathematik, Naturwissenschaften allgemein" ~
      "allgemeine naturwissenschaftliche und mathematische Fächer",
    studienbereich == "Ingenieurwissenschaften allgemein" ~
      "Ingenieurwesen allgemein",
    T ~ studienbereich
  )
  )


#Gesamt-Geschlecht und korrekte Bennenung Ingenieurwissenschaften
df_all <- df_all %>%
  pivot_wider(names_from = geschlecht, values_from = wert, values_fill = list(wert = NA)) %>%
  mutate(Männer = ifelse(is.na(Frauen), NA, Gesamt - Frauen)) %>%
  pivot_longer(c("Männer", "Frauen", "Gesamt"), names_to = "geschlecht", values_to = "wert") %>%
  mutate(studienbereich = case_when(
    studienbereich == "Ingenieurwissenschaften" & jahr > 2014 ~ "Ingenieurwissenschaften (inkl. Informatik)",
    studienbereich == "Ingenieurwissenschaften" & jahr < 2015 ~ "Ingenieurwissenschaften (ohne Informatik)",
    TRUE ~ studienbereich
  )) %>%
  na.omit()

# Ost und Westdeutschland ergänzen
states_east_west <- list(west = c("Baden-Württemberg", "Bayern", "Bremen", "Hamburg",
                                  "Hessen", "Niedersachsen", "Nordrhein-Westfalen",
                                  "Rheinland-Pfalz", "Saarland", "Schleswig-Holstein"),
                         east = c("Brandenburg", "Mecklenburg-Vorpommern", "Sachsen",
                                  "Sachsen-Anhalt", "Thüringen", "Berlin"))

df_ew <- df_all
df_ew$dummy_west <- ifelse(df_ew$bundesland %in% states_east_west$west & df_ew$bundesland != "Deutschland", "Westdeutschland (o. Berlin)", NA)
df_ew$dummy_west <- ifelse(df_ew$bundesland %in% states_east_west$east & df_ew$bundesland != "Deutschland", "Ostdeutschland (inkl. Berlin)", df_ew$dummy_west)

df_ew <- df_ew %>% dplyr::group_by(jahr, geschlecht, indikator, faechergruppe, studienbereich, dummy_west
                                   ,bereich, typ, mint_select) %>%
  dplyr::summarise(wert = sum(wert, na.rm = T))


colnames(df_ew)[colnames(df_ew) == "dummy_west"] <- "bundesland"
df_ew <- df_ew[, colnames(df_all)]
df_ew <- na.omit(df_ew)
df_all <- rbind(df_all, df_ew)

#Anpassung Spalten-Namen
colnames(df_all) <- c("region", "fachbereich", "fach", "jahr", "bereich", "indikator",
                      "mint_select", "typ", "geschlecht", "wert")

studierende_absolventen<- df_all
## Export

#setwd("C:/Users/tko/OneDrive - Stifterverband/2_MINT-Lücke schließen/MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/02 Datenmaterial/02_data/data/")
setwd("C:/Users/kbr/OneDrive - Stifterverband/MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/02 Datenmaterial/02_data/data/")

#usethis::use_data(studierende_absolventen, overwrite = T)
save(studierende_absolventen, file = "studierende_absolventen.rda")

## studierende_detailliert ----

#-----------------------------------------------------------------------------------------------------------------#
# Ein Datensatz, der nur die Datensätze für mit mit Fächerunterscheidung beinhaltet für alle Indikatoren          #
#-----------------------------------------------------------------------------------------------------------------#

### Creating and Cleaning ----

#### aus git ----

#setwd("C:/Users/kab/Downloads/datalab/datalab/data-raw/raw")
akro <- "kbr"
pfad <- paste0("C:/Users/", akro,
               "/OneDrive - Stifterverband/MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/02 Datenmaterial/01_Rohdaten/02_Alle Daten/")

#turan
#pfad <- paste0("C:/Users/tko/OneDrive - Stifterverband/2_MINT-Lücke schließen/MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/02 Datenmaterial/01_Rohdaten/02_Alle Daten/")


# Daten einlesen
sheet <- as.character(2013:2021)
raw <- data.frame()


for(i in 1:length(sheet)){
  temp <- readxl::read_xlsx(paste0(pfad, "DES068_Brunner_Stud_Land_FG_STB_2013_2021.xlsx"),
                            sheet = sheet[i])
  temp$jahr <- as.numeric(sheet[i])
  raw <- rbind(raw, temp)
}

temp <- readxl::read_xlsx(paste0(pfad, "DES069_Brunner_Stud_Land_FG_STB_2022.xlsx"))
temp$jahr <- 2022
raw <- rbind(raw, temp)
rm(temp)

bulas <- c(
  "Baden-Württemberg",
  "Bayern",
  "Berlin",
  "Brandenburg",
  "Bremen",
  "Hamburg",
  "Hessen",
  "Mecklenburg-Vorpommern",
  "Niedersachsen",
  "Nordrhein-Westfalen",
  "Rheinland-Pfalz",
  "Saarland",
  "Sachsen",
  "Sachsen-Anhalt",
  "Schleswig-Holstein",
  "Thüringen")

# Aufbereitung Jahre ohne Hochschule

raw <- raw %>% select(-c(`...2`, `...3`, `...5`))

colnames(raw) <- c("region", "fachbereich", "fach", "gesamt", "weiblich",
                   "auslaender", "international", "lehramt", "lehramt_weiblich",
                   "gesamt_1hs", "weiblich_1hs", "auslaender_1hs", "international_1hs",
                   "gesamt_1fs", "weiblich_1fs", "jahr")

raw$bereich <- "hochschule"

raw <- raw %>%
  pivot_longer(cols = "gesamt":"weiblich_1fs",
               values_to = "wert", names_to = "indikator") %>%
  na.omit()

df <- raw %>%
  mutate(
    region = case_when(
      region == 1 ~ bulas[1],
      region == 2~ bulas[2],
      region == 3 ~ bulas[3],
      region == 4 ~ bulas[4],
      region == 5 ~ bulas[5],
      region == 6 ~ bulas[6],
      region == 7 ~ bulas[7],
      region == 8 ~ bulas[8],
      region == 9 ~ bulas[9],
      region == 10 ~ bulas[10],
      region == 11 ~ bulas[11],
      region == 12 ~ bulas[12],
      region == 13 ~ bulas[13],
      region == 14 ~ bulas[14],
      region == 15 ~ bulas[15],
      region == 16 ~ bulas[16],
      region == "~~" ~ "Deutschland"
    ),
    fachbereich = case_when(
      fachbereich == "Zusammen" ~ "Gesamt",
      fachbereich == "Insgesamt" ~ "Gesamt",
      T ~ fachbereich
    ),
    fach = case_when(
      fach == "Zusammen" ~ "Alle Fächer",
      fach == "Insgesamt" ~ "Alle Fächer",
      T ~ fach
    ),
    wert = as.numeric(wert),
    jahr = as.numeric(jahr),
    geschlecht = case_when(
      str_detect(pattern="weiblich", indikator) ~ "Frauen",
      T ~ "Gesamt"
    ),
    indikator = case_when(
      indikator %in% c("gesamt", "weiblich") ~ "Studierende",
      indikator == "auslaender" ~ "ausländische Studierende",
      indikator == "international" ~ "internationale Studierende",
      indikator == "auslaender_1hs" ~ "ausländische Studienanfänger:innen (1. Hochschulsemester)",
      indikator == "international_1hs" ~ "internationale Studienanfänger:innen (1. Hochschulsemester)",
      indikator %in% c("gesamt_1hs", "weiblich_1hs") ~ "Studienanfänger:innen (1. Hochschulsemester)",
      indikator %in% c("gesamt_1fs", "weiblich_1fs") ~ "Studienanfänger:innen (1. Fachsemester)",
      str_detect(pattern = "lehramt", indikator) ~ "Studierende (Lehramt)",
      T ~ indikator
    ),
    mint_select = case_when(
      fachbereich %in% c("Mathematik, Naturwissenschaften", "Ingenieurwissenschaften") ~
        "MINT",
      T ~ "Nicht MINT"
    ),
    typ = case_when(
      fach == "Alle Fächer" ~ "Aggregat",
      fach != "Alle Fächer" ~ "Einzelauswahl"
    )
  ) %>% filter(fachbereich %in% c("Mathematik, Naturwissenschaften",
                                  "Ingenieurwissenschaften") |
                 fach == "Alle Fächer")


# Teil mit Hochschule - ab 2023
temp2 <- readxl::read_xlsx(paste0(pfad, "DES072_Brunner_Stud_Hochschultyp_Land_FG_STB_2023.xlsx"))
temp2$jahr <- 2023

temp2 <- temp2 %>% select(-c(`...3`, `...4`, `...6`))

colnames(temp2) <- c("Hochschultyp","region", "fachbereich", "fach", "gesamt", "weiblich",
                     "auslaender", "international", "lehramt", "lehramt_weiblich",
                     "gesamt_1hs", "weiblich_1hs", "auslaender_1hs", "international_1hs",
                     "gesamt_1fs", "weiblich_1fs", "jahr")

temp2 <- temp2 %>%
  pivot_longer(
    cols = "gesamt":"weiblich_1fs",
    values_to = "wert",
    names_to = "indikator"
  ) %>%
  na.omit()


temp2 <- temp2 %>%
  mutate(
    region = case_when(
      region == 1 ~ bulas[1],
      region == 2~ bulas[2],
      region == 3 ~ bulas[3],
      region == 4 ~ bulas[4],
      region == 5 ~ bulas[5],
      region == 6 ~ bulas[6],
      region == 7 ~ bulas[7],
      region == 8 ~ bulas[8],
      region == 9 ~ bulas[9],
      region == 10 ~ bulas[10],
      region == 11 ~ bulas[11],
      region == 12 ~ bulas[12],
      region == 13 ~ bulas[13],
      region == 14 ~ bulas[14],
      region == 15 ~ bulas[15],
      region == 16 ~ bulas[16],
      region == "~~" ~ "Deutschland"
    ),
    fachbereich = case_when(
      fachbereich == "Zusammen" ~ "Gesamt",
      fachbereich == "Insgesamt" ~ "Gesamt",
      T ~ fachbereich
    ),
    fach = case_when(
      fach == "Zusammen" ~ "Alle Fächer",
      fach == "Insgesamt" ~ "Alle Fächer",
      T ~ fach
    ),
    wert = as.numeric(wert),
    jahr = as.numeric(jahr),
    geschlecht = case_when(
      str_detect(pattern="weiblich", indikator) ~ "Frauen",
      T ~ "Gesamt"
    ),
    indikator = case_when(
      indikator %in% c("gesamt", "weiblich") & Hochschultyp == "Hochschulen insgesamt" ~ "Studierende",
      indikator %in% c("gesamt", "weiblich") & Hochschultyp == "Universitäten" ~ "Studierende (Universität)",
      indikator %in% c("gesamt", "weiblich") & Hochschultyp == "Fachhochschulen" ~ "Studierende (Fachhochschule)",

      indikator == "auslaender" & Hochschultyp == "Hochschulen insgesamt" ~ "ausländische Studierende",
      indikator == "auslaender" & Hochschultyp == "Universitäten" ~ "ausländische Studierende (Universität)",
      indikator == "auslaender" & Hochschultyp == "Fachhochschulen" ~ "ausländische Studierende (Fachhochschule)",

      indikator == "international" & Hochschultyp == "Hochschulen insgesamt" ~ "internationale Studierende",
      indikator == "international" & Hochschultyp == "Universitäten" ~ "internationale Studierende (Universität)",
      indikator == "international" & Hochschultyp == "Fachhochschulen" ~ "internationale Studierende (Fachhochschule)",

      indikator == "auslaender_1hs" & Hochschultyp == "Hochschulen insgesamt" ~ "ausländische Studienanfänger:innen (1. Hochschulsemester)",
      indikator == "auslaender_1hs" & Hochschultyp == "Universitäten" ~ "ausländische Studienanfänger:innen (1. Hochschulsemester, Universität)",
      indikator == "auslaender_1hs" & Hochschultyp == "Fachhochschulen" ~ "ausländische Studienanfänger:innen (1. Hochschulsemester, Fachhochschule)",

      indikator == "international_1hs" & Hochschultyp == "Hochschulen insgesamt" ~ "internationale Studienanfänger:innen (1. Hochschulsemester)",
      indikator == "international_1hs" & Hochschultyp == "Universitäten" ~ "internationale Studienanfänger:innen (1. Hochschulsemester, Universität)",
      indikator == "international_1hs" & Hochschultyp == "Fachhochschulen" ~ "internationale Studienanfänger:innen (1. Hochschulsemester, Fachhochschule)",

      indikator %in% c("gesamt_1hs", "weiblich_1hs") & Hochschultyp == "Hochschulen insgesamt" ~ "Studienanfänger:innen (1. Hochschulsemester)",
      indikator %in% c("gesamt_1hs", "weiblich_1hs") & Hochschultyp == "Universitäten" ~ "Studienanfänger:innen (1. Hochschulsemester, Universität)",
      indikator %in% c("gesamt_1hs", "weiblich_1hs") & Hochschultyp == "Fachhochschulen" ~ "Studienanfänger:innen (1. Hochschulsemester, Fachhochschule)",

      indikator %in% c("gesamt_1fs", "weiblich_1fs") & Hochschultyp == "Hochschulen insgesamt" ~ "Studiumsanfänger:innen (1. Fachsemester)",
      indikator %in% c("gesamt_1fs", "weiblich_1fs") & Hochschultyp == "Universitäten" ~ "Studiumsanfänger:innen (1. Fachsemester, Universität)",
      indikator %in% c("gesamt_1fs", "weiblich_1fs") & Hochschultyp == "Fachhochschulen" ~ "Studiumsanfänger:innen (1. Fachsemester, Fachhochschule)",

      str_detect(indikator, "lehramt") & Hochschultyp == "Hochschulen insgesamt" ~ "Studierende (Lehramt)",
      str_detect(indikator, "lehramt") & Hochschultyp == "Universitäten" ~ "Studierende (Lehramt, Universität)",
      str_detect(indikator, "lehramt") & Hochschultyp == "Fachhochschulen" ~ "Studierende (Lehramt, Fachhochschule)",

      TRUE ~ indikator  # Default für Fälle, die nicht explizit behandelt werden
    ),
    mint_select = case_when(
      fachbereich %in% c("Mathematik, Naturwissenschaften", "Ingenieurwissenschaften") ~
        "MINT",
      T ~ "Nicht MINT"
    ),
    typ = case_when(
      fach == "Alle Fächer" ~ "Aggregat",
      fach != "Alle Fächer" ~ "Einzelauswahl"
    )
  )

temp2 <- temp2 %>% select(-c(`Hochschultyp`))

temp2 <- temp2 %>%
  mutate(bereich = "hochschule")

temp2 <- temp2 %>%
  relocate(bereich, .after = jahr)

# fehlende Werte für 'Hochschulen Insgesamt' berechnen
fach_zsm <- temp2 %>%
  filter(indikator %in% c("Studierende",
                          "ausländische Studierende",
                          "internationale Studierende",
                          "Studierende (Lehramt)",
                          "Studienanfänger:innen (1. Hochschulsemester)",
                          "ausländische Studienanfänger:innen (1. Hochschulsemester)",
                          "internationale Studienanfänger:innen (1. Hochschulsemester)",
                          "Studiumsanfänger:innen (1. Fachsemester)" ),
         region != "Deutschland") %>%
  group_by(region, jahr, geschlecht, fachbereich, indikator, mint_select,
           bereich) %>%
  summarise(wert = sum(wert, na.rm = FALSE)) %>%
  ungroup() %>%
  mutate(typ = "Aggregat",
         fach = "Alle Fächer")

temp2 <- rbind(temp2, fach_zsm)
temp2 <- temp2 %>%
  filter(fachbereich %in% c("Mathematik, Naturwissenschaften",
                            "Ingenieurwissenschaften") |
           fach == "Alle Fächer")


# 2024
temp24 <- readxl::read_xlsx(paste0(pfad, "DES073_Stud_Hochschultyp_Land_FG_STB_2024.xlsx"))
temp24$jahr <- 2024

temp24 <- temp24 %>% select(-c(`...3`, `...4`, `...6`))

colnames(temp24) <- c("Hochschultyp","region", "fachbereich", "fach", "gesamt", "weiblich",
                     "auslaender", "international", "lehramt", "lehramt_weiblich",
                     "gesamt_1hs", "weiblich_1hs", "auslaender_1hs", "international_1hs",
                     "gesamt_1fs", "weiblich_1fs", "jahr")

temp24 <- temp24 %>%
  pivot_longer(
    cols = "gesamt":"weiblich_1fs",
    values_to = "wert",
    names_to = "indikator"
  ) %>%
  na.omit()


temp24 <- temp24 %>%
  mutate(
    region = case_when(
      region == 1 ~ bulas[1],
      region == 2~ bulas[2],
      region == 3 ~ bulas[3],
      region == 4 ~ bulas[4],
      region == 5 ~ bulas[5],
      region == 6 ~ bulas[6],
      region == 7 ~ bulas[7],
      region == 8 ~ bulas[8],
      region == 9 ~ bulas[9],
      region == 10 ~ bulas[10],
      region == 11 ~ bulas[11],
      region == 12 ~ bulas[12],
      region == 13 ~ bulas[13],
      region == 14 ~ bulas[14],
      region == 15 ~ bulas[15],
      region == 16 ~ bulas[16],
      region == "~~" ~ "Deutschland"
    ),
    fachbereich = case_when(
      fachbereich == "Zusammen" ~ "Gesamt",
      fachbereich == "Insgesamt" ~ "Gesamt",
      T ~ fachbereich
    ),
    fach = case_when(
      fach == "Zusammen" ~ "Alle Fächer",
      fach == "Insgesamt" ~ "Alle Fächer",
      T ~ fach
    ),
    wert = as.numeric(wert),
    jahr = as.numeric(jahr),
    geschlecht = case_when(
      str_detect(pattern="weiblich", indikator) ~ "Frauen",
      T ~ "Gesamt"
    ),
    indikator = case_when(
      indikator %in% c("gesamt", "weiblich") & Hochschultyp == "Hochschulen insgesamt" ~ "Studierende",
      indikator %in% c("gesamt", "weiblich") & Hochschultyp == "Universitäten" ~ "Studierende (Universität)",
      indikator %in% c("gesamt", "weiblich") & Hochschultyp == "Fachhochschulen" ~ "Studierende (Fachhochschule)",

      indikator == "auslaender" & Hochschultyp == "Hochschulen insgesamt" ~ "ausländische Studierende",
      indikator == "auslaender" & Hochschultyp == "Universitäten" ~ "ausländische Studierende (Universität)",
      indikator == "auslaender" & Hochschultyp == "Fachhochschulen" ~ "ausländische Studierende (Fachhochschule)",

      indikator == "international" & Hochschultyp == "Hochschulen insgesamt" ~ "internationale Studierende",
      indikator == "international" & Hochschultyp == "Universitäten" ~ "internationale Studierende (Universität)",
      indikator == "international" & Hochschultyp == "Fachhochschulen" ~ "internationale Studierende (Fachhochschule)",

      indikator == "auslaender_1hs" & Hochschultyp == "Hochschulen insgesamt" ~ "ausländische Studienanfänger:innen (1. Hochschulsemester)",
      indikator == "auslaender_1hs" & Hochschultyp == "Universitäten" ~ "ausländische Studienanfänger:innen (1. Hochschulsemester, Universität)",
      indikator == "auslaender_1hs" & Hochschultyp == "Fachhochschulen" ~ "ausländische Studienanfänger:innen (1. Hochschulsemester, Fachhochschule)",

      indikator == "international_1hs" & Hochschultyp == "Hochschulen insgesamt" ~ "internationale Studienanfänger:innen (1. Hochschulsemester)",
      indikator == "international_1hs" & Hochschultyp == "Universitäten" ~ "internationale Studienanfänger:innen (1. Hochschulsemester, Universität)",
      indikator == "international_1hs" & Hochschultyp == "Fachhochschulen" ~ "internationale Studienanfänger:innen (1. Hochschulsemester, Fachhochschule)",

      indikator %in% c("gesamt_1hs", "weiblich_1hs") & Hochschultyp == "Hochschulen insgesamt" ~ "Studienanfänger:innen (1. Hochschulsemester)",
      indikator %in% c("gesamt_1hs", "weiblich_1hs") & Hochschultyp == "Universitäten" ~ "Studienanfänger:innen (1. Hochschulsemester, Universität)",
      indikator %in% c("gesamt_1hs", "weiblich_1hs") & Hochschultyp == "Fachhochschulen" ~ "Studienanfänger:innen (1. Hochschulsemester, Fachhochschule)",

      indikator %in% c("gesamt_1fs", "weiblich_1fs") & Hochschultyp == "Hochschulen insgesamt" ~ "Studiumsanfänger:innen (1. Fachsemester)",
      indikator %in% c("gesamt_1fs", "weiblich_1fs") & Hochschultyp == "Universitäten" ~ "Studiumsanfänger:innen (1. Fachsemester, Universität)",
      indikator %in% c("gesamt_1fs", "weiblich_1fs") & Hochschultyp == "Fachhochschulen" ~ "Studiumsanfänger:innen (1. Fachsemester, Fachhochschule)",

      str_detect(indikator, "lehramt") & Hochschultyp == "Hochschulen insgesamt" ~ "Studierende (Lehramt)",
      str_detect(indikator, "lehramt") & Hochschultyp == "Universitäten" ~ "Studierende (Lehramt, Universität)",
      str_detect(indikator, "lehramt") & Hochschultyp == "Fachhochschulen" ~ "Studierende (Lehramt, Fachhochschule)",

      TRUE ~ indikator  # Default für Fälle, die nicht explizit behandelt werden
    ),
    mint_select = case_when(
      fachbereich %in% c("Mathematik, Naturwissenschaften", "Ingenieurwissenschaften") ~
        "MINT",
      T ~ "Nicht MINT"
    ),
    typ = case_when(
      fach == "Alle Fächer" ~ "Aggregat",
      fach != "Alle Fächer" ~ "Einzelauswahl"
    )
  )

temp24 <- temp24 %>% select(-c(`Hochschultyp`))

temp24 <- temp24 %>%
  mutate(bereich = "hochschule")

temp24 <- temp24 %>%
  relocate(bereich, .after = jahr)

# fehlende Werte für 'Hochschulen Insgesamt' berechnen
fach_zsm24 <- temp24 %>%
  filter(indikator %in% c("Studierende",
                          "ausländische Studierende",
                          "internationale Studierende",
                          "Studierende (Lehramt)",
                          "Studienanfänger:innen (1. Hochschulsemester)",
                          "ausländische Studienanfänger:innen (1. Hochschulsemester)",
                          "internationale Studienanfänger:innen (1. Hochschulsemester)",
                          "Studiumsanfänger:innen (1. Fachsemester)" ),
         region != "Deutschland") %>%
  group_by(region, jahr, geschlecht, fachbereich, indikator, mint_select,
           bereich) %>%
  summarise(wert = sum(wert, na.rm = FALSE)) %>%
  ungroup() %>%
  mutate(typ = "Aggregat",
         fach = "Alle Fächer")

temp24 <- rbind(temp24, fach_zsm24)
temp24 <- temp24 %>%
  filter(fachbereich %in% c("Mathematik, Naturwissenschaften",
                            "Ingenieurwissenschaften") |
           fach == "Alle Fächer")



# mit und ohne Hochschule zusammen

df <- rbind(df, temp2, temp24)
rm(temp2, temp24, fach_zsm, fach_zsm24, raw)

# Aggregate Berechnen
mint_agg <- df %>%
  filter(fachbereich %in% c("Mathematik, Naturwissenschaften",
                            "Ingenieurwissenschaften") &
           fach == "Alle Fächer",
         region != "Deutschland") %>%
  group_by(region, jahr, geschlecht, indikator) %>%
  summarise(wert = sum(wert, na.rm = FALSE)) %>%
  ungroup() %>%
  mutate(bereich = "hochschule",
         typ = "Aggregat",
         mint_select = "Nicht MINT",
         fach = "Alle MINT-Fächer",
         fachbereich = "MINT")

nicht_mint_agg <- df %>%
  filter(!(fachbereich %in% c("Mathematik, Naturwissenschaften",
                              "Ingenieurwissenschaften",
                              "Gesamt")) &
           fach == "Alle Fächer",
         region != "Deutschland") %>%
  group_by(region, jahr, geschlecht, indikator) %>%
  summarise(wert = sum(wert)) %>%
  ungroup() %>%
  mutate(bereich = "hochschule",
         typ = "Aggregat",
         mint_select = "Nicht MINT",
         fach = "Alle Nicht MINT-Fächer",
         fachbereich = "Nicht MINT")

df_all <- rbind(df, mint_agg, nicht_mint_agg)

#Deutschland berechnen
de_all <- df_all %>%
  filter(fachbereich != "Gesamt") %>%
  group_by(jahr, fachbereich, geschlecht, indikator, fach, typ, mint_select) %>%
  summarise(wert = sum(wert)) %>%
  ungroup() %>%
  mutate(bereich = "hochschule",
         region = "Deutschland")


df_all <- rbind(df_all, de_all)


df_all$fach <- ifelse(!(df_all$fachbereich %in% c("Gesamt")) & df_all$fach == "Alle Fächer",
                      df_all$fachbereich, df_all$fach)

df_all <- df_all %>%
  mutate(fach =case_when(
    fach == "Mathematik, Naturwissenschaften allgemein" ~
      "allgemeine naturwissenschaftliche und mathematische Fächer",
    fach == "Ingenieurwissenschaften allgemein" ~
      "Ingenieurwesen allgemein",
    T ~ fach
  )
  )


# Entferne doppelte Zeilen
df_all<- df_all %>%
  distinct(region, fachbereich, fach, jahr, bereich, indikator, mint_select, typ, geschlecht, wert)

# df_all <- df_all %>%
#   group_by(region, fachbereich, jahr, indikator, mint_select, geschlecht) %>%
#   summarise(wert = sum(wert, na.rm = TRUE), .groups = "drop")


df_all <- df_all %>%
  pivot_wider(names_from = geschlecht, values_from = wert, values_fill = list(wert = NA)) %>%
  mutate(Männer = ifelse(is.na(Frauen), NA, Gesamt - Frauen)) %>%
  pivot_longer(cols = c("Männer", "Frauen", "Gesamt"), names_to = "geschlecht", values_to = "wert") %>%
  mutate(fach = case_when(
    fach == "Ingenieurwissenschaften" & jahr > 2014 ~ "Ingenieurwissenschaften (inkl. Informatik)",
    fach == "Ingenieurwissenschaften" & jahr < 2015 ~ "Ingenieurwissenschaften (ohne Informatik)",
    TRUE ~ fach
  )) %>%
  na.omit()

states_east_west <- list(west = c("Baden-Württemberg", "Bayern", "Bremen", "Hamburg",
                                  "Hessen", "Niedersachsen", "Nordrhein-Westfalen",
                                  "Rheinland-Pfalz", "Saarland", "Schleswig-Holstein"),
                         east = c("Brandenburg", "Mecklenburg-Vorpommern", "Sachsen",
                                  "Sachsen-Anhalt", "Thüringen", "Berlin"))

df_ew <- df_all
df_ew$dummy_west <- ifelse(df_ew$region %in% states_east_west$west & df_ew$region != "Deutschland", "Westdeutschland (o. Berlin)", NA)
df_ew$dummy_west <- ifelse(df_ew$region %in% states_east_west$east & df_ew$region != "Deutschland", "Ostdeutschland (inkl. Berlin)", df_ew$dummy_west)

df_ew <- df_ew %>% dplyr::group_by(jahr, geschlecht, indikator, fachbereich, fach, dummy_west
                                   ,bereich, typ, mint_select) %>%
  dplyr::summarise(wert = sum(wert, na.rm = T))

colnames(df_ew)[colnames(df_ew) == "dummy_west"] <- "region"

df_ew <- df_ew[, colnames(df_all)]

df_ew <- na.omit(df_ew)
df_all <- rbind(df_all, df_ew)



##neu
ingenieur_incl_informatik <- df_all %>%
  filter(fach == "Ingenieurwissenschaften (inkl. Informatik)") %>%
  select(region, jahr, geschlecht, indikator, wert)

informatik <- df_all %>%
  filter(fach == "Informatik") %>%
  select(region, jahr, geschlecht, indikator, wert)

ingenieur_ohne_informatik <- ingenieur_incl_informatik %>%
  left_join(informatik, by = c("region", "jahr", "geschlecht", "indikator"), suffix = c(".ingenieur", ".informatik")) %>%
  mutate(wert = wert.ingenieur - wert.informatik) %>%
  select(region, jahr, geschlecht, indikator, wert) %>%
  mutate(fach = "Ingenieurwissenschaften (ohne Informatik)")

df_all <- bind_rows(df_all, ingenieur_ohne_informatik)

# Entferne doppelte Zeilen
df_all<- df_all %>%
  distinct(region, fachbereich, fach, jahr, bereich, indikator, mint_select, typ, geschlecht, wert)

studierende_detailliert <- df_all

load("C:/Users/tko/OneDrive - Stifterverband/2_MINT-Lücke schließen/MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/02 Datenmaterial/02_data/data/studierende_absolventen.rda")
# studierende_absolventen aktualiseren bevor Anhängen!

studierende_detailliert <- rbind(studierende_detailliert, studierende_absolventen)

## Export

#setwd("C:/Users/tko/OneDrive - Stifterverband/2_MINT-Lücke schließen/MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/02 Datenmaterial/02_data/data/")
setwd("C:/Users/kbr/OneDrive - Stifterverband/MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/02 Datenmaterial/02_data/data/")

save(studierende_detailliert, file = "studierende_detailliert.rda")


#################BEI absolventen auch noch studierende!

#### alt - kann raus ----

###einfügt neue daten 2023 :) tko 17.10.2024
### Creating and Cleaning ----

#setwd("C:/Users/kab/Downloads/datalab/datalab/data-raw/raw")
akro <- "kbr"
pfad <- paste0("C:/Users/", akro,
               "/OneDrive - Stifterverband/MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/02 Datenmaterial/01_Rohdaten/02_Alle Daten/")

#akro turan
pfad <- paste0("C:/Users/tko/OneDrive - Stifterverband/2_MINT-Lücke schließen/MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/02 Datenmaterial/01_Rohdaten/02_Alle Daten/")

# Daten einlesen
sheet <- as.character(2013:2021)
raw <- data.frame()

for(i in 1:length(sheet)){
  temp <- readxl::read_xlsx(paste0(pfad, "DES068_Brunner_Stud_Land_FG_STB_2013_2021.xlsx"),
                            sheet = sheet[i])
  temp$jahr <- as.numeric(sheet[i])
  raw <- rbind(raw, temp)
}

temp <- readxl::read_xlsx(paste0(pfad, "DES069_Brunner_Stud_Land_FG_STB_2022.xlsx"))
temp$jahr <- 2022
raw <- rbind(raw, temp)
rm(temp)



#neu hinzugekommen 17.10.2024
temp2 <- readxl::read_xlsx(paste0("C:/Users/tko/OneDrive - Stifterverband/2_MINT-Lücke schließen/MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/02 Datenmaterial/01_Rohdaten/01_Eingang/Destatis/Datenlieferung_24_10_Absolv_Studis2023/", "Brunner_Stud_Hochschultyp_Land_FG_STB_2023.xlsx"))
temp2$jahr <- 2023
###

temp2 <- temp2 %>% select(-c(`...3`, `...4`, `...6`))

colnames(temp2) <- c("Hochschultyp","region", "fachbereich", "fach", "gesamt", "weiblich",
                     "auslaender", "international", "lehramt", "lehramt_weiblich",
                     "gesamt_1hs", "weiblich_1hs", "auslaender_1hs", "international_1hs",
                     "gesamt_1fs", "weiblich_1fs", "jahr")

bulas <- c(
  "Baden-Württemberg",
  "Bayern",
  "Berlin",
  "Brandenburg",
  "Bremen",
  "Hamburg",
  "Hessen",
  "Mecklenburg-Vorpommern",
  "Niedersachsen",
  "Nordrhein-Westfalen",
  "Rheinland-Pfalz",
  "Saarland",
  "Sachsen",
  "Sachsen-Anhalt",
  "Schleswig-Holstein",
  "Thüringen")

# Aufbereitung Jahre ohne Hochschule

raw <- raw %>% select(-c(`...2`, `...3`, `...5`))

colnames(raw) <- c("region", "fachbereich", "fach", "gesamt", "weiblich",
                   "auslaender", "international", "lehramt", "lehramt_weiblich",
                   "gesamt_1hs", "weiblich_1hs", "auslaender_1hs", "international_1hs",
                   "gesamt_1fs", "weiblich_1fs", "jahr")

raw$bereich <- "hochschule"

raw <- raw %>%
  pivot_longer(cols = "gesamt":"weiblich_1fs",
               values_to = "wert", names_to = "indikator") %>%
  na.omit()

df <- raw %>%
  mutate(
    region = case_when(
      region == 1 ~ bulas[1],
      region == 2~ bulas[2],
      region == 3 ~ bulas[3],
      region == 4 ~ bulas[4],
      region == 5 ~ bulas[5],
      region == 6 ~ bulas[6],
      region == 7 ~ bulas[7],
      region == 8 ~ bulas[8],
      region == 9 ~ bulas[9],
      region == 10 ~ bulas[10],
      region == 11 ~ bulas[11],
      region == 12 ~ bulas[12],
      region == 13 ~ bulas[13],
      region == 14 ~ bulas[14],
      region == 15 ~ bulas[15],
      region == 16 ~ bulas[16],
      region == "~~" ~ "Deutschland"
    ),
    fachbereich = case_when(
      fachbereich == "Zusammen" ~ "Gesamt",
      fachbereich == "Insgesamt" ~ "Gesamt",
      T ~ fachbereich
    ),
    fach = case_when(
      fach == "Zusammen" ~ "Alle Fächer",
      fach == "Insgesamt" ~ "Alle Fächer",
      T ~ fach
    ),
    wert = as.numeric(wert),
    jahr = as.numeric(jahr),
    geschlecht = case_when(
      str_detect(pattern="weiblich", indikator) ~ "Frauen",
      T ~ "Gesamt"
    ),
    indikator = case_when(
      indikator %in% c("gesamt", "weiblich") ~ "Studierende",
      indikator == "auslaender" ~ "ausländische Studierende",
      indikator == "international" ~ "internationale Studierende",
      indikator == "auslaender_1hs" ~ "ausländische Studienanfänger:innen (1. Hochschulsemester)",
      indikator == "international_1hs" ~ "internationale Studienanfänger:innen (1. Hochschulsemester)",
      indikator %in% c("gesamt_1hs", "weiblich_1hs") ~ "Studienanfänger:innen (1. Hochschulsemester)",
      indikator %in% c("gesamt_1fs", "weiblich_1fs") ~ "Studienanfänger:innen (1. Fachsemester)",
      str_detect(pattern = "lehramt", indikator) ~ "Studierende (Lehramt)",
      T ~ indikator
    ),
    mint_select = case_when(
      fachbereich %in% c("Mathematik, Naturwissenschaften", "Ingenieurwissenschaften") ~
        "MINT",
      T ~ "Nicht MINT"
    ),
    typ = case_when(
      fach == "Alle Fächer" ~ "Aggregat",
      fach != "Alle Fächer" ~ "Einzelauswahl"
    )
  ) %>% filter(fachbereich %in% c("Mathematik, Naturwissenschaften",
                            "Ingenieurwissenschaften") |
           fach == "Alle Fächer")


# Teil mit Hochschule
temp2 <- readxl::read_xlsx(paste0(pfad, "DES072_Brunner_Stud_Hochschultyp_Land_FG_STB_2023.xlsx"))
temp2$jahr <- 2023

temp2 <- temp2 %>% select(-c(`...3`, `...4`, `...6`))

colnames(temp2) <- c("Hochschultyp","region", "fachbereich", "fach", "gesamt", "weiblich",
                     "auslaender", "international", "lehramt", "lehramt_weiblich",
                     "gesamt_1hs", "weiblich_1hs", "auslaender_1hs", "international_1hs",
                     "gesamt_1fs", "weiblich_1fs", "jahr")


raw <- raw %>%
  pivot_longer(cols = "gesamt":"weiblich_1fs",
               values_to = "wert", names_to = "indikator") %>%

  na.omit()

temp2 <- temp2 %>%
  mutate(
    region = case_when(
      region == 1 ~ bulas[1],
      region == 2~ bulas[2],
      region == 3 ~ bulas[3],
      region == 4 ~ bulas[4],
      region == 5 ~ bulas[5],
      region == 6 ~ bulas[6],
      region == 7 ~ bulas[7],
      region == 8 ~ bulas[8],
      region == 9 ~ bulas[9],
      region == 10 ~ bulas[10],
      region == 11 ~ bulas[11],
      region == 12 ~ bulas[12],
      region == 13 ~ bulas[13],
      region == 14 ~ bulas[14],
      region == 15 ~ bulas[15],
      region == 16 ~ bulas[16],
      region == "~~" ~ "Deutschland"
    ),
    fachbereich = case_when(
      fachbereich == "Zusammen" ~ "Gesamt",
      fachbereich == "Insgesamt" ~ "Gesamt",
      T ~ fachbereich
    ),
    fach = case_when(
      fach == "Zusammen" ~ "Alle Fächer",
      fach == "Insgesamt" ~ "Alle Fächer",
      T ~ fach
    ),
    wert = as.numeric(wert),
    jahr = as.numeric(jahr),
    geschlecht = case_when(
      str_detect(pattern="weiblich", indikator) ~ "Frauen",
      T ~ "Gesamt"
    ),
    indikator = case_when(
      indikator %in% c("gesamt", "weiblich") & Hochschultyp == "Hochschulen insgesamt" ~ "Studierende",
      indikator %in% c("gesamt", "weiblich") & Hochschultyp == "Universitäten" ~ "Studierende (Universität)",
      indikator %in% c("gesamt", "weiblich") & Hochschultyp == "Fachhochschulen" ~ "Studierende (Fachhochschule)",

      indikator == "auslaender" & Hochschultyp == "Hochschulen insgesamt" ~ "ausländische Studierende",
      indikator == "auslaender" & Hochschultyp == "Universitäten" ~ "ausländische Studierende (Universität)",
      indikator == "auslaender" & Hochschultyp == "Fachhochschulen" ~ "ausländische Studierende (Fachhochschule)",

      indikator == "international" & Hochschultyp == "Hochschulen insgesamt" ~ "internationale Studierende",
      indikator == "international" & Hochschultyp == "Universitäten" ~ "internationale Studierende (Universität)",
      indikator == "international" & Hochschultyp == "Fachhochschulen" ~ "internationale Studierende (Fachhochschule)",

      indikator == "auslaender_1hs" & Hochschultyp == "Hochschulen insgesamt" ~ "ausländische Studienanfänger:innen (1. Hochschulsemester)",
      indikator == "auslaender_1hs" & Hochschultyp == "Universitäten" ~ "ausländische Studienanfänger:innen (1. Hochschulsemester, Universität)",
      indikator == "auslaender_1hs" & Hochschultyp == "Fachhochschulen" ~ "ausländische Studienanfänger:innen (1. Hochschulsemester, Fachhochschule)",

      indikator == "international_1hs" & Hochschultyp == "Hochschulen insgesamt" ~ "internationale Studienanfänger:innen (1. Hochschulsemester)",
      indikator == "international_1hs" & Hochschultyp == "Universitäten" ~ "internationale Studienanfänger:innen (1. Hochschulsemester, Universität)",
      indikator == "international_1hs" & Hochschultyp == "Fachhochschulen" ~ "internationale Studienanfänger:innen (1. Hochschulsemester, Fachhochschule)",

      indikator %in% c("gesamt_1hs", "weiblich_1hs") & Hochschultyp == "Hochschulen insgesamt" ~ "Studienanfänger:innen (1. Hochschulsemester)",
      indikator %in% c("gesamt_1hs", "weiblich_1hs") & Hochschultyp == "Universitäten" ~ "Studienanfänger:innen (1. Hochschulsemester, Universität)",
      indikator %in% c("gesamt_1hs", "weiblich_1hs") & Hochschultyp == "Fachhochschulen" ~ "Studienanfänger:innen (1. Hochschulsemester, Fachhochschule)",

      indikator %in% c("gesamt_1fs", "weiblich_1fs") & Hochschultyp == "Hochschulen insgesamt" ~ "Studiumsanfänger:innen (1. Fachsemester)",
      indikator %in% c("gesamt_1fs", "weiblich_1fs") & Hochschultyp == "Universitäten" ~ "Studiumsanfänger:innen (1. Fachsemester, Universität)",
      indikator %in% c("gesamt_1fs", "weiblich_1fs") & Hochschultyp == "Fachhochschulen" ~ "Studiumsanfänger:innen (1. Fachsemester, Fachhochschule)",

      str_detect(indikator, "lehramt") & Hochschultyp == "Hochschulen insgesamt" ~ "Studierende (Lehramt)",
      str_detect(indikator, "lehramt") & Hochschultyp == "Universitäten" ~ "Studierende (Lehramt, Universität)",
      str_detect(indikator, "lehramt") & Hochschultyp == "Fachhochschulen" ~ "Studierende (Lehramt, Fachhochschule)",

      TRUE ~ indikator  # Default für Fälle, die nicht explizit behandelt werden
    ),
    mint_select = case_when(
      fachbereich %in% c("Mathematik, Naturwissenschaften", "Ingenieurwissenschaften") ~
        "MINT",
      T ~ "Nicht MINT"
    ),
    typ = case_when(
      fach == "Alle Fächer" ~ "Aggregat",
      fach != "Alle Fächer" ~ "Einzelauswahl"
    )
  )

temp2 <- temp2 %>% select(-c(`Hochschultyp`))

temp2 <- temp2 %>%
  mutate(bereich = "hochschule")

temp2 <- temp2 %>%
  relocate(bereich, .after = jahr)

# fehlende Werte für 'Hochschulen Insgesamt' berechnen
fach_zsm <- temp2 %>%
  filter(indikator %in% c("Studierende",
                          "ausländische Studierende",
                          "internationale Studierende",
                          "Studierende (Lehramt)",
                          "Studienanfänger:innen (1. Hochschulsemester)",
                          "ausländische Studienanfänger:innen (1. Hochschulsemester)",
                          "internationale Studienanfänger:innen (1. Hochschulsemester)",
                          "Studiumsanfänger:innen (1. Fachsemester)" ),
         region != "Deutschland") %>%
  group_by(region, jahr, geschlecht, fachbereich, indikator, mint_select,
           bereich) %>%
  summarise(wert = sum(wert, na.rm = FALSE)) %>%
  ungroup() %>%
  mutate(typ = "Aggregat",
         fach = "Alle Fächer")

temp2 <- rbind(temp2, fach_zsm)
temp2 <- temp2 %>%
  filter(fachbereich %in% c("Mathematik, Naturwissenschaften",
                            "Ingenieurwissenschaften") |
           fach == "Alle Fächer")

# mit und ohne Hochschule zusammen

df <- rbind(df, temp2)
rm(temp2)

# Aggregate Berechnen
mint_agg <- df %>%
  filter(fachbereich %in% c("Mathematik, Naturwissenschaften",
                            "Ingenieurwissenschaften") &
           fach == "Alle Fächer",
         region != "Deutschland") %>%
  group_by(region, jahr, geschlecht, indikator) %>%
  summarise(wert = sum(wert, na.rm = FALSE)) %>%
  ungroup() %>%
  mutate(bereich = "hochschule",
         typ = "Aggregat",
         mint_select = "Nicht MINT",
         fach = "Alle MINT-Fächer",
         fachbereich = "MINT")

nicht_mint_agg <- df %>%
  filter(!(fachbereich %in% c("Mathematik, Naturwissenschaften",
                              "Ingenieurwissenschaften",
                              "Gesamt")) &
           fach == "Alle Fächer",
         region != "Deutschland") %>%
  group_by(region, jahr, geschlecht, indikator) %>%
  summarise(wert = sum(wert)) %>%
  ungroup() %>%
  mutate(bereich = "hochschule",
         typ = "Aggregat",
         mint_select = "Nicht MINT",
         fach = "Alle Nicht MINT-Fächer",
         fachbereich = "Nicht MINT")

df_all <- rbind(df, mint_agg, nicht_mint_agg)

#Deutschland berechnen
de_all <- df_all %>%
  filter(fachbereich != "Gesamt") %>%
  group_by(jahr, fachbereich, geschlecht, indikator, fach, typ, mint_select) %>%
  summarise(wert = sum(wert)) %>%
  ungroup() %>%
  mutate(bereich = "hochschule",
         region = "Deutschland")


df_all <- rbind(df_all, de_all)


df_all$fach <- ifelse(!(df_all$fachbereich %in% c("Gesamt")) & df_all$fach == "Alle Fächer",
                      df_all$fachbereich, df_all$fach)

df_all <- df_all %>%
  mutate(fach =case_when(
    fach == "Mathematik, Naturwissenschaften allgemein" ~
      "allgemeine naturwissenschaftliche und mathematische Fächer",
    fach == "Ingenieurwissenschaften allgemein" ~
      "Ingenieurwesen allgemein",
    T ~ fach
  )
  )

# df_all <- df_all %>%
#   filter(!fach %in% c("Weitere naturwissenschaftliche und mathematische Fächer",
#                       "Außerhalb der Studienbereichsgliederung/Sonstige Fächer",
#                       "Weitere ingenieurwissenschaftliche Fächer"))

# Dublikate entfernen
df_all <- df_all %>%
  group_by(region, fachbereich, fach, jahr, bereich, indikator, mint_select, typ, geschlecht) %>%
  summarise(wert = sum(wert, na.rm = TRUE), .groups = "drop")

df_all <- df_all %>%
  pivot_wider(names_from = geschlecht, values_from = wert, values_fill = list(wert = NA)) %>%
  mutate(Männer = ifelse(is.na(Frauen), NA, Gesamt - Frauen)) %>%
  pivot_longer(cols = c("Männer", "Frauen", "Gesamt"), names_to = "geschlecht", values_to = "wert") %>%
  mutate(fach = case_when(
    fach == "Ingenieurwissenschaften" & jahr > 2014 ~ "Ingenieurwissenschaften (inkl. Informatik)",
    fach == "Ingenieurwissenschaften" & jahr < 2015 ~ "Ingenieurwissenschaften (ohne Informatik)",
    TRUE ~ fach
  )) %>%
  na.omit()

states_east_west <- list(west = c("Baden-Württemberg", "Bayern", "Bremen", "Hamburg",
                                  "Hessen", "Niedersachsen", "Nordrhein-Westfalen",
                                  "Rheinland-Pfalz", "Saarland", "Schleswig-Holstein"),
                         east = c("Brandenburg", "Mecklenburg-Vorpommern", "Sachsen",
                                  "Sachsen-Anhalt", "Thüringen", "Berlin"))

df_ew <- df_all
df_ew$dummy_west <- ifelse(df_ew$region %in% states_east_west$west & df_ew$region != "Deutschland", "Westdeutschland (o. Berlin)", NA)
df_ew$dummy_west <- ifelse(df_ew$region %in% states_east_west$east & df_ew$region != "Deutschland", "Ostdeutschland (inkl. Berlin)", df_ew$dummy_west)

df_ew <- df_ew %>% dplyr::group_by(jahr, geschlecht, indikator, fachbereich, fach, dummy_west
                                   ,bereich, typ, mint_select) %>%
  dplyr::summarise(wert = sum(wert, na.rm = T))

colnames(df_ew)[colnames(df_ew) == "dummy_west"] <- "region"

df_ew <- df_ew[, colnames(df_all)]

df_ew <- na.omit(df_ew)
df_all <- rbind(df_all, df_ew)

studierende_detailliert <- df_all

## Export

# rio::export(studierende_detailliert, paste0(pfad, "studierende_detailliert.xlsx"))

# setwd("C:/Users/kab/Downloads/datalab/datalab/data-raw")


## Export

setwd("C:/Users/tko/OneDrive - Stifterverband/2_MINT-Lücke schließen/MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/02 Datenmaterial/02_data/data/")
setwd("C:/Users/kbr/OneDrive - Stifterverband/MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/02 Datenmaterial/02_data/data/")

save(studierende_detailliert, file = "studierende_detailliert.rda")










## UNESCO001_anteil_MINT_absolv_weltweit ----
# kbr bei mir ist der Pfad leicht anders ... unpraktisch

# pfad <- path_kek


dat_unc <- readr::read_csv(paste0(pfad, "UNESCO002_anteil_MINT_absolv_weltweit.csv"))
# dat_unc <- readr::read_csv(paste0(pfad, "UNESCO001_anteil_MINT_absolv_weltweit.csv"))

dat_unc <- dat_unc %>%
  rename(Indicator = "indicatorId") %>%
  rename(Value = "value") %>%
  rename(Country = geoUnit) %>%
  rename(Time = year)


dat_unc_1<- dat_unc %>%
  dplyr::select(Indicator, Value, Country, Time)%>%
  dplyr::rename( indikator = Indicator, wert = Value, land = Country, jahr = Time)%>%
  dplyr::mutate(typ = "In Prozent", geschlecht = "Insgesamt")%>%
  dplyr::mutate(fach =dplyr:: case_when(stringr::str_detect(.$indikator, "other than") ~ "Alle Nicht MINT-Fächer",
                                        T~"Alle MINT-Fächer" ))%>%
  dplyr::mutate(mint_select = dplyr::case_when(fach == "Alle MINT-Fächer"~"mint",
                                               T ~ "nicht mint" ))%>%
  dplyr::mutate(indikator = "Abslovent:innen", bereich = "Studium", quelle = "UNESCO", population= "Weltweit")


#dat_unc_1$land <- countrycode::countrycode(dat_unc_1$land, origin = 'country.name' , destination = "country.name.de")
dat_unc_1$land <- countrycode::countrycode(dat_unc_1$land, origin = "iso3c", destination = "country.name.de")


studierende_absolventen_weltweit <- dat_unc_1

# ordnen und speichern
studierende_absolventen_weltweit <- studierende_absolventen_weltweit[,c("bereich", "quelle", "population", "typ",
                                                                        "indikator", "mint_select", "fach",
                                                                        "geschlecht", "land", "jahr", "wert")]

usethis::use_data(studierende_absolventen_weltweit, overwrite = T)
save(studierende_absolventen_weltweit, file = "studierende_absolventen_weltweit.rda")




## OECD Anteil internationaler Studis an allen Studis in Fach ----

#file_path <- paste0("C:/Users/", akro, "/OneDrive - Stifterverband/AP7 MINT-DataLab/02 Datenmaterial/01_Rohdaten/02_Alle Daten")

dat_oecd <- readr::read_csv(paste0(pfad, "OECD006_Anteil_intern_Studis_in_Fach_2.csv"))

dat_oecd <- readr::read_csv(paste0(pfad, "neu.csv"))



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
    stringr::str_ends("F061", .$fach) ~ "Informatik & Kommunikationstechnologie allgemein",
    stringr::str_ends("F068", .$fach) ~ "Interdisziplinäre Programme mit Schwerpunkt Informatik & Kommunikationstechnologie",
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

##
