################################################################################
#
# Preprocessing Data Lab
# Vorbereitung Datensatz: alles Ausserschulische
# Author: Katharina Brunner
#
################################################################################

library(dplyr)

# kbr
pfad <- "C:/Users/kbr/OneDrive - Stifterverband/MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/02 Datenmaterial/01_Rohdaten/02_Alle Daten"
pfad <- "C:/Users/tko/OneDrive - Stifterverband/2_MINT-Lücke schließen/MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/02 Datenmaterial/01_Rohdaten/02_Alle Daten/"

# Community Plattform Daten -----------------------------------------------
## Organisationen ----

orgas <- read.csv(paste0(pfad, "/CP001_Organisationen.csv"))
orgas <- subset(orgas, select=-c(X) )

# Datenaufbereitung
total_N <- length(unique(orgas$name))
orgas <- subset(orgas, !(is.na(orgas$area)) | !(is.na(orgas$organizationType)) |
                  !is.na(orgas$focus))
any_value_N <- length(unique(orgas$name))

# Gesamt
type <- subset(orgas, select = c(name, organizationType))
type <- type[!duplicated(type), ]
type <- na.omit(type)
N_type <- length(unique(type$name))
type <- type %>%
  mutate(organizationType = as.factor(organizationType)) %>%
  count(organizationType) %>%
  rename(wert = n,
         indikator = organizationType)
type$typ <- "Organisationstyp"
type$region <- "Gesamt"
type <- type[, c("region", "typ", "indikator", "wert")]

area <- subset(orgas, select = c(name, area))
area <- area[!duplicated(area), ]
area <- na.omit(area)
N_area <- length(unique(area$name))
area <- area %>%
  mutate(area = as.factor(area)) %>%
  count(area) %>%
  rename(wert = n,
         indikator = area)
area$typ <- "Region"
area$region <- "Gesamt"
area <- area[, c("region", "typ", "indikator", "wert")]

focus <- subset(orgas, select = c(name, focus))
focus <- focus[!duplicated(focus), ]
focus <- na.omit(focus)
N_focus <- length(unique(focus$name))
focus <- focus %>%
  mutate(focus = as.factor(focus)) %>%
  count(focus) %>%
  rename(wert = n,
         indikator = focus)
focus$typ <- "Fokus"
focus$region <- "Gesamt"
focus <- focus[, c("region", "typ", "indikator", "wert")]


# Schnittmengen
bundeslaender_string <- c(
  "Baden-Württemberg", "Bayern", "Berlin", "Brandenburg",
  "Bremen", "Hamburg", "Hessen", "Mecklenburg-Vorpommern",
  "Niedersachsen", "Nordrhein-Westfalen", "Rheinland-Pfalz",
  "Saarland", "Sachsen", "Sachsen-Anhalt", "Schleswig-Holstein", "Thüringen"
)

type_area <- subset(orgas, select = c(name, organizationType, area))
type_area <- type_area[!duplicated(type_area), ]
type_area <- na.omit(type_area)
type_area <- type_area %>%
  filter(area %in% c(bundeslaender_string, "Bundesweit"))
N_type_area <-  type_area %>%
  group_by(area) %>%
  summarize(count = n_distinct(name))
type_area <- type_area %>%
  mutate(organizationType = as.factor(organizationType)) %>%
  group_by(area) %>%
  count(organizationType) %>%
  ungroup() %>%
  rename(wert = n,
         indikator = organizationType,
         region = area)
type_area$typ <- "Organisationstyp"
type_area <- type_area[, c("region", "typ", "indikator", "wert")]

focus_area <- subset(orgas, select = c(name, focus, area))
focus_area <- focus_area[!duplicated(focus_area), ]
focus_area <- na.omit(focus_area)
focus_area <- focus_area %>%
  filter(area %in% c(bundeslaender_string, "Bundesweit"))
N_focus_area <-  focus_area %>%
  group_by(area) %>%
  summarize(count = n_distinct(name))
focus_area <- focus_area %>%
  mutate(focus = as.factor(focus)) %>%
  group_by(area) %>%
  count(focus) %>%
  ungroup() %>%
  rename(wert = n,
         indikator = focus,
         region = area)
focus_area$typ <- "Fokus"
focus_area <- focus_area[, c("region", "typ", "indikator", "wert")]

# zusammenhängen
cp_orgas <- rbind(area, type, type_area, focus, focus_area)
cp_orgas$indikator <- as.character(cp_orgas$indikator)

#Ns anhängen
alle <- c(region ="Gesamt", typ = "Gesamt", indikator = "Alle",  wert= total_N)
ges <- c(region ="Gesamt", typ = "Gesamt", indikator = "Gesamt",  wert = any_value_N)
ges_area <- c(region = "Gesamt", typ = "Region", indikator = "Gesamt", wert = N_area)
ges_focus <- c(region = "Gesamt", typ = "Fokus", indikator = "Gesamt", wert = N_focus)
ges_typ <- c(region = "Gesamt", typ = "Organisationstyp", indikator = "Gesamt", wert = N_type)
N_focus_area <- N_focus_area %>%
  rename(region = area,
         wert = count) %>%
  mutate(typ = "Fokus",
        indikator = "Gesamt")
N_type_area <- N_type_area %>%
  rename(region = area,
         wert = count) %>%
  mutate(typ = "Organisationstyp",
         indikator = "Gesamt")

cp_orgas <- rbind(cp_orgas, alle, ges, ges_area, ges_focus, ges_typ,
                  N_type_area, N_focus_area)

#Speichern
ausserschulisch_cp_organisationen <- cp_orgas

  setwd("C:/Users/tko/OneDrive - Stifterverband/2_MINT-Lücke schließen/MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/02 Datenmaterial/02_data/data/")
  setwd("C:/Users/kbr/OneDrive - Stifterverband/MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/02 Datenmaterial/02_data/data/")


save(ausserschulisch_cp_organisationen, file = "ausserschulisch_cp_organisationen.rda")

#y?
setwd("~/datalab2")

## Envir. aufräumen
all <- ls()
keep <- c("pfad")
delete <- setdiff(all, keep)
rm(list=delete)


## Projekte ----

pros <- read.csv(paste0(pfad, "/CP002_Projekte.csv"))
pros <- subset(pros, select=-c(X) )

# Datenaufbereitung
total_N <- length(unique(pros$name))
pros <- subset(pros, !(is.na(pros$area)) | !(is.na(pros$discipline)) |
                  is.na(pros$additionalDiscipline) | is.na(pros$projectTargetGroup) |
                 is.na(pros$specialTargetGroup) | is.na(pros$format) |
                 is.na(pros$financing)
                 )
any_value_N <- length(unique(pros$name))

type <- c("discipline",  "additionalDiscipline", "projectTargetGroup", "specialTargetGroup",
          "format", "financing", "area")
indi <- c("MINT-Disziplin",  "weitere Disziplin", "Zielgruppe", "weitere Zielgruppe",
          "Format", "Finanzierung", "Region")
pros <- pros %>%
  mutate_at(vars(discipline, additionalDiscipline, projectTargetGroup, specialTargetGroup,
                 format, financing, area), as.factor)

# Gesamt
daten_gesamt_aufbereiten <- function(pros, typ, indi){

  var <- sym(typ)
  df <- subset(pros, select = c("name", typ))
  df <- df[!duplicated(df), ]
  df <- na.omit(df)
  N <- length(unique(df$name))
  df <- df %>%
    count(!!var) %>%
    rename(wert = n,
           indikator = !!var)
  df$typ <- indi
  df$region <- "Gesamt"
  df <- df[, c("region", "typ", "indikator", "wert")]

  return(list(df = df,
              df = N))
}

ges <- list()
for(i in 1:7){

  ges_new <- daten_gesamt_aufbereiten(pros, type[i], indi[i])
  ges <- c(ges, ges_new)

}

discipline <- ges[[1]]
N_discipline <- ges[[2]]
additionalDiscipline <- ges[[3]]
N_additionalDiscipline <- ges[[4]]
projectTargetGroup <- ges[[5]]
N_projectTargetGroup <- ges[[6]]
specialTargetGroup <- ges[[7]]
N_specialTargetGroup <- ges[[8]]
format <- ges[[9]]
N_format <- ges[[10]]
financing <- ges[[11]]
N_financing <- ges[[12]]
area <- ges[[13]]
N_area <- ges[[14]]


# Schnittmengen
bundeslaender_string <- c(
  "Baden-Württemberg", "Bayern", "Berlin", "Brandenburg",
  "Bremen", "Hamburg", "Hessen", "Mecklenburg-Vorpommern",
  "Niedersachsen", "Nordrhein-Westfalen", "Rheinland-Pfalz",
  "Saarland", "Sachsen", "Sachsen-Anhalt", "Schleswig-Holstein", "Thüringen"
)

type <- c("discipline",  "additionalDiscipline", "projectTargetGroup", "specialTargetGroup",
          "format", "financing")
indi <- c("MINT-Disziplin",  "weitere Disziplin", "Zielgruppe", "weitere Zielgruppe",
          "Format", "Finanzierung")

daten_schnittmenge_aufbereiten <- function(pros, vek, indi){

  var <- sym(vek)
  temp <- subset(pros, select = c("name", vek, "area"))
  temp <- temp[!duplicated(temp), ]
  temp <- na.omit(temp)
  temp <- temp %>%
    filter(area %in% c(bundeslaender_string, "Bundesweit"))
  N_schnitt <-  temp %>%
    group_by(area) %>%
    summarize(count = n_distinct(name))
  temp <- temp %>%
    group_by(area) %>%
    count(!!var) %>%
    ungroup() %>%
    rename(wert = n,
           indikator = var,
           region = area)
  temp$typ <- indi
  temp <- temp[, c("region", "typ", "indikator", "wert")]

  return(list(df = temp,
              N = N_schnitt))
}

schnittmenge<- list()
for(i in 1:length(type)){

  schnitt_new <- daten_schnittmenge_aufbereiten(pros, type[i], indi[i])
  schnittmenge <- c(schnittmenge, schnitt_new)

}

discipline_area <- schnittmenge[[1]]
N_discipline_area <- schnittmenge[[2]]
additionalDiscipline_area <- schnittmenge[[3]]
N_additionalDiscipline_area <- schnittmenge[[4]]
projectTargetGroup_area <- schnittmenge[[5]]
N_projectTargetGroup_area <- schnittmenge[[6]]
specialTargetGroup_area <- schnittmenge[[7]]
N_specialTargetGroup_area <- schnittmenge[[8]]
format_area <- schnittmenge[[9]]
N_format_area <- schnittmenge[[10]]
financing_area <- schnittmenge[[11]]
N_financing_area <- schnittmenge[[12]]

# Zusammenhängen der Datensetzt und Ns
cp_pros <- rbind(area, discipline, discipline_area,
                 additionalDiscipline, additionalDiscipline_area,
                 projectTargetGroup, projectTargetGroup_area,
                 specialTargetGroup, specialTargetGroup_area,
                 format, format_area, financing, financing_area)

cp_pros$indikator <- as.character(cp_pros$indikator)

#Ns anhängen
alle <- c(region ="Gesamt", typ = "Gesamt", indikator = "Alle",  wert= total_N)
ges <- c(region ="Gesamt", typ = "Gesamt", indikator = "Gesamt",  wert = any_value_N)
ges_area <- c(region = "Gesamt", typ = "Region", indikator = "Gesamt", wert = N_area)
ges_discipline <- c(region = "Gesamt", typ = "MINT-Disziplin", indikator = "Gesamt", wert = N_discipline)
ges_adidtional_disp <- c(region = "Gesamt", typ = "weitere Disziplin", indikator = "Gesamt", wert = N_additionalDiscipline)
ges_targetgr <- c(region = "Gesamt", typ = "Zielgruppe", indikator = "Gesamt", wert = N_projectTargetGroup)
ges_special_target <- c(region = "Gesamt", typ = "weitere Zielgruppe", indikator = "Gesamt", wert = N_specialTargetGroup)
ges_format <- c(region = "Gesamt", typ = "Format", indikator = "Gesamt", wert = N_format)
ges_financing <- c(region = "Gesamt", typ = "Finanzierung", indikator = "Gesamt", wert = N_financing)

N_discipline_area <- N_discipline_area %>%
  rename(region = area,
         wert = count) %>%
  mutate(typ = "MINT-Disziplin",
         indikator = "Gesamt")
N_additionalDiscipline_area <- N_additionalDiscipline_area %>%
  rename(region = area,
         wert = count) %>%
  mutate(typ = "weitere Disziplin",
         indikator = "Gesamt")
N_projectTargetGroup_area <- N_projectTargetGroup_area %>%
  rename(region = area,
         wert = count) %>%
  mutate(typ = "Zielgruppe",
         indikator = "Gesamt")
N_specialTargetGroup_area <- N_specialTargetGroup_area %>%
  rename(region = area,
         wert = count) %>%
  mutate(typ = "weitere Zielgruppe",
         indikator = "Gesamt")
N_format_area <- N_format_area %>%
  rename(region = area,
         wert = count) %>%
  mutate(typ = "Format",
         indikator = "Gesamt")
N_financing_area <- N_financing_area %>%
  rename(region = area,
         wert = count) %>%
  mutate(typ = "Finanzierung",
         indikator = "Gesamt")

cp_pros <- rbind(cp_pros, alle, ges, ges_area, ges_discipline,
                 ges_adidtional_disp, ges_targetgr,
                 ges_special_target, ges_format, ges_financing,
                 N_discipline_area, N_additionalDiscipline_area,
                 N_projectTargetGroup_area, N_specialTargetGroup_area,
                 N_format_area, N_financing_area)

#Speichern
ausserschulisch_cp_projekte <- cp_pros

setwd("C:/Users/tko/OneDrive - Stifterverband/2_MINT-Lücke schließen/MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/02 Datenmaterial/02_data/data/")
#setwd("C:/Users/kbr/OneDrive - Stifterverband/MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/02 Datenmaterial/02_data/data/")

save(ausserschulisch_cp_projekte, file = "ausserschulisch_cp_projekte.rda")


## Envir. aufräumen
all <- ls()
keep <- c("pfad")
delete <- setdiff(all, keep)
rm(list=delete)

## Profile -----

profs <- read.csv(paste0(pfad, "/CP003_Profile.csv"))
profs <- subset(profs, select=-c(X) )

# Datenaufbereitung
total_N <- length(unique(profs$id))
profs <- subset(profs, !(is.na(profs$area)) | !(is.na(profs$offer)) |
                  !is.na(profs$seekingsseeking_offer))
any_value_N <- length(unique(profs$id))

# Gesamt
area <- subset(profs, select = c(id, area))
area <- area[!duplicated(area), ]
area <- na.omit(area)
N_area <- length(unique(area$id))
area <- area %>%
  mutate(area = as.factor(area)) %>%
  count(area) %>%
  rename(wert = n,
         indikator = area)
area$typ <- "Region"
area$region <- "Gesamt"
area <- area[, c("region", "typ", "indikator", "wert")]

offer <- subset(profs, select = c(id, offer))
offer <- offer[!duplicated(offer), ]
offer <- na.omit(offer)
offer <- offer %>% filter(offer != "")
N_offer <- length(unique(offer$id))
offer <- offer %>%
  mutate(offer = as.factor(offer)) %>%
  count(offer) %>%
  rename(wert = n,
         indikator = offer)
offer$typ <- "Angebote"
offer$region <- "Gesamt"
offer <- offer[, c("region", "typ", "indikator", "wert")]

seeking <- subset(profs, select = c(id, seekingsseeking_offer))
seeking <- seeking[!duplicated(seeking), ]
seeking <- na.omit(seeking)
seeking <- seeking %>% filter(seekingsseeking_offer != "")
N_seeking <- length(unique(seeking$id))
seeking <- seeking %>%
  mutate(seekingsseeking_offer = as.factor(seekingsseeking_offer)) %>%
  count(seekingsseeking_offer) %>%
  rename(wert = n,
         indikator = seekingsseeking_offer)
seeking$typ <- "Gesucht"
seeking$region <- "Gesamt"
seeking <- seeking[, c("region", "typ", "indikator", "wert")]

# Schnittmengen
bundeslaender_string <- c(
  "Baden-Württemberg", "Bayern", "Berlin", "Brandenburg",
  "Bremen", "Hamburg", "Hessen", "Mecklenburg-Vorpommern",
  "Niedersachsen", "Nordrhein-Westfalen", "Rheinland-Pfalz",
  "Saarland", "Sachsen", "Sachsen-Anhalt", "Schleswig-Holstein", "Thüringen"
)

offer_area <- subset(profs, select = c(id, offer, area))
offer_area <- offer_area[!duplicated(offer_area), ]
offer_area <- na.omit(offer_area)
offer_area <- offer_area %>% filter(offer != "")
offer_area <- offer_area %>%
  filter(area %in% c(bundeslaender_string, "Bundesweit"))
N_offer_area <-  offer_area %>%
  group_by(area) %>%
  summarize(count = n_distinct(id))
offer_area <- offer_area %>%
  mutate(offer = as.factor(offer)) %>%
  group_by(area) %>%
  count(offer) %>%
  ungroup() %>%
  rename(wert = n,
         indikator = offer,
         region = area)
offer_area$typ <- "Angebote"
offer_area <- offer_area[, c("region", "typ", "indikator", "wert")]

seeking_area <- subset(profs, select = c(id, seekingsseeking_offer, area))
seeking_area <- seeking_area[!duplicated(seeking_area), ]
seeking_area <- na.omit(seeking_area)
seeking_area <- seeking_area %>% filter(seekingsseeking_offer != "")
seeking_area <- seeking_area %>%
  filter(area %in% c(bundeslaender_string, "Bundesweit"))
N_seeking_area <-  seeking_area %>%
  group_by(area) %>%
  summarize(count = n_distinct(id))
seeking_area <- seeking_area %>%
  mutate(seekingsseeking_offer = as.factor(seekingsseeking_offer)) %>%
  group_by(area) %>%
  count(seekingsseeking_offer) %>%
  ungroup() %>%
  rename(wert = n,
         indikator = seekingsseeking_offer,
         region = area)
seeking_area$typ <- "Gesucht"
seeking_area <- seeking_area[, c("region", "typ", "indikator", "wert")]

# zusammenhängen
cp_profile <- rbind(area, offer, offer_area, seeking, seeking_area)
cp_profile$indikator <- as.character(cp_profile$indikator)

#Ns anhängen
alle <- c(region ="Gesamt", typ = "Gesamt", indikator = "Alle",  wert= total_N)
ges <- c(region ="Gesamt", typ = "Gesamt", indikator = "Gesamt",  wert = any_value_N)
ges_area <- c(region = "Gesamt", typ = "Region", indikator = "Gesamt", wert = N_area)
ges_offer <- c(region = "Gesamt", typ = "Angebote", indikator = "Gesamt", wert = N_offer)
ges_seeking <- c(region = "Gesamt", typ = "Gesucht", indikator = "Gesamt", wert = N_seeking)
N_offer_area <- N_offer_area %>%
  rename(region = area,
         wert = count) %>%
  mutate(typ = "Angebote",
         indikator = "Gesamt")
N_seeking_area <- N_seeking_area %>%
  rename(region = area,
         wert = count) %>%
  mutate(typ = "Gesucht",
         indikator = "Gesamt")

cp_profile <- rbind(cp_profile, alle, ges, ges_area, ges_offer, ges_seeking,
                    N_offer_area, N_seeking_area)

#Speichern
ausserschulisch_cp_profile <- cp_profile

setwd("C:/Users/tko/OneDrive - Stifterverband/2_MINT-Lücke schließen/MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/02 Datenmaterial/02_data/data/")
#setwd("C:/Users/kbr/OneDrive - Stifterverband/MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/02 Datenmaterial/02_data/data/")
save(ausserschulisch_cp_profile, file = "ausserschulisch_cp_profile.rda")
setwd("~/datalab2")

## Envir. aufräumen
all <- ls()
keep <- c("pfad")
delete <- setdiff(all, keep)
rm(list=delete)



# MINTvernetzt-Befragungen ------------------------------------------------

## Akteursbefragung ----

df_akt <- readxl::read_excel(paste0(pfad,"/CP004_MINTvernetzt_Befragungsdaten.xlsx"),
                             sheet = "Akteursbefragung", col_names = TRUE)
df_akt <- df_akt[,1:3]

df_akt$indikator[df_akt$indikator == "Sonstige:"] <- "Sonstige"
df_akt$indikator[df_akt$indikator == "Anderer Sektor:"] <- "anderer Sektor"

# Speichern
ausserschulisch_akteursbefragung <- df_akt
setwd("C:/Users/kbr/OneDrive - Stifterverband/MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/02 Datenmaterial/02_data/data/")
save(ausserschulisch_akteursbefragung, file = "ausserschulisch_akteursbefragung.rda")
rm(list=ls())

## Stimmungsbarometer ----
df_stimm <- readxl::read_excel(paste0(pfad,"/CP004_MINTvernetzt_Befragungsdaten.xlsx"),
                             sheet = "Stimmungsbarometer", col_names = TRUE)

# Speichern
ausserschulisch_stimmungsbarometer <- df_stimm
setwd("C:/Users/kbr/OneDrive - Stifterverband/MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/02 Datenmaterial/02_data/data/")
save(ausserschulisch_stimmungsbarometer, file = "ausserschulisch_stimmungsbarometer.rda")
rm(list=ls())

## Genderbefragung ----
df_gen <- readxl::read_excel(paste0(pfad,"/CP004_MINTvernetzt_Befragungsdaten.xlsx"),
                                sheet = "Genderbefragung", col_names = TRUE)

# Speichern
ausserschulisch_genderbefragung <- df_gen
setwd("C:/Users/kbr/OneDrive - Stifterverband/MINTvernetzt (SV)/MINTv_SV_AP7 MINT-DataLab/02 Datenmaterial/02_data/data/")
save(ausserschulisch_genderbefragung, file = "ausserschulisch_genderbefragung.rda")
rm(list=ls())

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
