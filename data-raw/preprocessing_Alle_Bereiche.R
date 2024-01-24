library(magrittr)
library(dplyr)

pfad_kab <- "C:/Users/kab/Downloads/datalab/datalab/data/"

# pfad_kbr <-


pfad <- pfad_kab

# daten laden
load(paste0(pfad,"kurse.rda"))
load(paste0(pfad,"arbeitsmarkt.rda"))
load(paste0(pfad,"studierende.rda"))

# Schule
data_schule <-kurse %>%
  select(bereich, indikator, anzeige_geschlecht, fachbereich, jahr, region, wert)%>%
  #rename(jahr=Jahr)%>%
  mutate(across(jahr, ~ as.character(.)))%>%
  rename(geschlecht=anzeige_geschlecht)
duplikate <- data_schule %>%
  janitor::get_dupes()

# Arbeitsmarkt
data_arbeitsmarkt <- arbeitsmarkt %>%
  filter(anforderung=="Gesamt") %>%
  select(-anforderung)%>%
  mutate(across(wert, ~ as.numeric(.)))%>%
  #rename(jahr=Jahr)%>%
  mutate(across(jahr, ~ as.character(.)))%>%
  filter(region== "Deutschland")

# Studierende
data_studierende <- studierende%>%
  filter(indikator=="Studierende") %>%
  #select(-quelle,-hinweise,-indikator)%>%
  #rename(indikator=label)%>%
  mutate(across(wert, ~ as.numeric(.)))%>%
  #rename(jahr=Jahr)%>%
  mutate(across(jahr, ~ as.character(.)))%>%
  mutate(bereich = "Hochschule",
         fachbereich = case_when(
           fachbereich == "MINT (Gesamt)" ~ "MINT",
           T ~ .$fachbereich
         ))

duplikate <- data_studierende%>%
  janitor::get_dupes(-wert)


zentral <- bind_rows(data_schule,  data_arbeitsmarkt,  data_studierende)



# zentral_read_2 <-
#   readxl::read_xlsx(paste0(pfad, "Zentraler_Datensatz_neu_12_01_24.xlsx"))%>%
#   janitor::clean_names() %>%
#   janitor::remove_empty()
#
#
# zentral_read_2$region <- gsub("\\.", "", zentral_read_2$region, perl=TRUE)
#
# zentral_read_2$region <- gsub(' ', '', zentral_read_2$region)
#
# zentral_neu <- zentral_read_2 %>% dplyr::mutate(wert = round(as.numeric(wert))) %>%
#   dplyr::mutate(geschlecht = replace(anzeige_geschlecht,
#                                      anzeige_geschlecht == "frauen",
#                                              "Frauen"),
#                 geschlecht = replace(anzeige_geschlecht,
#                                      anzeige_geschlecht == "männer",
#                                              "Männer"),
#                 geschlecht = replace(anzeige_geschlecht,
#                                      anzeige_geschlecht == "gesamt",
#                                              "Gesamt"),
#                 fachbereich = replace(fachbereich,
#                                       fachbereich == "Mathe",
#                                       "Mathematik/Naturwissenschaften"),
#                 fachbereich = replace(fachbereich,
#                                       fachbereich == "Ingenieur",
#                                       "Ingenieurwissenschaften"))%>%
#   dplyr::filter(jahr >= "2010")



usethis::use_data(zentral, overwrite = T)

