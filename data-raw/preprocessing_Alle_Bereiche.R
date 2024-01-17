library(magrittr)

pfad_kab <- "C:/Users/kab/OneDrive - Stifterverband/AP7 MINT-DataLab/02 Datenmaterial/02_Prozess/Datenaufbereitung 2023/Zentral/"

# pfab_kbr <-


pfad <- pfad_kab



zentral_read_2 <-
  readxl::read_xlsx(paste0(pfad, "Zentraler_Datensatz_neu_12_01_24.xlsx"))%>%
  janitor::clean_names() %>%
  janitor::remove_empty()


zentral_read_2$region <- gsub("\\.", "", zentral_read_2$region, perl=TRUE)

zentral_read_2$region <- gsub(' ', '', zentral_read_2$region)

zentral_neu <- zentral_read_2 %>% dplyr::mutate(wert = round(as.numeric(wert))) %>%
  dplyr::mutate(geschlecht = replace(anzeige_geschlecht,
                                     anzeige_geschlecht == "frauen",
                                             "Frauen"),
                geschlecht = replace(anzeige_geschlecht,
                                     anzeige_geschlecht == "männer",
                                             "Männer"),
                geschlecht = replace(anzeige_geschlecht,
                                     anzeige_geschlecht == "gesamt",
                                             "Gesamt"),
                fachbereich = replace(fachbereich,
                                      fachbereich == "Mathe",
                                      "Mathematik/Naturwissenschaften"),
                fachbereich = replace(fachbereich,
                                      fachbereich == "Ingenieur",
                                      "Ingenieurwissenschaften"))%>%
  dplyr::filter(jahr >= "2010")



usethis::use_data(zentral_neu, overwrite = T)

