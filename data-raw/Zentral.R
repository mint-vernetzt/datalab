zentral_read <-
  readxl::read_xlsx(
    system.file(package = "datalab", "data-raw/Zentraler_Datensatz_23_10_22.xlsx")
  ) %>%
  janitor::clean_names() %>%
  janitor::remove_empty()

# remove dots from strings in column "region"
zentral_read$region <- gsub("\\.", "", zentral_read$region, perl=TRUE)

zentral_read$region <- gsub(' ', '', zentral_read$region)

zentral <- zentral_read %>% dplyr::mutate(wert = round(as.numeric(wert))) %>%
  dplyr::mutate(anzeige_geschlecht = replace(anzeige_geschlecht,
                                             anzeige_geschlecht == "frauen",
                                             "Frauen"),
                anzeige_geschlecht = replace(anzeige_geschlecht,
                                             anzeige_geschlecht == "männer",
                                             "Männer"),
                anzeige_geschlecht = replace(anzeige_geschlecht,
                                             anzeige_geschlecht == "gesamt",
                                             "Gesamt"),
                fachbereich = replace(fachbereich,
                                      fachbereich == "Mathe",
                                      "Mathematik/Naturwissenschaften"),
                fachbereich = replace(fachbereich,
                                      fachbereich == "Ingenieur",
                                      "Ingenieurwissenschaften"))%>%
  dplyr::filter(jahr >= 2010)

usethis::use_data(zentral, overwrite = T)
