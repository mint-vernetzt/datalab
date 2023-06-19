studierende_read <-
  readxl::read_xlsx(
    system.file(package = "datalab", "data-raw/Studierende.xlsx")
  ) %>%
  janitor::clean_names() %>%
  janitor::remove_empty()

# remove dots from strings in column "region"
studierende_read$region <- gsub("\\.", "", studierende_read$region, perl=TRUE)

studierende_read$region <- gsub(' ', '', studierende_read$region)

studierende_read <- studierende_read %>%
  dplyr::mutate(wert = as.numeric(wert)) %>%
  dplyr::mutate(anzeige_geschlecht = replace(anzeige_geschlecht,
                                             anzeige_geschlecht == "frauen",
                                           "Frauen"),
                anzeige_geschlecht = replace(anzeige_geschlecht,
                                             anzeige_geschlecht == "gesamt",
                                    "Gesamt"),
                fachbereich = replace(fachbereich,
                                      fachbereich == "Mathe",
                                      "Mathematik/Naturwissenschaften"),
                fachbereich = replace(fachbereich,
                                      fachbereich == "Ingenieur",
                                      "Ingenieurwissenschaften"))

studierende <- studierende_read

usethis::use_data(studierende, overwrite = T)
