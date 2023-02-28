setwd( "C:/Users/kab/Downloads/datalab/datalab/data-raw")

zentral_read <-
  readxl::read_xlsx(
    system.file(package = "datalab", "data-raw/Zentraler_Datensatz_alt_27_02_23.xlsx")
  ) %>%
  janitor::clean_names() %>%
  janitor::remove_empty()

# remove dots from strings in column "region"
zentral_read$region <- gsub("\\.", "", zentral_read$region, perl=TRUE)

zentral_read$region <- gsub(' ', '', zentral_read$region)

zentral_alt <- zentral_read %>% dplyr::mutate(wert = round(as.numeric(wert))) %>%
  dplyr::mutate(geschlecht = replace(geschlecht,
                                             geschlecht == "frauen",
                                             "Frauen"),
                geschlecht = replace(geschlecht,
                                             geschlecht == "männer",
                                             "Männer"),
                geschlecht = replace(geschlecht,
                                             geschlecht == "gesamt",
                                             "Gesamt"),
                fachbereich = replace(fachbereich,
                                      fachbereich == "Mathe",
                                      "Mathematik/Naturwissenschaften"),
                fachbereich = replace(fachbereich,
                                      fachbereich == "Ingenieur",
                                      "Ingenieurwissenschaften"))%>%
  dplyr::filter(jahr >= 2010)

usethis::use_data(zentral_alt, overwrite = T)

setwd( "C:/Users/kab/Downloads/datalab/datalab/data-raw")

zentral_read_2 <-
  readxl::read_xlsx("Zentraler_Datensatz_neu_27_02_23.xlsx")%>%
  janitor::clean_names() %>%
  janitor::remove_empty()


zentral_read_2$region <- gsub("\\.", "", zentral_read$region, perl=TRUE)

zentral_read_2$region <- gsub(' ', '', zentral_read$region)

zentral_neu <- zentral_read_2 %>% dplyr::mutate(wert = round(as.numeric(wert))) %>%
  dplyr::mutate(geschlecht = replace(geschlecht,
                                             geschlecht == "frauen",
                                             "Frauen"),
                geschlecht = replace(geschlecht,
                                             geschlecht == "männer",
                                             "Männer"),
                geschlecht = replace(geschlecht,
                                             geschlecht == "gesamt",
                                             "Gesamt"),
                fachbereich = replace(fachbereich,
                                      fachbereich == "Mathe",
                                      "Mathematik/Naturwissenschaften"),
                fachbereich = replace(fachbereich,
                                      fachbereich == "Ingenieur",
                                      "Ingenieurwissenschaften"))%>%
  dplyr::filter(jahr >= "2010")



usethis::use_data(zentral_neu, overwrite = T)
