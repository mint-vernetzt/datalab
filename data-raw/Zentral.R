zentral_read <-
  readxl::read_xlsx(
    system.file(package = "datalab", "data-raw/Zentraler_Datensatz.xlsx")
  ) %>%
  janitor::clean_names() %>%
  janitor::remove_empty()

# remove dots from strings in column "region"
zentral_read$region <- gsub("\\.", "", zentral_read$region, perl=TRUE)

zentral_read$region <- gsub(' ', '', zentral_read$region)

zentral_read$wert <- as.numeric(zentral_read$wert)

zentral_read$wert <- round(zentral_read$wert)

zentral_read[zentral_read$anzeige_geschlecht == "frauen", "anzeige_geschlecht"] <- "Frauen"
zentral_read[zentral_read$anzeige_geschlecht == "männer", "anzeige_geschlecht"] <- "Männer"
zentral_read[zentral_read$anzeige_geschlecht == "gesamt", "anzeige_geschlecht"] <- "Gesamt"


zentral <- zentral_read %>% dplyr::filter(jahr >= 2010)

usethis::use_data(zentral, overwrite = T)
