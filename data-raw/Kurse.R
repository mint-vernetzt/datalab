kurse_read <-
  readxl::read_xlsx(
    system.file(package = "datalab", "data-raw/Kurse_29_06_22.xlsx")
  ) %>%
  janitor::clean_names() %>%
  janitor::remove_empty() %>%
  dplyr::select(-c("quelle", "hinweise")) %>%
  dplyr::filter(!fachbereich %in% c("Nicht MINT", "MINT")) %>%
  dplyr::mutate(wert = tidyr::replace_na(wert, 0))

kurse <- kurse_read %>% dplyr::filter(jahr >= 2010)

kurse$wert <- round(kurse$wert)

#kurse <- kurse %>% dplyr::filter(region != "Baden-Württemberg")

kurse[kurse$region == "Rheinland-Pflaz", "region"] <- "Rheinland-Pfalz"

kurse[kurse$anzeige_geschlecht == "frauen", "anzeige_geschlecht"] <- "Frauen"
kurse[kurse$anzeige_geschlecht == "gesamt", "anzeige_geschlecht"] <- "Gesamt"
kurse[kurse$anzeige_geschlecht == "männer", "anzeige_geschlecht"] <- "Männer"

usethis::use_data(kurse, overwrite = T)
