arbeitsmarkt_read <-
  readxl::read_xlsx(
    system.file(package = "datalab", "data-raw/Arbeitsmarkt.xlsx")
  ) %>%
  janitor::clean_names() %>%
  janitor::remove_empty()

arbeitsmarkt <- arbeitsmarkt_read %>% dplyr::filter(anforderungsniveau != "Keine Zuordnung möglich")

arbeitsmarkt$wert <- as.numeric(arbeitsmarkt$wert)

arbeitsmarkt[arbeitsmarkt$anzeige_geschlecht == "frauen", "anzeige_geschlecht"] <- "Frauen"
arbeitsmarkt[arbeitsmarkt$anzeige_geschlecht == "gesamt", "anzeige_geschlecht"] <- "Gesamt"

arbeitsmarkt[arbeitsmarkt$anforderungsniveau == "gesamt", "anforderungsniveau"] <- "Gesamt"

arbeitsmarkt[arbeitsmarkt$region == "Rheinland-Pflaz", "region"] <- "Rheinland-Pfalz"


usethis::use_data(arbeitsmarkt, overwrite = T)
