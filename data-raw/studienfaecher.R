studierende_faecher <-
  readxl::read_xlsx(
    system.file(package = "datalab", "data-raw/studierende_faecher.xlsx")
  ) %>%
  janitor::clean_names() %>%
  janitor::remove_empty() %>%
  dplyr::rename(anzeige_geschlecht = geschlecht) %>%
  dplyr::filter(indikator %in% c("Studierende", "Studierende_Lehramt")) %>%
  dplyr::select(-c("label", "bereich"))


usethis::use_data(studierende_faecher, overwrite = T)
