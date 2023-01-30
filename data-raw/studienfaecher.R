studierende_faecher <-
  readxl::read_xlsx(
    system.file(package = "datalab", "data-raw/studierende_faecher.xlsx")
  ) %>%
  janitor::clean_names() %>%
  janitor::remove_empty() %>%
  dplyr::rename(anzeige_geschlecht = geschlecht) %>%
  dplyr::filter(indikator == "Studierende",
                !(fachbereich %in% c("MINT", "Zusammen"))
                ) %>%
  dplyr::select(-c("label", "bereich")) %>%
  dplyr::mutate(fachbereich = dplyr::case_when(fachbereich == "Ingenieurwissenschaft" ~ "Ingenieurwissenschaften",
                                                      TRUE ~ fachbereich)) %>%
  dplyr::mutate(fach = dplyr::case_when(fachbereich %in% c("Mathematik, Naturwissenschaften", "Ingenieurwissenschaften") ~ fach,
                                               TRUE ~ fachbereich)) %>%
  dplyr::filter(!(fach %in% c("Alle Fächer", "Alle MINT-Fächer")))

# create region Deutschland
deutschland_gesamt <- studierende_faecher %>% dplyr::group_by(indikator,
                                                              jahr,
                                                              fachbereich,
                                                              fach,
                                                              anzeige_geschlecht) %>%
  dplyr::summarise(wert = sum(wert, na.rm = T)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(region = "Deutschland")

# create region Osten
osten <- studierende_faecher %>%
  dplyr::filter(region %in% c("Brandenburg",
                              "Mecklenburg-Vorpommern",
                              "Sachsen",
                              "Sachsen-Anhalt",
                              "Thüringen")) %>%
  dplyr::group_by(indikator,
                  jahr,
                  fachbereich,
                  fach,
                  anzeige_geschlecht) %>%
  dplyr::summarise(wert = sum(wert, na.rm = T)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(region = "Osten")

# create region Westen
westen <- studierende_faecher %>%
  dplyr::filter(!(region %in% c("Brandenburg",
                                "Mecklenburg-Vorpommern",
                                "Sachsen",
                                "Sachsen-Anhalt",
                                "Thüringen"))) %>%
  dplyr::group_by(indikator,
                  jahr,
                  fachbereich,
                  fach,
                  anzeige_geschlecht) %>%
  dplyr::summarise(wert = sum(wert, na.rm = T)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(region = "Westen")


studierende_faecher <- dplyr::bind_rows(studierende_faecher, deutschland_gesamt, westen, osten)

usethis::use_data(studierende_faecher, overwrite = T)
