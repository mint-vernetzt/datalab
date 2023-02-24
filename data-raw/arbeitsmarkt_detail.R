# arbeitsmarkt_detail.rda
# "Alter" Datensatz, Berechnung stimmt noch nicht, Berechnung vor gegebener Beispiele


load(file = system.file(package="datalab","data/arbeitsmarkt_detail.rda"))

arbeitsmarkt_detail <- arbeitsmarkt_detail %>%
  dplyr::mutate(wert = ifelse(is.na(wert), 0, wert))

# Calculate Beschäftigte 25-55
arbeitsmarkt_detail_alter <- arbeitsmarkt_detail %>% dplyr::filter(indikator %in% c("Beschäftigte", "Beschäftigte u25", "Beschäftigte ü55"),
                                                                   geschlecht == "Gesamt")

arbeitsmarkt_detail_alter <- arbeitsmarkt_detail_alter %>% dplyr::group_by(bereich, kategorie, fachbereich, geschlecht, bundesland, landkreis,
                                                                           landkreis_zusatz, landkreis_nummer, jahr, anforderung) %>%
  dplyr::summarise(wert = wert - dplyr::lead(wert, 1) - dplyr::lead(wert, 2)) %>%
  dplyr::mutate(indikator = "Beschäftigte 25-55") %>%
  dplyr::filter(!is.na(wert)) %>%
  dplyr::bind_rows(., arbeitsmarkt_detail_alter)

# Calculate ausländische Beschäftigte 25-55
arbeitsmarkt_detail_ausl_alter <- arbeitsmarkt_detail %>% dplyr::filter(indikator %in% c("ausländische Beschäftigte", "ausländische Beschäftigte u25", "ausländische Beschäftigte ü55"),
                                                                        geschlecht == "Gesamt")

arbeitsmarkt_detail_ausl_alter <- arbeitsmarkt_detail_ausl_alter %>% dplyr::group_by(bereich, kategorie, fachbereich, geschlecht, bundesland, landkreis,
                                                                                     landkreis_zusatz, landkreis_nummer, jahr, anforderung) %>%
  dplyr::summarise(wert = wert - dplyr::lead(wert, 1) - dplyr::lead(wert, 2)) %>%
  dplyr::mutate(indikator = "ausländische Beschäftigte 25-55") %>%
  dplyr::filter(!is.na(wert)) %>%
  dplyr::bind_rows(., arbeitsmarkt_detail_ausl_alter)

# Calculate males
arbeitsmarkt_detail_geschlecht <- arbeitsmarkt_detail %>% dplyr::filter(!indikator %in% c("Beschäftigte u25", "Beschäftigte ü55",
                                                                                          "ausländische Beschäftigte u25", "ausländische Beschäftigte ü55")) %>%
  dplyr::group_by(bereich, kategorie, indikator, fachbereich, bundesland, landkreis,
                  landkreis_zusatz, landkreis_nummer, jahr, anforderung) %>%
  dplyr::summarise(wert = wert - dplyr::lead(wert, 1)) %>%
  dplyr::mutate(geschlecht = "Männer") %>%
  dplyr::filter(!is.na(wert)) %>%
  dplyr::bind_rows(., arbeitsmarkt_detail)

arbeitsmarkt_detail_final <- dplyr::bind_rows(arbeitsmarkt_detail_geschlecht, arbeitsmarkt_detail_alter, arbeitsmarkt_detail_ausl_alter) %>%
  dplyr::distinct()

usethis::use_data(arbeitsmarkt_detail_final, overwrite = T)
