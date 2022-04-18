## code to prepare `langzeitarbeitslosen` dataset goes here

# read file and clean names, fill missing values from merged cells
langzeitarbeitslose_read <- readxl::read_xlsx(system.file(package="datalab", "data-raw/BA005_2021-11-23_A-322213_LZA-Alo_Kröger-MintBerufe-Frauen.xlsx"),
                                         sheet = 2, skip = 6) %>%
  janitor::clean_names() %>%
  dplyr::slice(-nrow(.)) %>%
  dplyr::slice(-3) %>%
  tidyr::fill(region, mint_aggregat)

# adjust column names
colnames(langzeitarbeitslose_read) <- as.data.frame(t(langzeitarbeitslose_read[1:2,])) %>%
  tidyr::fill(V1) %>%
  dplyr::mutate(V1 = paste0("x", V1, "_", V2)) %>%
  dplyr::mutate(V1 = ifelse(V1 == "xNA_NA", rownames(.), V1)) %>%
  dplyr::select(-V2) %>%
  t()

# prepare data
langzeitarbeitslose <- langzeitarbeitslose_read %>%
  dplyr::slice(-c(1:2)) %>%
  tidyr::pivot_longer(cols = tidyr::starts_with("x20"),
                      names_to = "xjahr_geschlecht",
                      values_to = "anzahl") %>%
  dplyr::mutate(jahr = stringr::str_sub(xjahr_geschlecht, 2, 5),
                geschlecht_aggregat = stringr::str_sub(xjahr_geschlecht, 7, nchar(xjahr_geschlecht))) %>%
  dplyr::select(-xjahr_geschlecht) %>%
  dplyr::mutate(anzahl = round(as.numeric(anzahl),0)) %>%
  dplyr::mutate(geschlecht_aggregat=dplyr::recode(geschlecht_aggregat,
                    Insgesamt="insgesamt",
                    Frauen="weiblich"))


usethis::use_data(langzeitarbeitslose, overwrite = TRUE)
