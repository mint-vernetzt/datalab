## code to prepare `professor_innen` dataset goes here

# read file remove empty rows
professor_innen_read <- readxl::read_xlsx(system.file(package="datalab", "data-raw/DES052_HP_Prof n FG_Geschl_Frauenanteil_ab 1992.xlsx"),
                  sheet = 1,
                  skip = 10) %>%
  janitor::clean_names() %>%
  janitor::remove_empty() %>%
  dplyr::slice(1:(dplyr::n()-5))

# adjust column names
colnames(professor_innen_read) <- c("code", "fachrichtung", "jahr", "insgesamt", "weiblich", "frauenanteil")

# prepare data
professor_innen <- professor_innen_read %>%
  dplyr::select(fachrichtung, jahr, insgesamt, weiblich) %>%
  tidyr::pivot_longer(cols = c("insgesamt", "weiblich"),
                      names_to = "geschlecht_aggregat",
                      values_to = "anzahl")


usethis::use_data(professor_innen, overwrite = TRUE)
