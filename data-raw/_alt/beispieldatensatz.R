## code to prepare `DATASET` dataset goes here

example_data <- readxl::read_xlsx(system.file(package="datalab", "data-raw/beispieldatensatz.xlsx"), sheet = 1) %>%
  janitor::clean_names()

usethis::use_data(example_data)
