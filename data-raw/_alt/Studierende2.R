studierende_read <-
  readxl::read_xlsx(
    system.file(package = "datalab", "data-raw/Studierende_2023.xlsx")
  ) %>%
  janitor::clean_names() %>%
  janitor::remove_empty()


studierende_read$region <- gsub('Deutschland ...', 'Deutschland', studierende_read$region)

studierende_read$region <- gsub("\\.", "", studierende_read$region, perl=TRUE)

studierende_read$region <- gsub(' ', '', studierende_read$region)

# studierende_read <- studierende_read %>% dplyr::select(-quelle,-hinweis)



studierende_read <- studierende_read %>%
  dplyr::mutate(wert = as.numeric(wert))

studierende2 <- studierende_read

usethis::use_data(studierende2, overwrite = T)
