studierende_read <-
  readxl::read_xlsx(
    system.file(package = "datalab", "data-raw/Studierende.xlsx")
  ) %>%
  janitor::clean_names() %>%
  janitor::remove_empty()

# remove dots from strings in column "region"
studierende_read$region <- gsub("\\.", "", studierende_read$region, perl=TRUE)

studierende_read$region <- gsub(' ', '', studierende_read$region)

studierende_read$wert <- as.numeric(studierende_read$wert)

studierende <- studierende_read

usethis::use_data(studierende, overwrite = T)
