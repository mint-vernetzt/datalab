
data_naa <- readxlsb::read_xlsb(system.file(package="datalab",
                                          "data-raw/BA002_Ausbildungsmarkt-MINT-Frauenanteil-2020.xlsb"),
                            sheet  = "Verträge_Daten", range = "A4:VE219")

# remove all districts
data_naa <- data_naa %>% dplyr::select(-contains("X"))

# remove
data_naa <- data_naa %>% subset(select = -c(code, Bezeichnung))

# clean column with job titles
# rename column
data_naa <- data_naa %>% dplyr::rename(ebene = "Ebene",
                           fachrichtung = "Bezeichnung.BIBB.modifiziert")
# remove row with sum over all jobs
data_naa <- data_naa %>% dplyr::filter(!grepl('Referenzzeile', fachrichtung)) %>%
  # remove numbers from job title
  dplyr::mutate(fachrichtung = gsub('[[:digit:]]+', '', fachrichtung),
                # remove white space
                fachrichtung = gsub(' ', '', fachrichtung)) %>%
  # remove all column which provide information about the "Frauenanteil"
  dplyr::select(-contains("anteil"))

# reshape data_naa in long format
data_naa <- data_naa %>% tidyr::gather(region, anzahl, -ebene, -fachrichtung)

# extract the information of gender contained in the strings of the
# column "Bundesländer"
data_naa <- data_naa %>% dplyr::mutate(geschlecht_aggregat = stringr::str_extract(region, "_w_"),
                                       # replace "_w_" with "weiblich"
                                       geschlecht_aggregat = gsub("^.*\\_","weiblich", geschlecht_aggregat),
                                       # the remaining NA are replaced by the label "insgesamt"
                                       geschlecht_aggregat = tidyr::replace_na(geschlecht_aggregat, 'insgesamt')) %>%
  # extract the information of the year from the string in column "Bundesländer"
  tidyr::extract(region, c("region", "jahr"), "(.*)_([^_]+)") %>%
  # clean the string so that only the names of the "Bundesländer" are left
  dplyr::mutate(region = sub(".NAA.*", "", region),
                # replace dot with dash
                region = gsub('\\.', '-', region)) %>%
  # drop row if no label for the job title is given
  dplyr::filter(fachrichtung != "")

# create a sub-data_naaframe split by gender
data_naa_weiblich <- data_naa %>% dplyr::group_by(ebene, fachrichtung, region, jahr) %>%
  dplyr::filter(geschlecht_aggregat == "weiblich")

# same for "insgesamt"
data_naa_insgesamt <- data_naa %>% dplyr::group_by(ebene, fachrichtung, region, jahr) %>%
  dplyr::filter(geschlecht_aggregat == "insgesamt")

# now subtract the values for "weiblich" from "insgesamt" to create "männnlich"
 # first create new data_frame (copy)
data_naa_maennlich <- data_naa_insgesamt
 # second subtract values
data_naa_maennlich$anzahl <- data_naa_maennlich$anzahl - data_naa_weiblich$anzahl
 # specify gender as "männlich"
data_naa_maennlich$geschlecht_aggregat <- "männlich"

# combine data_frames
data_naa <- rbind(data_naa_insgesamt, data_naa_weiblich, data_naa_maennlich)

# insert zero if NA
data_naa <- data_naa %>%
  dplyr::mutate(anzahl = tidyr::replace_na(anzahl, 0))

# sort data_naa
data_naa <- data_naa[with(data_naa, order(fachrichtung, region, jahr)), ]

usethis::use_data(data_naa, overwrite = T)




