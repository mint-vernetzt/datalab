
data_naa <- readxlsb::read_xlsb(system.file(package="datalab",
                                          "data-raw/BA002_Ausbildungsmarkt-MINT-Frauenanteil-2020.xlsb"),
                            sheet  = "Verträge_Daten", range = "A4:VE219")

# remove all districts
data_naa <- data_naa %>% dplyr::select(-contains("X"))

# remove
data_naa <- data_naa %>% subset(select = -c(code, Bezeichnung))

# clean column with job titles
# rename column
names(data_naa)[2] <- "Bezeichnung"
# remove row with sum over all jobs
data_naa <- data_naa[!grepl('Referenzzeile', data_naa$Bezeichnung),]
# remove numbers from job title
data_naa$Bezeichnung <- gsub('[[:digit:]]+', '', data_naa$Bezeichnung)
# remove white space
data_naa$Bezeichnung <- gsub(' ', '', data_naa$Bezeichnung)

# remove all column which provide information about the "Frauenanteil"
data_naa <- data_naa %>% dplyr::select(-contains("anteil"))

# reshape data_naa in long format
data_naa <- data_naa %>% tidyr::gather(Bundeslaender, Wert, -Ebene, -Bezeichnung)

# extract the information of gender contained in the strings of the
# column "Bundesländer"
data_naa <- data_naa %>% dplyr::mutate(Geschlecht = stringr::str_extract(Bundeslaender, "_w_"))

# replace "_w_" with "weiblich"
data_naa$Geschlecht <- gsub("^.*\\_","weiblich", data_naa$Geschlecht)

# the remaining NA are replaced by the label "insgesamt"
data_naa$Geschlecht <- data_naa$Geschlecht %>% tidyr::replace_na('insgesamt')

# extract the information of the year from the string in column "Bundesländer"
data_naa <- data_naa %>% tidyr::extract(Bundeslaender, c("Bundeslaender", "Jahr"), "(.*)_([^_]+)")

# clean the string so that only the names of the "Bundesländer" are left
data_naa$Bundeslaender <-  sub(".NAA.*", "", data_naa$Bundeslaender)

# replace dot with dash
data_naa$Bundeslaender <- gsub('\\.', '-', data_naa$Bundeslaender)

# drop row if no label for the job title is given
data_naa <- data_naa[-which(data_naa$Bezeichnung == ""), ]

# create a sub-data_naaframe split by gender
data_naa_weiblich <- data_naa %>% dplyr::group_by(Ebene, Bezeichnung, Bundeslaender, Jahr) %>%
  dplyr::filter(Geschlecht == "weiblich")

# same for "insgesamt"
data_naa_insgesamt <- data_naa %>% dplyr::group_by(Ebene, Bezeichnung, Bundeslaender, Jahr) %>%
  dplyr::filter(Geschlecht == "insgesamt")

# now subtract the values for "weiblich" from "insgesamt" to create "männnlich"
 # first create new data_frame (copy)
data_naa_maennlich <- data_naa_insgesamt
 # second subtract values
data_naa_maennlich$Wert <- data_naa_maennlich$Wert - data_naa_weiblich$Wert
 # specify gender as "männlich"
data_naa_maennlich$Geschlecht <- "männlich"

# combine data_frames
data_naa <- rbind(data_naa_insgesamt, data_naa_weiblich, data_naa_maennlich)

# insert zero if NA
data_naa[is.na(data_naa)] <- 0

# sort data_naa
data_naa <- data_naa[with(data_naa, order(Bezeichnung, Bundeslaender, Jahr)), ]

usethis::use_data(data_naa, overwrite = T)




