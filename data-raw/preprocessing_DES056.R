
des056 <- readxl::read_xlsx(system.file(package="datalab", "data-raw/DES056_Statistik MINT-Fächer nach BL Jahre.xlsx"),
                            skip = 3, n_max = 2452)



##############################################################################
############################# specify function


gather_gender <- function(data_states, data_sub, input_string) {

  # rename column based on input of first row
  data_sub <- data_sub %>% janitor::row_to_names(row_number = 1)

  # include NA row to match other datasets for later steps
  data_sub <- rbind(c(NA, NA, NA), data_sub)

  # specify the origin: deutsch, ausländer, insgesamt
  data_sub$Herkunft <- input_string

  # combine data
  data_final <- cbind(data_states, data_sub)

  # gather the columns: männlich, weiblich, insgesamt into on column
  data_final <- data_final %>%
    tidyr::gather(key="Geschlecht", value="Wert", 5:7)

  # return data
  return(data_final)

}

##############################################################################
#############################  create the basis dataset with year and state

# subset the column containing year, state and subject
state_year <- des056[, 1:2]

# rename
names(state_year) <- c("Indikator", "Studienfach")

# extract the year e.g. WS 2010/11
state_year <- state_year %>%
  dplyr::mutate(Semester = stringr::str_extract(Indikator, "^.*/(\\d+)"))

# forward fill missing values
state_year <- state_year %>% tidyr::fill(Semester)

# specify all german states
string_states <- c("Baden-Württemberg",
                   "Bayern",
                   "Berlin",
                   "Brandenburg",
                   "Bremen",
                   "Hamburg",
                   "Hessen",
                   "Mecklenburg-Vorpommern",
                   "Niedersachsen",
                   "Nordrhein-Westfalen",
                   "Rheinland-Pfalz",
                   "Saarland",
                   "Sachsen",
                   "Sachsen-Anhalt",
                   "Schleswig-Holstein",
                   "Thüringen")


# create new column where all states will be listed
state_year$Bundesländer <- state_year$Indikator

# specify a "not in" operator
`%notin%` <- Negate(`%in%`)

# replace string with NA if not present in states string list
state_year <- state_year %>% naniar::replace_with_na_at(
  .vars = "Bundesländer",
  condition =  ~.x %notin% string_states)

# forward fill missing values
state_year <- state_year %>% tidyr::fill(Bundesländer)

##############################################################################
#############################  call function to create subdatasets

data_deutsch <- des056[, 3:5]

state_deutsch_comb <- gather_gender(data_states = state_year,
                                    data_sub = data_deutsch,
                                    input_string = "deutsch")

data_ausl <- des056[, 6:8]

state_ausl_comb <- gather_gender(data_states = state_year,
                                 data_sub = data_ausl,
                                 input_string = "ausländer")

data_insg <- des056[, 9:11]

state_insg_comb <- gather_gender(data_states = state_year,
                                 data_sub = data_insg,
                                 input_string = "insgesamt")

##############################################################################
#############################  minor preprocessing

# combine data
des056_final <- rbind(state_deutsch_comb,state_ausl_comb,state_insg_comb)

# drop row if NA is present
des056_final <- des056_final[complete.cases(des056_final), ]

# replace dash with NA
des056_final[des056_final == "-" ] <- NA

# drop ID number from subject
des056_final$Indikator <- NULL


usethis::use_data(des056_final)
