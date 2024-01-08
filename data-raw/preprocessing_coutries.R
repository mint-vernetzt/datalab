library(stringdist)

# englische Namen der Länder
# https://github.com/stefangabos/world_countries/blob/master/data/countries/de/countries.csv
this_url <-  "https://raw.githubusercontent.com/stefangabos/world_countries/master/data/countries/de/countries.csv"
countries_raw_en <-  read.csv(file = this_url)


# add aditional country codes
# puerto rico PR
# Swasiland SZ / Eswatini SZ
countries_raw_en <- rbind(
  countries_raw_en,
  data.frame(
    id = NA,
    alpha2 = "PR",
    alpha3 = NA,
    name = "Puerto Rico"),
  data.frame(
    id = NA,
    alpha2 = "SZ",
    alpha3 = NA,
    name = "Swasiland")
  )


# check existing country names
data_files <- list.files("data/", pattern = ".rda")
data_names <- gsub(".rda", "", data_files)
# fix spelling
data_names <- gsub("„", "ä", data_names)

country_list <- c()

i_data <- data_names[1]
for (i_data in data_names) {
  tmp <- get(i_data)
  if ("land" %in% names(tmp)) {
    print(i_data)
    country_list <- unique(c(country_list, tmp$land))
  }
}

missing_countries <- setdiff(country_list, countries_raw_en$name)

logger::log_info(length(country_list),
                 " found countries in all data")
logger::log_info(length(countries_raw_en$name),
                 " countries in mapping data")
logger::log_info(length(missing_countries),
                 " countries could not be mapped initialy")
logger::log_info(paste0(missing_countries, collapse = " | "))

dist_mat <- stringdist::stringdistmatrix(
  a = missing_countries,
  b = countries_raw_en$name)

min_dist <- unlist(
  apply(
    X = dist_mat,
    MARGIN = 1,
    FUN = function(x) {
      if (all(is.na(x))) {
        out <- NA
      } else {
        out <- which.min(x)
      }
    }, simplify = FALSE
  )
)

# Versuch über string dist ein match zu lösen
# ist niciht sehr erfolgreich!
# land_matched <- data.frame(
#   land = missing_countries,
#   pos_match = countries_raw_en$name[min_dist]
# )
# View(land_matched)

# manual maping with hardcoded countries
mapping_dt <- tibble::tribble(
  ~ land, ~ mapped_land,
  "Tschechische Republik", "Tschechien",
  "Großbritannien", "Vereinigtes Königreich", # Spezialfall
  "Korea, Republik von", "Korea, Süd (Südkorea)",
  "Russische Föderation", "Russland",
  #"OECD-Mitglieder aus der EU",
  #"OECD",
  #"OECD Durchschnitt",
  NA, NA,
  "Hongkong", NA,
  "Taiwan", NA,
  #"Nordirland", # Spezialfall!
  #"England", # Spezialfall!
  "Saudi Arabien", "Saudi-Arabien",
  "Kosovo", NA,
  #"Interantionaler Durchschnitt",
  #"Swasiland", NA,
  "Kongo", "Kongo, Republik",
  "Gibraltar", NA,
  "Montserrat", NA,
  #"Puerto Rico", NA,
  "Demokratische Volksrepublik Laos", "Laos",
  "Bermuda", NA,
  "Republik Moldau","Moldau",
  "Niue", NA,
  "Korea, Demokratische Volksrepublik", "Korea, Nord (Nordkorea)",
  "Palästina", NA,
  "Cabo Verde", "Kap Verde", # ?
  "Aruba", NA,
  "Brunei Darussalam", "Brunei",
  "Demokratische Republik Kongo", "Kongo, Demokratische Republik",
  "Macao", NA,
  "Weißrussland", "Belarus",
  "Sint Maarten (Niederländischer Teil)", NA,
  "Britische Jungferninseln", NA,
  "Syrische Arabische Republik","Syrien",
  "Turks- und Caicosinseln", NA
  #"EU (27), seit 2020",
  #"EU (28)",
  #"OECD (Europa)",
  #"OECD (Total)",
  #"OECD (Durchschnitt)",

)


missing_countries <- mapping_dt %>%
  dplyr::filter(is.na(mapped_land) &
                  land %in% missing_countries) %>%
  dplyr::pull(land)



logger::log_info(length(missing_countries),
                 " countries could not be mapped manually yet")

countries_names <- rbind(
  countries_raw_en %>%
  # adding missing countries
  dplyr::filter(name %in% mapping_dt$mapped_land) %>%
  dplyr::left_join(mapping_dt, by = dplyr::join_by(name == mapped_land)) %>%
  dplyr::select(-name),
  countries_raw_en %>%
    dplyr::rename(land = name)
)




# CHECKS
check_land <- mapping_dt %>%
  dplyr::filter(is.na(mapped_land)) %>%
  dplyr::pull(land)

i_data <- data_names[1]
for (i_data in data_names) {
  tmp <- get(i_data)
  if ("land" %in% names(tmp)) {
    if (any(check_land %in% tmp$land)) {
      print(i_data)
      print(intersect(check_land, tmp$land))
      print("-----------")

    }
  }
}


# Save data
save(countries_names, file = "data/countries_names.rda")
