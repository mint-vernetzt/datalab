#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  #Sys.setlocale(category = "LC_ALL", locale = "German_Germany.utf8")

  con <<- DBI::dbConnect(RSQLite::SQLite(), "data/mint_db.sqlite", encoding = "UTF-8")


  # callModule(mod_home_server, "home_ui_1", data=example_data)
  mod_home_server("home_ui_1")

  # callModule(mod_schule_server, "schule_ui_1", data=example_data, filter_name="schule")
  mod_schule_server("schule_ui_1")

 # mod_studium_server("studium_ui_1")

 # mod_beruf_server("beruf_ui_1")


 # mod_ausbildung_server("ausbildung_ui_1", data_ausbildungsvertraege = data_naa)

  load(file = system.file(package="datalab","data/arbeitsmarkt_absolvent_oecd.rda"))

  load(file = system.file(package="datalab","data/arbeitsmarkt_anfaenger_absolv_oecd.rda"))

  load(file = system.file(package="datalab","data/arbeitsmarkt_anzahl_azubis_oecd.rda"))

  load(file = system.file(package="datalab","data/arbeitsmarkt_beschaeftigte_eu.rda"))

  load(file = system.file(package="datalab","data/suchtabelle.rda"))

  mod_beruf_server("beruf_ui_1",
                   data_arbeitsmarkt_absolvent_oecd = arbeitsmarkt_absolvent_oecd, # Anzahl
                   data_arbeitsmarkt_anfaenger_absolv_oecd = arbeitsmarkt_anfaenger_absolv_oecd, # Anteil
                   data_arbeitsmarkt_anzahl_azubis_oecd = arbeitsmarkt_anzahl_azubis_oecd,
                   data_arbeitsmarkt_beschaeftigte_eu = arbeitsmarkt_beschaeftigte_eu
  )

  mod_studium_server("studium_ui_1",
                     data_studierende_absolventen_weltweit = studierende_absolventen_weltweit,
                     data_studierende_anzahl_oecd = studierende_anzahl_oecd,
                     data_studierende_europa = studierende_europa,
                     data_studierende_mobil_eu_absolut= studierende_mobil_eu_absolut,
                     data_countries_names = countries_names
  )

}
