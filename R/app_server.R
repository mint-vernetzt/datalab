#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  require(logger)
  VERSION <- packageVersion("datalab")
  LOG_LEVEL <- Sys.getenv("LOG_LEVEL", "INFO")
  logger::log_threshold(level = LOG_LEVEL)
  logger::log_info("APP VERSION: ", VERSION,
                   " Log-level: ", LOG_LEVEL)


  logger::log_debug("Starte initiale Daten Laden")
  # Alle Bereiche / home
  load(file = system.file(package="datalab","data/zentral.rda"))

  load(file = system.file(package="datalab","data/zentral_alt.rda"))

  load(file = system.file(package="datalab","data/zentral_neu.rda"))

  # Schule
  load(file = system.file(package="datalab","data/kurse.rda"))

  load(file = system.file(package = "datalab", "data/iqb.rda"))

  load(file = system.file(package="datalab", "data/ausserschulisch_skf.rda"))

  # Studium
  load(file = system.file(package="datalab","data/studierende.rda"))

  load(file = system.file(package="datalab","data/studierende_detailliert.rda"))


  # Arbeitsmarkt
  load(file = system.file(package="datalab","data/arbeitsmarkt.rda"))

  load(file = system.file(package="datalab","data/arbeitsmarkt_detail.rda"))

  load(file = system.file(package="datalab","data/data_naa.rda"))


  # International
  load(file = system.file(package="datalab","data/countries_names.rda"))

  load(file = system.file(package="datalab","data/studierende_anzahl_oecd.rda"))

  load(file = system.file(package="datalab","data/studierende_europa.rda"))

  load(file = system.file(package="datalab","data/studierende_absolventen_weltweit.rda"))

  # International Arbeitsmarkt

  load(file = system.file(package="datalab","data/arbeitsmarkt_absolvent_oecd.rda"))

  load(file = system.file(package="datalab","data/arbeitsmarkt_anfänger_absolv_oecd.rda"))

  load(file = system.file(package="datalab","data/arbeitsmarkt_anzahl_azubis_oecd.rda"))

  load(file = system.file(package="datalab","data/arbeitsmarkt_beschäftigte_eu.rda"))

  logger::log_debug("Daten laden fertig")


  # example_data <- mod_load_data_server("beispieldatensatz", path=system.file(package="datalab", "data-raw/beispieldatensatz.xlsx"))

  # callModule(mod_home_server, "home_ui_1", data=example_data)

  logger::log_debug("Lade Seiten-Module")
  mod_home_server("home_ui_1", data_zentral = zentral,  data_zentral_alt = zentral_alt, data_zentral_neu = zentral_neu, data_ausbildungsvertraege = data_naa)

  # callModule(mod_schule_server, "schule_ui_1", data=example_data, filter_name="schule")
  mod_schule_server("schule_ui_1", data_kurse = kurse, data_iqb = iqb, data_skf = ausserschulisch_skf)

  mod_studium_server("studium_ui_1",
                     data_studierende = studierende,
                     data_studierende_detailliert = studierende_detailliert
                     )

  mod_beruf_server("beruf_ui_1", data_arbeitsmarkt = arbeitsmarkt, data_arbeitsmarkt_detail = arbeitsmarkt_detail, data_naa = data_naa)

  mod_ausbildung_server("ausbildung_ui_1", data_ausbildungsvertraege = data_naa)

  mod_international_server("international_ui_1",
                           data_studierende_absolventen_weltweit = studierende_absolventen_weltweit,
                           data_studierende_anzahl_oecd = studierende_anzahl_oecd,
                           data_studierende_europa = studierende_europa,
                           data_countries_names = countries_names,
                           #data_studierende_intern_oecd = studierende_intern_oecd
                           #
                           data_arbeitsmarkt_absolvent_oecd = arbeitsmarkt_absolvent_oecd, # Anzahl
                           data_arbeitsmarkt_anfänger_absolv_oecd = arbeitsmarkt_anfänger_absolv_oecd, # Anteil
                           data_arbeitsmarkt_anzahl_azubis_oecd = arbeitsmarkt_anzahl_azubis_oecd,
                           data_arbeitsmarkt_beschäftigte_eu = arbeitsmarkt_beschäftigte_eu
                           )

  logger::log_debug("Seiten-Module fertig")

  output$debug <- shiny::renderUI({
    shiny::actionButton(
      "debug",
      label = "Debug",
      icon = shiny::icon("bug"),
      class = "btn btn-petrol",
      width = "120px",
      style = "border: 0.1px solid #111111; background-color: #111111; color: #ffffff; ")
  })

  shiny::observeEvent(input$debug, browser())

}
