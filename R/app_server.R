#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  # Alle Bereiche / home
  load(file = system.file(package="datalab","data/zentral.rda"))

  load(file = system.file(package="datalab","data/zentral_alt.rda"))

  load(file = system.file(package="datalab","data/zentral_neu.rda"))

  # Schule
  load(file = system.file(package="datalab","data/kurse.rda"))

  load(file=system.file(package = "datalab", "data/iqb.rda"))

  load(file=system.file(package="datalab", "data/iqb_standard.rda"))

  load(file=system.file(package="datalab", "data/iqb_score.rda"))

  load(file=system.file(package="datalab", "data/ausserschulisch_skf.rda"))

  # Studium
  load(file = system.file(package="datalab","data/studierende.rda"))

  load(file = system.file(package="datalab","data/studierende_faecher.rda"))

  load(file = system.file(package="datalab","data/studierende_faecher_alle_indi.rda"))

  load(file = system.file(package="datalab","data/studierende2.rda"))


  # Arbeitsmarkt
  load(file = system.file(package="datalab","data/arbeitsmarkt.rda"))

  load(file = system.file(package="datalab","data/arbeitsmarkt_detail.rda"))

  load(file = system.file(package="datalab","data/data_naa.rda"))


  # example_data <- mod_load_data_server("beispieldatensatz", path=system.file(package="datalab", "data-raw/beispieldatensatz.xlsx"))

  # callModule(mod_home_server, "home_ui_1", data=example_data)
  mod_home_server("home_ui_1", data_zentral = zentral,  data_zentral_alt = zentral_alt, data_zentral_neu = zentral_neu, data_ausbildungsvertraege = data_naa)

  # callModule(mod_schule_server, "schule_ui_1", data=example_data, filter_name="schule")
  mod_schule_server("schule_ui_1", data_kurse = kurse, data_iqb = iqb, data_iqb_standard = iqb_standard, data_iqb_score = iqb_score, data_skf = ausserschulisch_skf)

  mod_studium_server("studium_ui_1", data_studierende = studierende, data_studierende2 = studierende2, data_studierende_faecher = studierende_faecher,
                     data_studierende_faecher_alle_indi = studierende_faecher_alle_indi
                     #data_studierende_faecher_alle_indi2 = studierende_faecher_alle_indi2
                     )

  mod_beruf_server("beruf_ui_1", data_arbeitsmarkt = arbeitsmarkt, data_arbeitsmarkt_detail = arbeitsmarkt_detail, data_naa = data_naa)

  mod_ausbildung_server("ausbildung_ui_1", data_ausbildungsvertraege = data_naa)


}
