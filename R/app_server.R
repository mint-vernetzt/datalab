#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  load(file = system.file(package="datalab","data/example_data.rda"))

  # example_data <- mod_load_data_server("beispieldatensatz", path=system.file(package="datalab", "data-raw/beispieldatensatz.xlsx"))

  # callModule(mod_home_server, "home_ui_1", data=example_data)
  mod_home_server("home_ui_1", data=example_data)

  # callModule(mod_schule_server, "schule_ui_1", data=example_data, filter_name="schule")
  mod_schule_server("schule_ui_1", data=example_data, filter_name="schule")

}
