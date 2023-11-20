#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic


  # callModule(mod_home_server, "home_ui_1", data=example_data)
  mod_home_server("home_ui_1")

  # callModule(mod_schule_server, "schule_ui_1", data=example_data, filter_name="schule")
  mod_schule_server("schule_ui_1")

  mod_studium_server("studium_ui_1")

  mod_beruf_server("beruf_ui_1")


 # mod_ausbildung_server("ausbildung_ui_1", data_ausbildungsvertraege = data_naa)


}
