#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  # Alte Logik

   mod_startseite_server("startseite_ui_1")

  mod_argumentation_server("argumentationshilfe_ui_1")

  # callModule(mod_home_server, "home_ui_1", data=example_data)
  mod_home_server("home_ui_1")

  # callModule(mod_schule_server, "schule_ui_1", data=example_data, filter_name="schule")
  mod_schule_server("schule_ui_1")

  mod_studium_server("studium_ui_1")

  mod_beruf_server("beruf_ui_1")

  mod_international_server("international_ui_1")

  mod_fachkraft_server("fachkraft_ui_1")

  mod_ausserschulisch_server("ausserschulisch_ui_1")



}
