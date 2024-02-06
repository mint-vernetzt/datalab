#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  # Alte Logik

  # callModule(mod_home_server, "home_ui_1", data=example_data)
  mod_home_server("home_ui_1")

  # callModule(mod_schule_server, "schule_ui_1", data=example_data, filter_name="schule")
  mod_schule_server("schule_ui_1")

  mod_studium_server("studium_ui_1")

  mod_beruf_server("beruf_ui_1")

  # Lazy Loading Logik

  # shinyjs::enable("tabs")  # Dies ermöglicht das dynamische Umschalten von Registerkarten mit shinyjs
  #
  # observe({
  #   # Hier können Sie bedingte Logik basierend auf der ausgewählten Registerkarte implementieren
  #   if (input$tabs == "home") {
  #
  #     mod_home_server("home_ui_1")
  #     shinyjs::disable("schule")  # Hier können Sie andere Registerkarten deaktivieren, wenn nötig
  #     shinyjs::disable("studium")
  #     shinyjs::disable("beruf")
  #
  #   } else if (input$tabs == "schule") {
  #
  #     mod_schule_server("schule_ui_1")
  #     shinyjs::disable("home")
  #     shinyjs::disable("studium")
  #     shinyjs::disable("beruf")
  #
  #   } else if (input$tabs == "studium"){
  #
  #     mod_studium_server("studium_ui_1")
  #     shinyjs::disable("home")
  #     shinyjs::disable("schule")
  #     shinyjs::disable("beruf")
  #
  #   } else if (input$tabs == "beruf"){
  #
  #     mod_beruf_server("beruf_ui_1")
  #     shinyjs::disable("home")
  #     shinyjs::disable("schule")
  #     shinyjs::disable("studium")
  #   }
  # })
  #
  # shinyjs::runjs("$('#tabs a[href=\"#home\"]').tab('show');")


  # Disconnect funktioniert nicht wenn implementiert bei publishen??

  # session$onSessionEnded(function() {
  #   if (!is.null(con) && DBI::dbIsValid(con)) {
  #     DBI::dbDisconnect(con)
  #   }
  # })
  # shiny::onStop(function() {
  #   if (!is.null(con) && DBI::dbIsValid(con)) {
  #     DBI::dbDisconnect(con)
  #   }
  # })

}
