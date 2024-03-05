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
  #
  # # callModule(mod_home_server, "home_ui_1", data=example_data)
  # mod_home_server("home_ui_1")
  #
  # # callModule(mod_schule_server, "schule_ui_1", data=example_data, filter_name="schule")
  # mod_schule_server("schule_ui_1")
  #
  # mod_studium_server("studium_ui_1")
  #
  # mod_beruf_server("beruf_ui_1")
  #
  # mod_international_server("international_ui_1")
  #
  # mod_fachkraft_server("fachkraft_ui_1")

  # react_search <- reactiveValues()
  # mod_suche_server("suche_1", react_search, parent_session = session)
  # mod_suche_eingabe_server("suche_eingabe_1", react_search, parent_session = session)
  # mod_suche_eingabe_server("suche_eingabe_2", react_search, parent_session = session)

  # Lazy Loading Logik

  shinyjs::enable("tabs")  # Dies ermöglicht das dynamische Umschalten von Registerkarten mit shinyjs

  observe({
    # Hier können Sie bedingte Logik basierend auf der ausgewählten Registerkarte implementieren
    if (input$tabs == "home") {

      mod_home_server("home_ui_1")
      shinyjs::disable("schule")  # Hier können Sie andere Registerkarten deaktivieren, wenn nötig
      shinyjs::disable("studium")
      shinyjs::disable("beruf")
      shinyjs::disable("international")
      shinyjs::disable("fachkraft")


    } else if (input$tabs == "schule") {

      mod_schule_server("schule_ui_1")
      shinyjs::disable("home")
      shinyjs::disable("studium")
      shinyjs::disable("beruf")
      shinyjs::disable("international")
      shinyjs::disable("fachkraft")

    } else if (input$tabs == "studium"){

      mod_studium_server("studium_ui_1")
      shinyjs::disable("home")
      shinyjs::disable("schule")
      shinyjs::disable("beruf")
      shinyjs::disable("international")
      shinyjs::disable("fachkraft")

    } else if (input$tabs == "beruf"){

      mod_beruf_server("beruf_ui_1")
      shinyjs::disable("home")
      shinyjs::disable("schule")
      shinyjs::disable("studium")
      shinyjs::disable("international")
      shinyjs::disable("fachkraft")

    } else if (input$tabs == "fachkraft"){

      mod_fachkraft_server("fachkraft_ui_1")
      shinyjs::disable("home")
      shinyjs::disable("schule")
      shinyjs::disable("studium")
      shinyjs::disable("beruf")
      shinyjs::disable("international")

    }else if (input$tabs == "international"){

      mod_international_server("international_ui_1")
      shinyjs::disable("home")
      shinyjs::disable("schule")
      shinyjs::disable("studium")
      shinyjs::disable("beruf")
      shinyjs::disable("fachkraft")
    }
  })

  shinyjs::runjs("$('#tabs a[href=\"#home\"]').tab('show');")


  # Disconnect funktioniert nicht wenn implementiert bei publishen??

  # session$onSessionEnded(function() {
  #   if (!is.null(con) && DBI::dbIsValid(con)) {
  #     print("Disconnecting from DuckDB...")
  #     #DBI::dbDisconnect(con, shutdown = TRUE)
  #     duckdb::dbDisconnect(con)
  #   }else {
  #     print("connection is already invalid or null.")
  #   }
  #   if(DBI::dbIsValid(con)){
  #     print("sollte es nicht mehr sein")
  #   }else{
  #     print("gut!")
  #   }
  # })
  # shiny::onStop(function() {
  #   if (!is.null(con) && DBI::dbIsValid(con)) {
  #     DBI::dbDisconnect(con, shutdown=TRUE)
  #   }
  # })

}
