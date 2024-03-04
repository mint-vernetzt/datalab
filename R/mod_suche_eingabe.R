#' suche_eingabe UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_suche_eingabe_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::div(
      id = "search-input-btn",
      style = "margin: 10px 10px -10px 0px;",
      shinyWidgets::searchInput(
        inputId = ns("suche_eingabe_txt"),
        label = NULL,#"Click search icon to update or hit 'Enter'",
        placeholder = "Durchsuchen Sie das MINT-DataLab...",
        btnSearch = icon("search"),
        btnReset = icon("remove"),
        width = "400px"
      )
    )
  )
}

#' suche_eingabe Server Functions
#'
#' @noRd
mod_suche_eingabe_server <- function(id, react_search, parent_session){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$suche_eingabe_txt, {
      react_search$suche_eingabe_txt <- input$suche_eingabe_txt
      react_search$suchtabelle <- get_search_data(
        term = input$suche_eingabe_txt,
        session = parent_session)


      if (input$suche_eingabe_txt != "") {
        shinydashboard::updateTabItems(
          session = parent_session,
          inputId = "tabs",
          selected = "suche")
      }

      shinyWidgets::updateSearchInput(
        session,
        inputId = "suche_eingabe_txt",
        value = ""
      )

    })

  })
}

## To be copied in the UI
# mod_suche_eingabe_ui("suche_eingabe_1")

## To be copied in the server
# mod_suche_eingabe_server("suche_eingabe_1")
