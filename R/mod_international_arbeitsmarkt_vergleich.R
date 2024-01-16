#' international_arbeitsmarkt_vergleich UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_international_arbeitsmarkt_vergleich_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Jahr:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("vergleich_y_int_arbeitsmarkt"),
      label = NULL,
      choices = international_ui_years(region = "arbeit"),
      selected = "2020"
    ),
    p("Länder:"),
    tags$head(
      tags$style(HTML("
      .bootstrap-select .dropdown-menu.inner {
        max-height: 300px;  /* Adjust this value to achieve the desired height */
        overflow-y: auto;   /* Enable vertical scrolling */
      }
    "))
    ),
    shinyWidgets::pickerInput(
      inputId = ns("vergleich_l_int_arbeitsmarkt"),
      choices = international_ui_country(n = NA_integer_),
      selected = international_ui_country(n = 10),
      options = list(`actions-box` = TRUE,
                     `live-search` = TRUE),
      multiple = TRUE
    ),
    p("Fachbereich:"),
    shinyWidgets::pickerInput(
      inputId = ns("vergleich_f_int_arbeitsmarkt"),
      choices = international_ui_faecher(region = "arbeit"),
      selected = "MINT",
      multiple = FALSE
    )
  )
}

#' international_arbeitsmarkt_vergleich Server Functions
#'
#' @noRd
mod_international_arbeitsmarkt_vergleich_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    observeEvent(input$vergleich_y_int_arbeitsmarkt, {
      r$vergleich_y_int_arbeitsmarkt <- input$vergleich_y_int_arbeitsmarkt
    })

    observeEvent(input$vergleich_l_int_arbeitsmarkt, {
      r$vergleich_l_int_arbeitsmarkt <- input$vergleich_l_int_arbeitsmarkt
    })

    observeEvent(input$vergleich_f_int_arbeitsmarkt, {
      r$vergleich_f_int_arbeitsmarkt <- input$vergleich_f_int_arbeitsmarkt
    })


  })
}

## To be copied in the UI
# mod_international_arbeitsmarkt_vergleich_ui("international_arbeitsmarkt_vergleich_1")

## To be copied in the server
# mod_international_arbeitsmarkt_vergleich_server("international_arbeitsmarkt_vergleich_1")
