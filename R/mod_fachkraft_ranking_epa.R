#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_fachkraft_ranking_epa_ui <- function(id){
  ns <- NS(id)
  tagList(


    p("Jahr:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("fachkraft_ranking_epa_1"),
      label = NULL,
      choices = fachkraft_ui_years(reg="DE"),
      selected = "2023"
    ),

    # p("Fachbereich:"),
    # shinyWidgets::pickerInput(
    #   inputId = ns("fachkraft_ranking_epa_2"),
    #   choices = fachkraft_ui_faecher(),
    #   selected = c("MINT gesamt", "Nicht MINT"),
    #   multiple = TRUE,
    #   options =  list(
    #     "max-options" = 2,
    #     "max-options-text" = "<span style='color: red;'>Maximal 2 Indikatoren auswählen</span>")
    # ),

    p("Berufslevel:"),
    shinyWidgets::pickerInput(
      inputId = ns("fachkraft_ranking_epa_3"),
      choices = fachkraft_ui_berufslevel(),
      selected = c("Gesamt"),
      multiple = FALSE
    ),

    br(),



  )
}

#' fachkraft_item_epa Server Functions
#'
#' @noRd
mod_fachkraft_ranking_epa_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    observeEvent(input$fachkraft_ranking_epa_1, {
      r$fachkraft_ranking_epa_1 <- input$fachkraft_ranking_epa_1
    })

    # observeEvent(input$fachkraft_ranking_epa_2, {
    #   r$fachkraft_ranking_epa_2 <- input$fachkraft_ranking_epa_2
    # })

    observeEvent(input$fachkraft_ranking_epa_3, {
      r$fachkraft_ranking_epa_3 <- input$fachkraft_ranking_epa_3
    })


  })
}

## To be copied in the UI
# mod_fachkraft_item_epa_ui("fachkraft_item_epa_1")

## To be copied in the server
# mod_fachkraft_item_epa_server("fachkraft_item_epa_1")
