#' beruf_arbeitsmarkt_anforderungen_gender UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_beruf_arbeitsmarkt_anforderungen_gender_ui <- function(id){
  ns <- NS(id)
  tagList(
    # Zeit rausgenommen, da in detailliertem Datensatz  nur 2021 vorliegt

    # p("Auswahl des Jahres:"),
    # shinyWidgets::sliderTextInput(
    #   inputId = ns("date_arbeitsmarkt_anforderungen_gender"),
    #   label = NULL,
    #   choices = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021),
    #   selected = 2021
    # ),
    # p("Auswahl der Beschäftigungsform:"),
    # shinyWidgets::radioGroupButtons(
    #   inputId = ns("level_arbeitsmarkt_anforderungen_gender"),
    #   choices = c("Auszubildende", "Beschäftigte"),
    #   justified = TRUE,
    #   checkIcon = list(yes = icon("ok",
    #                               lib = "glyphicon"))
    # ),

    # Auswahlform zu Dropdown geändert - alle möglichen neuen Indikatore können einfach hier ergänzt werden

    p("Beschäftigungsform:"),
    shinyWidgets::pickerInput(
      inputId = ns("level_arbeitsmarkt_anforderungen_gender"),
      choices = c(
        "Auszubildende",
        "Auszubildende (1. Jahr)",
        "Beschäftigte",
                  "ausländische Beschäftigte"),
      multiple = FALSE,
      selected = "Beschäftigte")

  )
}

#' beruf_arbeitsmarkt_anforderungen_gender Server Functions
#'
#' @noRd
mod_beruf_arbeitsmarkt_anforderungen_gender_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    # observeEvent(input$date_arbeitsmarkt_anforderungen_gender, {
    #   r$date_arbeitsmarkt_anforderungen_gender <- input$date_arbeitsmarkt_anforderungen_gender
    # })

    observeEvent(input$level_arbeitsmarkt_anforderungen_gender, {
      r$level_arbeitsmarkt_anforderungen_gender <- input$level_arbeitsmarkt_anforderungen_gender
    })

  })
}

## To be copied in the UI
# mod_beruf_arbeitsmarkt_anforderungen_gender_ui("beruf_arbeitsmarkt_anforderungen_gender_1")

## To be copied in the server
# mod_beruf_arbeitsmarkt_anforderungen_gender_server("beruf_arbeitsmarkt_anforderungen_gender_1")
