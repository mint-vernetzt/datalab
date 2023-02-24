#' beruf_arbeitsmarkt_bl_gender UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_beruf_arbeitsmarkt_bl_gender_ui <- function(id){
  ns <- NS(id)
  tagList(
    # p("Auswahl des Jahres:"),
    # shinyWidgets::sliderTextInput(
    #   inputId = ns("date_arbeitsmarkt_bl_gender"),
    #   label = NULL,
    #   choices = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021),
    #   selected = 2021
    # ),
    p("Auswahl der Beschäftigungsform:"),
    shinyWidgets::pickerInput(
      inputId = ns("level_arbeitsmarkt_bl_gender"),
      choices = c("Auszubildende",
                  "Auszubildende (1. Jahr)",
                  "Beschäftigte",
                  "ausländische Beschäftigte"),
      multiple = FALSE,
      selected = "Beschäftigte"),

    p("Auswahl der Berufsfachgruppe:"),
    shinyWidgets::pickerInput(
      inputId = ns("fach_arbeitsmarkt_bl_gender"),
      choices = c("MINT",
                  "Mathematik, Naturwissenschaften",
                  "Informatik",
                  "Technik (gesamt)",
                  "Andere Berufsgruppen"),
      multiple = FALSE,
      selected = "MINT")

    # ,
    # p("Auswahl des Anforderungsniveaus:"),
    # shinyWidgets::pickerInput(
    #   inputId = ns("anforderungsniveau_arbeitsmarkt_bl_gender"),
    #   choices = c("Gesamt", "Fachkraft", "Spezialist:in"="Spezialist", "Expert:in"="Experte")
    # )
  )
}

#' beruf_arbeitsmarkt_bl_gender Server Functions
#'
#' @noRd
mod_beruf_arbeitsmarkt_bl_gender_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    # observeEvent(input$date_arbeitsmarkt_bl_gender, {
    #   r$date_arbeitsmarkt_bl_gender <- input$date_arbeitsmarkt_bl_gender
    # })

    # observeEvent(input$anforderungsniveau_arbeitsmarkt_bl_gender, {
    #   r$anforderungsniveau_arbeitsmarkt_bl_gender <- input$anforderungsniveau_arbeitsmarkt_bl_gender
    # })

    observeEvent(input$level_arbeitsmarkt_bl_gender, {
      r$level_arbeitsmarkt_bl_gender <- input$level_arbeitsmarkt_bl_gender
    })

    observeEvent(input$fach_arbeitsmarkt_bl_gender, {
      r$fach_arbeitsmarkt_bl_gender <- input$fach_arbeitsmarkt_bl_gender
    })

  })
}

## To be copied in the UI
# mod_beruf_arbeitsmarkt_bl_gender_ui("beruf_arbeitsmarkt_bl_gender_1")

## To be copied in the server
# mod_beruf_arbeitsmarkt_bl_gender_server("beruf_arbeitsmarkt_bl_gender_1")
