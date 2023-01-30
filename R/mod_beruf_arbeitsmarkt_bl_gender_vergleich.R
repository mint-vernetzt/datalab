#' beruf_arbeitsmarkt_bl_gender_vergleich UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_beruf_arbeitsmarkt_bl_gender_vergleich_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Auswahl des Jahres:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_arbeitsmarkt_bl_gender_vergleich"),
      label = NULL,
      choices = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021),
      selected = 2021
    )
    # ,
    # p("Auswahl des Anforderungsniveaus:"),
    # shinyWidgets::pickerInput(
    #   inputId = ns("anforderungsniveau_arbeitsmarkt_bl_gender_vergleich"),
    #   choices = c("Gesamt", "Fachkraft", "Spezialist:in"="Spezialist", "Expert:in"="Experte")
    # )
  )
}

#' beruf_arbeitsmarkt_bl_gender_vergleich Server Functions
#'
#' @noRd
mod_beruf_arbeitsmarkt_bl_gender_vergleich_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_arbeitsmarkt_bl_gender_vergleich, {
      r$date_arbeitsmarkt_bl_gender_vergleich <- input$date_arbeitsmarkt_bl_gender_vergleich
    })

    observeEvent(input$anforderungsniveau_arbeitsmarkt_bl_gender_vergleich, {
      r$anforderungsniveau_arbeitsmarkt_bl_gender_vergleich <- input$anforderungsniveau_arbeitsmarkt_bl_gender_vergleich
    })

  })
}

## To be copied in the UI
# mod_beruf_arbeitsmarkt_bl_gender_vergleich_ui("beruf_arbeitsmarkt_bl_gender_vergleich_1")

## To be copied in the server
# mod_beruf_arbeitsmarkt_bl_gender_vergleich_server("beruf_arbeitsmarkt_bl_gender_vergleich_1")
