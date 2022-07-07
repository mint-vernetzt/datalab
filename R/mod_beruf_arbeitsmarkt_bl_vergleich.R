#' beruf_arbeitsmarkt_bl_vergleich UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_beruf_arbeitsmarkt_bl_vergleich_ui <- function(id){
  ns <- NS(id)
  tagList(

    p("Auswahl Jahr:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_beruf_arbeitsmarkt_bl_vergleich"),
      label = NULL,
      choices = c("2013", "2014", "2015", "2016", "2017",
                  "2018","2019", "2020"),
      selected = "2020"
    ),
    p("Wählen Sie eine Beschäftigungsform der Arbeitnehmer*innen:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("indikator_beruf_arbeitsmarkt_bl_vergleich"),
      choices = c("Auszubildende", "Beschäftigte"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
    ),
    p("Auswahl Beschäftigungstyp (Anforderungsniveau):"),
    shinyWidgets::pickerInput(
      inputId = ns("anforderungsniveau_beruf_arbeitsmarkt_bl_vergleich"),
      choices = c("Gesamt", "Fachkraft", "Spezialist", "Experte"),
      selected = "Gesamt"
    )
  )
}

#' beruf_arbeitsmarkt_bl_vergleich Server Functions
#'
#' @noRd
mod_beruf_arbeitsmarkt_bl_vergleich_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_beruf_arbeitsmarkt_bl_vergleich, {
      r$date_beruf_arbeitsmarkt_bl_vergleich <- input$date_beruf_arbeitsmarkt_bl_vergleich
    })

    observeEvent(input$indikator_beruf_arbeitsmarkt_bl_vergleich, {
      r$indikator_beruf_arbeitsmarkt_bl_vergleich <- input$indikator_beruf_arbeitsmarkt_bl_vergleich
    })

    observeEvent(input$anforderungsniveau_beruf_arbeitsmarkt_bl_vergleich, {
      r$anforderungsniveau_beruf_arbeitsmarkt_bl_vergleich <- input$anforderungsniveau_beruf_arbeitsmarkt_bl_vergleich
    })

  })
}

## To be copied in the UI
# mod_beruf_arbeitsmarkt_bl_vergleich_ui("beruf_arbeitsmarkt_bl_vergleich_1")

## To be copied in the server
# mod_beruf_arbeitsmarkt_bl_vergleich_server("beruf_arbeitsmarkt_bl_vergleich_1")
