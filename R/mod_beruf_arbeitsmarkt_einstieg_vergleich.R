#' beruf_arbeitsmarkt_einstieg_vergleich UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_beruf_arbeitsmarkt_einstieg_vergleich_ui <- function(id){
  ns <- NS(id)
  tagList(
    # p("Auswahl des Jahres:"),
    # shinyWidgets::sliderTextInput(
    #   inputId = ns("date_arbeitsmarkt_einstieg_vergleich"),
    #   label = NULL,
    #   choices = c("2013", "2014", "2015", "2016", "2017",
    #               "2018","2019", "2020", "2021"),
    #   selected = "2021"
    # )

    shinyBS::bsPopover(id="ih_beruf_mint_3", title="",
                       content = paste0("Die Darstellung zeigt, dass der MINT-Anteil in der Gruppe der Auszubildenden mit knapp einem Drittel vergleichsweise hoch ist. Am wenigsten groß ist der Anteil an Beschäftigungen in MINT im Minijob."),
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_beruf_mint_3")

  )
}

#' beruf_arbeitsmarkt_einstieg_vergleich Server Functions
#'
#' @noRd
mod_beruf_arbeitsmarkt_einstieg_vergleich_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    # observeEvent(input$date_arbeitsmarkt_einstieg_vergleich, {
    #   r$date_arbeitsmarkt_einstieg_vergleich <- input$date_arbeitsmarkt_einstieg_vergleich
   # })

  })
}

## To be copied in the UI
# mod_beruf_arbeitsmarkt_einstieg_vergleich_ui("beruf_arbeitsmarkt_einstieg_vergleich_1")

## To be copied in the server
# mod_beruf_arbeitsmarkt_einstieg_vergleich_server("beruf_arbeitsmarkt_einstieg_vergleich_1")
