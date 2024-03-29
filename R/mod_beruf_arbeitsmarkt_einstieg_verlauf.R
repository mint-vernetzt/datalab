#' beruf_arbeitsmarkt_einstieg_verlauf UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_beruf_arbeitsmarkt_einstieg_verlauf_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Jahre:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_arbeitsmarkt_einstieg_verlauf"),
      label = NULL,
      choices = c("2013", "2014", "2015", "2016", "2017",
                  "2018","2019", "2020", "2021", "2022"),
      selected = c("2017", "2022")
    ),
    p("Betrachtung:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("abs_zahlen_arbeitsmarkt_einstieg_verlauf"),
      choices = c("In Prozent", "Anzahl"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
    ),
    br(),
    shinyBS::bsPopover(id="ih_beruf_mint_2", title="",
                       content = paste0("In der ersten Einstellung ist zu sehen, dass der Anteil an Auszubildenden in MINT von 2020 auf 2021 leicht sinkt. Betrachtet man die absolute Anzahl, sieht man, dass es deutschlandweit 2022 ca. 50.000 Auszubildende weniger in MINT gibt als noch 2020."),
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_beruf_mint_2")
  )

}

#' beruf_arbeitsmarkt_einstieg_verlauf Server Functions
#'
#' @noRd
mod_beruf_arbeitsmarkt_einstieg_verlauf_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$date_arbeitsmarkt_einstieg_verlauf, {
      r$date_arbeitsmarkt_einstieg_verlauf <- input$date_arbeitsmarkt_einstieg_verlauf
    })

    observeEvent(input$abs_zahlen_arbeitsmarkt_einstieg_verlauf, {
      r$abs_zahlen_arbeitsmarkt_einstieg_verlauf <- input$abs_zahlen_arbeitsmarkt_einstieg_verlauf
    })


  })
}

## To be copied in the UI
# mod_beruf_arbeitsmarkt_einstieg_verlauf_ui("beruf_arbeitsmarkt_einstieg_verlauf_1")

## To be copied in the server
# mod_beruf_arbeitsmarkt_einstieg_verlauf_server("beruf_arbeitsmarkt_einstieg_verlauf_1")
