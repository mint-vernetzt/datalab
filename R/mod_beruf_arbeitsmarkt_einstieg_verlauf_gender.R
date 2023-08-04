#' beruf_arbeitsmarkt_einstieg_verlauf_gender UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_beruf_arbeitsmarkt_einstieg_verlauf_gender_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Jahre:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_arbeitsmarkt_einstieg_verlauf_gender"),
      label = NULL,
      choices = c("2013", "2014", "2015", "2016", "2017",
                  "2018","2019", "2020", "2021", "2022"),
      selected = c("2017", "2022")
    ),
    p("Betrachtung:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("abs_zahlen_arbeitsmarkt_einstieg_verlauf_gender"),
      choices = c("In Prozent", "Anzahl"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
    ),
    br(),
    shinyBS::bsPopover(id="ih_beruf_frauen_2", title="",
                       content = paste0("Betrachten man den Zeitverlauf in der ersten Einstellung sieht man, dass der Frauenanteil in MINT-Ausbildungen und -Berufen seit 2018 etwas gestiegen ist. Wechselt man in der Betrachtung auf &quotAnzahl&quot kann man die Entwicklung der absoluten Anzahl von Frauen in MINT betrachten."),
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_beruf_frauen_2")
  )

}

#' beruf_arbeitsmarkt_einstieg_verlauf_gender Server Functions
#'
#' @noRd
mod_beruf_arbeitsmarkt_einstieg_verlauf_gender_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_arbeitsmarkt_einstieg_verlauf_gender, {
      r$date_arbeitsmarkt_einstieg_verlauf_gender <- input$date_arbeitsmarkt_einstieg_verlauf_gender
    })

    observeEvent(input$abs_zahlen_arbeitsmarkt_einstieg_verlauf_gender, {
      r$abs_zahlen_arbeitsmarkt_einstieg_verlauf_gender <- input$abs_zahlen_arbeitsmarkt_einstieg_verlauf_gender
    })

  })
}

## To be copied in the UI
# mod_beruf_arbeitsmarkt_einstieg_verlauf_gender_ui("beruf_arbeitsmarkt_einstieg_verlauf_gender_1")

## To be copied in the server
# mod_beruf_arbeitsmarkt_einstieg_verlauf_gender_server("beruf_arbeitsmarkt_einstieg_verlauf_gender_1")
