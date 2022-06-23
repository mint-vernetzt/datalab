#' beruf_arbeitsmarkt_anforderungen_gender_vergleich UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_beruf_arbeitsmarkt_anforderungen_gender_vergleich_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Wählen Sie einen Zeitpunkt:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_arbeitsmarkt_anforderungen_gender_vegleich"),
      label = NULL,
      choices = c("2013", "2014", "2015", "2016", "2017",
                  "2018","2019", "2020"),
      selected = "2020"
    ),
    p("Wählen Sie ein Bundesland:"),
    shinyWidgets::pickerInput(
      inputId = ns("states_arbeitsmarkt_anforderungen_gender_vegleich"),
      choices = c("Berlin",
                  "Brandenburg",
                  "Bremen",
                  "Hamburg",
                  "Hessen",
                  "Mecklenburg-Vorpommern",
                  "Niedersachsen",
                  "Nordrhein-Westfalen",
                  "Rheinland-Pfalz",
                  "Saarland",
                  "Sachsen",
                  "Sachsen-Anhalt",
                  "Schleswig-Holstein",
                  "Thüringen",
                  "Westen",
                  "Osten"),
      multiple = FALSE,
      selected = c("Hessen")
    )
  )
}

#' beruf_arbeitsmarkt_anforderungen_gender_vergleich Server Functions
#'
#' @noRd
mod_beruf_arbeitsmarkt_anforderungen_gender_vergleich_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_arbeitsmarkt_anforderungen_gender_vegleich, {
      r$date_arbeitsmarkt_anforderungen_gender_vegleich <- input$date_arbeitsmarkt_anforderungen_gender_vegleich
    })

    observeEvent(input$states_arbeitsmarkt_anforderungen_gender_vegleich, {
      r$states_arbeitsmarkt_anforderungen_gender_vegleich <- input$states_arbeitsmarkt_anforderungen_gender_vegleich
    })

  })
}

## To be copied in the UI
# mod_beruf_arbeitsmarkt_anforderungen_gender_vergleich_ui("beruf_arbeitsmarkt_anforderungen_gender_vergleich_1")

## To be copied in the server
# mod_beruf_arbeitsmarkt_anforderungen_gender_vergleich_server("beruf_arbeitsmarkt_anforderungen_gender_vergleich_1")
