#' ausbildung_vertraege_verlauf UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_ausbildung_vertraege_verlauf_ui <- function(id){
  ns <- NS(id)

  load(file = system.file(package="datalab","data/data_naa.rda"))

  tagList(
    p("Bereich der neuen Auszubildenden:"),
    shinyWidgets::pickerInput(
      inputId = ns("indikator_ausbildung_verlauf"),
      choices = unique(data_naa$fachbereich),
      selected = "Informatik-Fachkräfte"
    ),
    p("Sollen die Bundesländer auf Ost und West zusammengefasst werden?"),
    tags$div(
      shinyWidgets::materialSwitch(inputId = ns("ost_west"), label = "Nein", inline = TRUE),
      tags$span("Ja")
    ),
    # shinyWidgets::radioGroupButtons(
    #   inputId = ns("ost_west"),
    #   choices = c("Ja", "Nein"),
    #   selected = "Nein"
    # ),
    conditionalPanel(condition = "input.ost_west == false",
                     ns = ns,
                     p("Wählen Sie ein oder mehrere Bundesländer:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("states_ausbildung_verlauf"),
                       choices = c("Deutschland",
                                   "Berlin",
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
                                   "Thüringen"),
                       multiple = TRUE,
                       options = list(`actions-box` = TRUE,
                                      `deselect-all-text` = "Alle abwählen",
                                      `select-all-text` = "Alle auswählen"),
                       selected = c("Hessen", "Hamburg")
                     )),
    conditionalPanel(condition = "input.ost_west != false",
                     ns = ns)
  )
}

#' ausbildung_vertraege_verlauf Server Functions
#'
#' @noRd
mod_ausbildung_vertraege_verlauf_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$indikator_ausbildung_verlauf, {
      r$indikator_ausbildung_verlauf <- input$indikator_ausbildung_verlauf
    })

    observeEvent(input$states_ausbildung_verlauf, {
      r$states_ausbildung_verlauf <- input$states_ausbildung_verlauf
    })

    observeEvent(input$ost_west, {
      r$ost_west <- input$ost_west
    })

  })
}

## To be copied in the UI
# mod_ausbildung_vertraege_verlauf_ui("ausbildung_vertraege_verlauf_1")

## To be copied in the server
# mod_ausbildung_vertraege_verlauf_server("ausbildung_vertraege_verlauf_1")
