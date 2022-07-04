#' schule_kurse_verlauf UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_schule_kurse_verlauf_ui <- function(id){
  ns <- NS(id)

  tagList(

    p("Wählen Sie einen Zeitraum:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_kurse_verlauf"),
      label = NULL,
      choices = c("2013", "2014", "2015", "2016", "2017",
                  "2018","2019", "2020"),
      selected = c("2015", "2020")
    ),
    p("Wählen Sie in welcher Form der Kurs belegt wurde:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("indikator_kurse_verlauf"),
      choices = c("Grundkurse", "Leistungskurse"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
    ),

    p("Wählen Sie ein Fach:"),
    shinyWidgets::pickerInput(
      inputId = ns("subject_selected"),
      choices = c("MINT (aggregiert)",
                  "Mathematik",
                  "Informatik",
                  "Physik",
                  "Chemie",
                  "Biologie",
                  "andere Fächer (aggregiert)",
                  "Deutsch",
                  "Fremdsprachen",
                  "Gesellschaftswissenschaften",
                  "Musik/Kunst",
                  "Religion/Ethik",
                  "Sport"),
      selected = "MINT (aggregiert)"
    ),

    p("Wählen Sie ein oder mehrere Bundesländer:"),
    shinyWidgets::pickerInput(
      inputId = ns("states_kurse_verlauf"),
      choices = c("Deutschland",
                  "Bayern",
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
                  "Thüringen",
                  "Westen",
                  "Osten"),
      multiple = TRUE,
      options = list(`actions-box` = TRUE,
                     `deselect-all-text` = "Alle abwählen",
                     `select-all-text` = "Alle auswählen"),
      selected = c("Hessen", "Hamburg")
    )
  )

}

#' schule_kurse_verlauf Server Functions
#'
#' @noRd
mod_schule_kurse_verlauf_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_kurse_verlauf, {
      r$date_kurse_verlauf <- input$date_kurse_verlauf
    })

    observeEvent(input$indikator_kurse_verlauf, {
      r$indikator_kurse_verlauf <- input$indikator_kurse_verlauf
    })

    observeEvent(input$topic_kurse_verlauf, {
      r$topic_kurse_verlauf <- input$topic_kurse_verlauf
    })

    observeEvent(input$ost_west, {
      r$ost_west <- input$ost_west
    })

    observeEvent(input$states_kurse_verlauf, {
      r$states_kurse_verlauf <- input$states_kurse_verlauf
    })

    observeEvent(input$subjects_aggregated, {
      r$subjects_aggregated <- input$subjects_aggregated
    })

    observeEvent(input$subject_selected, {
      r$subject_selected <- input$subject_selected
    })

  })
}

## To be copied in the UI
# mod_schule_kurse_verlauf_ui("schule_kurse_verlauf_1")

## To be copied in the server
# mod_schule_kurse_verlauf_server("schule_kurse_verlauf_1")
