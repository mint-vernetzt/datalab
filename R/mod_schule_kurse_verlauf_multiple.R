#' schule_kurse_verlauf_multiple UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_schule_kurse_verlauf_multiple_ui <- function(id){
  ns <- NS(id)
  tagList(

    p("Auswahl des Zeitraums:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_kurse_verlauf_multiple"),
      label = NULL,
      choices = c("2013", "2014", "2015", "2016", "2017",
                  "2018","2019", "2020"),
      selected = c("2015", "2020")
    ),
    p("Form der Kursbelegung:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("topic_selected_multiple"),
      choices = c("Grundkurse", "Leistungskurse"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
    ),
    p("Auswahl des Fachs:"),
    shinyWidgets::pickerInput(
      inputId = ns("subject_selected_multiple"),
      choices = c("MINT-Fächer (gesamt)",
                  "Mathematik",
                  "Informatik",
                  "Physik",
                  "Chemie",
                  "Biologie",
                  "andere Fächer (gesamt)",
                  "Deutsch",
                  "Fremdsprachen",
                  "Gesellschaftswissenschaften",
                  "Musik/Kunst",
                  "Religion/Ethik",
                  "Sport"),
      selected = "MINT-Fächer (gesamt)",
    ),
    p("Auswahl eines oder mehrerer Bundesländer:"),
    shinyWidgets::pickerInput(
      inputId = ns("states_kurse_verlauf_multiple"),
      choices = c("Deutschland",
                  "Baden-Württemberg",
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

#' schule_kurse_verlauf_multiple Server Functions
#'
#' @noRd
mod_schule_kurse_verlauf_multiple_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$states_kurse_verlauf_multiple, {
      r$states_kurse_verlauf_multiple <- input$states_kurse_verlauf_multiple
    })

    observeEvent(input$subject_selected_multiple, {
      r$subject_selected_multiple <- input$subject_selected_multiple
    })

    observeEvent(input$topic_selected_multiple, {
      r$topic_selected_multiple <- input$topic_selected_multiple
    })

    observeEvent(input$date_kurse_verlauf_multiple, {
      r$date_kurse_verlauf_multiple <- input$date_kurse_verlauf_multiple
    })


  })
}

## To be copied in the UI
# mod_schule_kurse_verlauf_multiple_ui("schule_kurse_verlauf_multiple_1")

## To be copied in the server
# mod_schule_kurse_verlauf_multiple_server("schule_kurse_verlauf_multiple_1")
