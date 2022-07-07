#' schule_kurse_verlauf_bl_subjects UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_schule_kurse_verlauf_bl_subjects_ui <- function(id){
  ns <- NS(id)
  tagList(

    p("Auswahl Zeitraum:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_kurse_verlauf_subject_bl"),
      label = NULL,
      choices = c("2013", "2014", "2015", "2016", "2017",
                  "2018","2019", "2020"),
      selected = c("2015", "2020")
    ),
    p("Auswahl Grundkurs oder Leistungskurs:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("topic_selected_subject_bl"),
      choices = c("Grundkurse", "Leistungskurse"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
    ),
    p("Auswahl Fach (Mehrfachauswahl möglich):"),
    shinyWidgets::pickerInput(
      inputId = ns("subject_selected_bl_sub"),
      choices = c("MINT (aggregiert)","Mathematik", "Informatik", "Physik", "Chemie",
                  "Biologie", "Deutsch", "Fremdsprachen", "Gesellschaftswissenschaften",
                  "Musik/Kunst", "Religion/Ethik", "Sport"),
      selected = c("MINT (aggregiert)", "Mathematik"),
      options = list(`actions-box` = TRUE,
                     `deselect-all-text` = "Alle abwählen",
                     `select-all-text` = "Alle auswählen"),
      multiple = TRUE
    ),
    p("Auswahl Bundesland oder Region:"),
    shinyWidgets::pickerInput(
      inputId = ns("states_kurse_verlauf_subject_bl"),
      choices = list(
        Regionen = c("Deutschland", "Westen", "Osten"),
        Bundesländer = c( "Bayern",
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
                          "Thüringen")),
      selected = "Hessen"
    )
  )
}

#' schule_kurse_verlauf_bl_subjects Server Functions
#'
#' @noRd
mod_schule_kurse_verlauf_bl_subjects_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$date_kurse_verlauf_subject_bl, {
      r$date_kurse_verlauf_subject_bl <- input$date_kurse_verlauf_subject_bl
    })

    observeEvent(input$subject_selected_bl_sub, {
      r$subject_selected_bl_sub <- input$subject_selected_bl_sub
    })

    observeEvent(input$states_kurse_verlauf_subject_bl, {
      r$states_kurse_verlauf_subject_bl <- input$states_kurse_verlauf_subject_bl
    })

    observeEvent(input$topic_selected_subject_bl, {
      r$topic_selected_subject_bl <- input$topic_selected_subject_bl
    })

  })
}

## To be copied in the UI
# mod_schule_kurse_verlauf_bl_subjects_ui("schule_kurse_verlauf_bl_subjects_1")

## To be copied in the server
# mod_schule_kurse_verlauf_bl_subjects_server("schule_kurse_verlauf_bl_subjects_1")
