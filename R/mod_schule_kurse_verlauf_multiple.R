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

    p("Jahre:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_kurse_verlauf_multiple"),
      label = NULL,
      choices = c("2013", "2014", "2015", "2016", "2017",
                  "2018","2019", "2020", "2021", "2022"),
      selected = c("2016", "2021")
    ),
    p("Kursart:"),
    shinyWidgets::pickerInput(
      inputId = ns("topic_selected_multiple"),
      choices = c("Grundkurse", "Leistungskurse"),
      selected = "Leistungskurse"
    ),
    p("Fach/Fächergruppe:"),
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
    p("Regionen:"),
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
                  "Thüringen"
                  ,
                  "Westdeutschland (o. Berlin)",
                  "Ostdeutschland (inkl. Berlin)"
                  ),
      multiple = TRUE,
      options = list(`actions-box` = TRUE,
                     `deselect-all-text` = "Alle abwählen",
                     `select-all-text` = "Alle auswählen"),
      selected = c("Hessen", "Hamburg")
    ),
    p("Betrachtung:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("abs_zahlen_kurse_verlauf_multiple"),
      choices = c("In Prozent", "Anzahl"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
    ),
    br(),
    shinyBS::bsPopover(id="ih_schule_fach_2", title="",
                       content = paste0("Diese erste Einstellung zeigt, dass der Anteil an Leitungskursbelegungen in MINT-Fächern in Hessen höher als in Hamburg ist. Das muss nicht am Interesse der Schüler*innen an MINT liegen, sondern könnte auch auf Unterschiede in den Vorschriften zur Oberstufe-Kursbelegung zurückgehen."),
                       placement = "top",
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_schule_fach_2")
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

    observeEvent(input$abs_zahlen_kurse_verlauf_multiple, {
      r$abs_zahlen_kurse_verlauf_multiple <- input$abs_zahlen_kurse_verlauf_multiple
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
