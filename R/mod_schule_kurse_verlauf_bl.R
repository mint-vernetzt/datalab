#' schule_kurse_verlauf_bl UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_schule_kurse_verlauf_bl_ui <- function(id){
  ns <- NS(id)
  load(file = system.file(package="datalab","data/kurse.rda"))

  kurse <- kurse %>% dplyr::filter(fachbereich != "Alle Fächer")

  tagList(

    p("Wähle einen Zeitraum:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_kurse_verlauf_bl"),
      label = NULL,
      choices = c("2010","2011", "2012", "2013", "2014", "2015", "2016", "2017",
                  "2018","2019", "2020"),
      selected = c("2015", "2020")
    ),
    p("Einzelne Fächer oder als MINT zusammengefasst:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("subjects_aggregated_bl"),
      choices = c("einzeln", "aggregiert"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon")),
      selected = "aggregiert"
    ),
    conditionalPanel(condition = "input.subjects_aggregated_bl == 'aggregiert'",
                     ns = ns,
                     p("Wähle ob MINT oder alle anderen Fächer dargestellt werden sollen:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("topic_kurse_verlauf_bl"),
                       choices = c("MINT", "andere Fächer"),
                       selected = "MINT"
                     )),
    conditionalPanel(condition = "input.subjects_aggregated_bl == 'einzeln'",
                     ns = ns,
                     p("Wähle ein Fach:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("subject_selected_bl"),
                       choices = c(unique(kurse$fachbereich)),
                       selected = "Informatik"
                     )),

    p("Wähle ein Bundesland:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("states_kurse_verlauf_bl"),
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
                       selected = "Hessen"
                     )
  )

}

#' schule_kurse_verlauf_bl Server Functions
#'
#' @noRd
mod_schule_kurse_verlauf_bl_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_kurse_verlauf_bl, {
      r$date_kurse_verlauf_bl <- input$date_kurse_verlauf_bl
    })

    observeEvent(input$indikator_kurse_verlauf_bl, {
      r$indikator_kurse_verlauf_bl <- input$indikator_kurse_verlauf_bl
    })

    observeEvent(input$topic_kurse_verlauf_bl, {
      r$topic_kurse_verlauf_bl <- input$topic_kurse_verlauf_bl
    })

    observeEvent(input$states_kurse_verlauf_bl, {
      r$states_kurse_verlauf_bl <- input$states_kurse_verlauf_bl
    })

    observeEvent(input$subjects_aggregated_bl, {
      r$subjects_aggregated_bl <- input$subjects_aggregated_bl
    })

    observeEvent(input$subject_selected_bl, {
      r$subject_selected_bl <- input$subject_selected_bl
    })


  })
}

## To be copied in the UI
# mod_schule_kurse_verlauf_bl_ui("schule_kurse_verlauf_bl_1")

## To be copied in the server
# mod_schule_kurse_verlauf_bl_server("schule_kurse_verlauf_bl_1")
