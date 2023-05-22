#' schule_kurse_comparison_subjects UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_schule_kurse_comparison_subjects_ui <- function(id){
  ns <- NS(id)
  tagList(

    p("Jahr:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_comparison_subject"),
      label = NULL,
      choices = c("2013", "2014", "2015", "2016", "2017",
                  "2018","2019", "2020", "2021"),
      selected = "2021"
    ),
    p("Kursart:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("indikator_comparison_subject"),
      choices = c("Grundkurse", "Leistungskurse"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
    ),
    p("Region:"),
      # shinyBS::bsPopover(id="b2_t5_sb1", title = "",
      #                    content = "Alle Bundesländer haben andere LK vorschriften. In BY ...",
      #                    trigger = "hover"), #das ist in Box
      # tags$a(icon("question-circle"), id="b2_t5_sb1"), # das ist was man in App sieht
    shinyWidgets::pickerInput(
      inputId = ns("state_comparison_subject"),
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
      selected = "Hessen"

    )
    # ,
    # tags$a(p("Hinweis zu Bayern")),
    # shinyBS::bsPopover(id="b2_t5_sb2", title = "",
    #                    content = "Hinweis: xxx",
    #                    trigger = "hover"), #das ist in Box
    # tags$a(icon("question-circle"), id="b2_t5_sb2"), # das ist was man in App sieht

  )
}

#' schule_kurse_comparison_subjects Server Functions
#'
#' @noRd
mod_schule_kurse_comparison_subjects_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_comparison_subject, {
      r$date_comparison_subject <- input$date_comparison_subject
    })

    observeEvent(input$indikator_comparison_subject, {
      r$indikator_comparison_subject <- input$indikator_comparison_subject
    })

    observeEvent(input$state_comparison_subject, {
      r$state_comparison_subject <- input$state_comparison_subject
    })

  })
}

## To be copied in the UI
# mod_schule_kurse_comparison_subjects_ui("schule_kurse_comparison_subjects_1")

## To be copied in the server
# mod_schule_kurse_comparison_subjects_server("schule_kurse_comparison_subjects_1")
