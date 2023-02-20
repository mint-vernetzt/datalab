#' schule_kurse_comparison_bl UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_schule_kurse_comparison_bl_ui <- function(id){
  ns <- NS(id)
  tagList(

    p("Auswahl des Jahres:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_comparison_bl"),
      label = NULL,
      choices = c("2013","2014", "2015", "2016", "2017",
                  "2018","2019", "2020", "2021"),
      selected = "2021"),

    p("Form der Kursbelegung:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("indikator_comparison_bl"),
      choices = c("Grundkurse", "Leistungskurse"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
    ),

    p("Auswahl des Fachs:"),
    shinyWidgets::pickerInput(
      inputId = ns("subject_comparison_bl"),
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
      selected = "MINT-Fächer (gesamt)"
    )

  )
}

#' schule_kurse_comparison_bl Server Functions
#'
#' @noRd
mod_schule_kurse_comparison_bl_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_comparison_bl, {
      r$date_comparison_bl <- input$date_comparison_bl
    })

    observeEvent(input$subject_comparison_bl, {
      r$subject_comparison_bl <- input$subject_comparison_bl
    })

    observeEvent(input$indikator_comparison_bl, {
      r$indikator_comparison_bl <- input$indikator_comparison_bl
    })


  })
}

## To be copied in the UI
# mod_schule_kurse_comparison_bl_ui("schule_kurse_comparison_bl_1")

## To be copied in the server
# mod_schule_kurse_comparison_bl_server("schule_kurse_comparison_bl_1")
