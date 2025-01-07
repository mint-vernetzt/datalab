#' mod_schule_kurse_iqb_fragebogen UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_schule_kurse_iqb_fragen_ui <- function(id){
  ns <- NS(id)
  tagList(

    p("Jahre:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("jahr_iqb_fragebogen"),
      label = NULL,
      choices = c("2016", "2021"),
      selected = c("2021")
    ),

    p("Schulfach:"),
    shinyWidgets::pickerInput(
     inputId = ns("fach_iqb_fragebogen"),
     choices = c("Mathematik",
                 "Deutsch"),
     multiple = FALSE,
     selected = "Mathematik"
    ),

    br(),
    darstellung(id = "leistungsschwache_schueler2"),
    br(),
    br(),
    shinyBS::bsPopover(id="ih_schule_kompetenz_3", title="",
                       content = paste0("Die erste Darstellung zeigt, dass sowohl das Interesse als auch die Einschätzung der eignenen Fähigkeiten in Mathematik bei Mädchen geringer als bei Jungen ist. Betrachtet man zum Vergleich Interesse und Einschätzung der Fähigkeiten für das Fach Deutsch, sieht man eine gegenteilige Tendenz. Hier geben Mädchen ein höheres Interesse an und schätzen sich als kompetenter ein."),
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_schule_kompetenz_3")
  )
}

#' schule_kurse_verlauf Server Functions
#'
#' @noRd
mod_schule_kurse_iqb_fragen_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$jahr_iqb_fragebogen, {
      r$jahr_iqb_fragebogen <- input$jahr_iqb_fragebogen
    })

    observeEvent(input$fach_iqb_fragebogen, {
      r$fach_iqb_fragebogen <- input$fach_iqb_fragebogen
    })

  })
}

## To be copied in the UI
# mod_schule_kurse_iqb_fragen_ui("mod_schule_kurse_iqb_fragen_ui_1")

## To be copied in the server
# mod_schule_kurse_iqb_fragen_server("mod_schule_kurse_iqb_fragen_ui_1")
