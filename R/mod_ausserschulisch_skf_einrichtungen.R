#' mod_ausserschulisch_skf_einrichtungen UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_ausserschulisch_skf_einrichtungen_ui <- function(id){
  ns <- NS(id)

  tagList(

    p("Auswahl des Zeitraums:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_skf_einrichtungen"),
      label = NULL,
      choices = c("2012", "2014", "2015", "2016", "2017",
                  "2018","2019", "2020", "2021", "2022"),
      selected = c("2017", "2022")
    ),

    p("Einrichtungsart:"),
    shinyWidgets::pickerInput(
      inputId = ns("ort_skf_einrichtungen"),
      choices = c("Alle Einrichtungen",
                  "Kita",
                  "Hort",
                  "Grundschule"),
      multiple = FALSE,
      selected = c("Kita")

    ),

    br(),
    shinyBS::bsPopover(id="sfk_pop1", title = "",
                       content = paste0("Falls die Grafiken abgeschnitten dargestellt werden, bitte das gesamte Ansichtsfenster einmal verkleinern und dann wieder maximieren. Dann stellt sich das Seitenverhältnis des Desktops richtig ein."),
                       trigger = "hover"),
    tags$a(paste0("Probleme bei der Darstellung"), icon("question-circle"), id = "sfk_pop1"),
    br(),

    br(),
    shinyBS::bsPopover(id="ih_schule_ausserschulisch_1", title="",
                       content = paste0("Die erste Einstellung der Grafik zeigt, wie die Anzahl an Kitas, die bei der Stiftung Kinder forschen aktiv sind, kontinuierlich steigt. Im Jahr 2022 sind 28.120 Kitas bei SKf aktiv. Davon haben 22.638 Kitas Fachpersonal, das sich bei SKf in MINT-Bildung weitergebildet hat."),
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_schule_ausserschulisch_1")
  )

}

#' schule_kurse_verlauf Server Functions
#'
#' @noRd
mod_ausserschulisch_skf_einrichtungen_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_skf_einrichtungen, {
      r$date_skf_einrichtungen <- input$date_skf_einrichtungen
    })

    observeEvent(input$ort_skf_einrichtungen, {
      r$ort_skf_einrichtungen <- input$ort_skf_einrichtungen
    })

  })
}

## To be copied in the UI (in mod_schule_kurse)
# mod_ausserschulisch_skf_einrichtungen_ui("mod_ausserschulisch_skf_einrichtungen_ui_1")

## To be copied in the server (in mod_schule)
# mod_ausserschulisch_skf_einrichtungen_server("mod_ausserschulisch_skf_einrichtungen_ui_1")
