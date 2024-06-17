#' home_start_einstieg_gender UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_home_start_einstieg_gender_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Bereiche (max. 3):"),
    shinyWidgets::pickerInput(
      inputId = ns("indikator_start_einstieg_1_gender"),
      choices = c("Schüler:innen Leistungskurse","Studierende",
                  "Auszubildende", "Beschäftigte"),
      selected = c("Schüler:innen Leistungskurse", "Beschäftigte"),
      multiple = TRUE,
      options =  list(
        "max-options" = 3,
        "max-options-text" = "Bitte nur maximal 3 Bereiche auswählen"
      )
    ),
    br(),
    shinyBS::bsPopover(id="dh_alle_frauen_1", title = "",
                       content = paste0("Falls die Grafiken abgeschnitten dargestellt werden, bitte das gesamte Ansichtsfenster einmal verkleinern und dann wieder maximieren. Dann stellt sich das Seitenverhältnis des Desktops richtig ein."),
                       trigger = "hover"),
    tags$a(paste0("Probleme bei der Darstellung"), icon("question-circle"), id = "dh_alle_frauen_1"),
    br(),
    br(),
    shinyBS::bsPopover(id="ih_alle_frauen_1", title="",
                       content = paste0("Betrachtet man hier beispielsweise die MINT-Beschäftigten, sieht man, dass deutschlandweit im Jahr 2021 17 % der MINT-Beschäftigten Frauen sind. Betrachtet man dagegen alle Berufsgruppen außer MINT zusammen, machen Frauen sogar die Mehrheit der Beschäftigten aus (55 %)."),
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_alle_frauen_1")

  )
}

#' home_start_einstieg_gender Server Functions
#'
#' @noRd
mod_home_start_einstieg_gender_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$indikator_start_einstieg_1_gender, {
      r$indikator_start_einstieg_1_gender <- input$indikator_start_einstieg_1_gender
    })

  })
}

## To be copied in the UI
# mod_home_start_einstieg_gender_ui("home_start_einstieg_gender_1")

## To be copied in the server
# mod_home_start_einstieg_gender_server("home_start_einstieg_gender_1")
