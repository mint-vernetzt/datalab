#' home_start_einstieg UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList


mod_home_start_einstieg_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Bereiche:"),
    shinyWidgets::pickerInput(
      inputId = ns("indikator_start_einstieg_1"),
      choices = c("Schüler:innen Leistungskurse",
                   "Studierende",
                  "Auszubildende", "Beschäftigte"),
      selected = c( "Beschäftigte", "Studierende"),
      multiple = TRUE,
      options =  list(
        "max-options" = 2,
        "max-options-text" = "Bitte genau 2 Bereiche auswählen"
      )
    ),
    br(),

    shinyBS::bsPopover(id="dh", title = "",
                       content = paste0("Falls die Grafiken abgeschnitten dargestellt werden, bitte das gesamte Ansichtsfenster einmal verkleinern und dann wieder maximieren. Dann stellt sich das Seitenverhältnis des Desktops richtig ein."),
                       trigger = "hover"),
    tags$a(paste0("Probleme bei der Darstellung"), icon("question-circle"), id = "dh"),
    br(),
    br(),
    shinyBS::bsPopover(id="ih_alle_mint_1", title="",
                       content = paste0("Wenn man beispielsweise Auszubildende und Beschäftigte betrachtet, sieht man, dass sich von allen Auszubildenden deutschlandweit im Jahr 2021 31 % in einer Ausbildung in einem MINT-Beruf befinden. Bei den Beschäftigten in Deutschland ist dieser Anteil ein wenig geringer. Im Jahr 2021 arbeiten 23 % der Beschäftigten in einem MINT-Beruf."),
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_alle_mint_1")

  )
}

#' home_start_einstieg Server Functions
#'
#' @noRd
mod_home_start_einstieg_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$indikator_start_einstieg_1, {
      r$indikator_start_einstieg_1 <- input$indikator_start_einstieg_1
    })

  })
}

## To be copied in the UI
# mod_home_start_einstieg_ui("home_start_einstieg_1")

## To be copied in the server
# mod_home_start_einstieg_server("home_start_einstieg_1")
