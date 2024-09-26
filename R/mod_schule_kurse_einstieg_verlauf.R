#' schule_kurse_einstieg_verlauf UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_schule_kurse_einstieg_verlauf_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Jahre:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_kurse_einstieg_verlauf"),
      label = NULL,
      choices = c("2013", "2014", "2015", "2016", "2017",
                  "2018","2019", "2020", "2021", "2022"),
      selected = c("2016", "2022")
    ),
    p("Region:"),
    shinyWidgets::pickerInput(
      inputId = ns("region_kurse_einstieg_verlauf"),
      choices = c("Deutschland",
                  "Westdeutschland (o. Berlin)",
                  "Ostdeutschland (inkl. Berlin)",
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
      ),
      multiple = FALSE,
      selected = c("Deutschland")
    ),
    p("Darstellungsart:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("abs_zahlen_kurse_einstieg_verlauf"),
      choices = c("In Prozent", "Anzahl"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
    ),
    br(),
    shinyBS::bsPopover(id="ih_schule_mint_3", title="",
                       content = paste0("Die erste Einstellung der Grafik zeigt z. B., dass der Anteil an MINT-Belegungen in der Oberstufenbelegungen über die Jahre ziemlich konstant ist (26%). Seit 2018 sind die MINT-Leistungskursbelegungen etwas gesunken (2 Prozentpunkte). Dagegen ist der MINT-Anteil der Grundkursbelegungen von 2020 auf 2021 um einen Prozentpunkt gestiegen."),
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_schule_mint_3"),
    br(),
    br(),
    shinyBS::bsPopover(id="dh_schule_fach_neu", title = "",
                       content = paste0("Falls die Grafiken abgeschnitten dargestellt werden, bitte das gesamte Ansichtsfenster einmal verkleinern und dann wieder maximieren. Dann stellt sich das Seitenverhältnis des Desktops richtig ein."),
                       trigger = "hover"),
    tags$a(paste0("Probleme bei der Darstellung"), icon("question-circle"), id = "dh_schule_fach_neu"),
    br(),

  )


}

#' schule_kurse_einstieg_verlauf Server Functions
#'
#' @noRd
mod_schule_kurse_einstieg_verlauf_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_kurse_einstieg_verlauf, {
      r$date_kurse_einstieg_verlauf <- input$date_kurse_einstieg_verlauf
    })

    observeEvent(input$abs_zahlen_kurse_einstieg_verlauf, {
      r$abs_zahlen_kurse_einstieg_verlauf <- input$abs_zahlen_kurse_einstieg_verlauf
    })

    observeEvent(input$region_kurse_einstieg_verlauf, {
      r$region_kurse_einstieg_verlauf <- input$region_kurse_einstieg_verlauf
    })



  })
}

## To be copied in the UI
# mod_schule_kurse_einstieg_verlauf_ui("schule_kurse_einstieg_verlauf_1")

## To be copied in the server
# mod_schule_kurse_einstieg_verlauf_server("schule_kurse_einstieg_verlauf_1")
