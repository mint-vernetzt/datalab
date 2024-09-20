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
    p("Darstellungsart:"),
    shiny::radioButtons(
      inputId = ns("ansicht_start_einstieg"),
      label = NULL,
      choices = c("Einzelansicht - Kuchendiagramm",
                  "Gruppenvergleich - Balkendiagramm"),
      selected = "Einzelansicht - Kuchendiagramm"
    ),

    p("Jahr:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_start_comparison_mint"),
      label = NULL,
      choices = 2013:2022,
      selected = 2022
    ),

    # p("Region:"),
    # shinyWidgets::pickerInput(
    #   inputId = ns("region_start_einstieg_comparsion"),
    #   choices = c("Deutschland",
    #               "Baden-Württemberg",
    #               "Bayern",
    #               "Berlin",
    #               "Brandenburg",
    #               "Bremen",
    #               "Hamburg",
    #               "Hessen",
    #               "Mecklenburg-Vorpommern",
    #               "Niedersachsen",
    #               "Nordrhein-Westfalen",
    #               "Rheinland-Pfalz",
    #               "Saarland",
    #               "Sachsen",
    #               "Sachsen-Anhalt",
    #               "Schleswig-Holstein",
    #               "Thüringen",
    #               "Westdeutschland (o. Berlin)",
    #               "Ostdeutschland (inkl. Berlin)"
    #   ),
    #   multiple = FALSE,
    #   selected = c("Deutschland")
    # ),

    conditionalPanel(condition = "input.ansicht_start_einstieg == 'Einzelansicht - Kuchendiagramm'",
                     ns = ns,

          p("Bereiche (max. 3):"),
          shinyWidgets::pickerInput(
            inputId = ns("indikator_start_einstieg_1"),
            choices = c("Schüler:innen Leistungskurse",
                         "Studierende",
                        "Auszubildende", "Beschäftigte"),
            selected = c( "Beschäftigte", "Studierende"),
            multiple = TRUE,
            options =  list(
              "max-options" = 3,
              "max-options-text" = "Bitte nur maximal 3 Bereiche auswählen"
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
    ),

    conditionalPanel(condition = "input.ansicht_start_einstieg == 'Gruppenvergleich - Balkendiagramm'",
                     ns = ns,

         br(),
         shinyBS::bsPopover(id="ih_alle_mint_3", title="",
                            content = paste0("Diese Übersicht zeigt beispielsweise, dass deutschlandweit 2021 37 % der Studierenden ein MINT-Fach studieren. Dagegen studieren 63 % in einem anderen Fachbereich. Über alle Bildungsbereiche hinweg ist der MINT-Anteil der Beschäftigten mit 23 % 2021 am geringsten."),
                            trigger = "hover"),
         tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_alle_mint_3")
    )

  )
}

#' home_start_einstieg Server Functions
#'
#' @noRd
mod_home_start_einstieg_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$ansicht_start_einstieg, {
      r$ansicht_start_einstieg <- input$ansicht_start_einstieg
    })

    observeEvent(input$date_start_comparison_mint, {
      r$date_start_comparison_mint <- input$date_start_comparison_mint
    })

    # observeEvent(input$region_start_einstieg_comparsion, {
    #   r$region_start_einstieg_comparsion <- input$region_start_einstieg_comparsion
    # })

    observeEvent(input$indikator_start_einstieg_1, {
      r$indikator_start_einstieg_1 <- input$indikator_start_einstieg_1
    })


  })
}

## To be copied in the UI
# mod_home_start_einstieg_ui("home_start_einstieg_1")

## To be copied in the server
# mod_home_start_einstieg_server("home_start_einstieg_1")
