#' schule_kurse_einstieg_comparison UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_schule_kurse_einstieg_comparison_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Betrachtungsart:"),
    shiny::radioButtons(
      inputId = ns("ansicht_kurse_einstieg_comparison"),
      label = NULL,
      choices = c("Einzelansicht - Kuchendiagramm", "Gruppenvergleich - Balkendiagramm"),
      selected = "Einzelansicht - Kuchendiagramm"
    ),

    p("Jahr:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_kurse_einstieg_comparison"),
      label = NULL,
      choices = 2013:2022,
      selected = 2022
    ),
    p("Region:"),
    shinyWidgets::pickerInput(
      inputId = ns("region_kurse_einstieg_comparsion"),
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
                  "Thüringen",
                  "Westdeutschland (o. Berlin)",
                  "Ostdeutschland (inkl. Berlin)"
      ),
      multiple = FALSE,
      selected = c("Deutschland")
    ),

    conditionalPanel(condition = "input.ansicht_kurse_einstieg_comparison ==
                     'Einzelansicht - Kuchendiagramm'",
                     ns = ns,
                     p("Kursniveau:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("indikator_kurse_einstieg_comparison"),
                       choices = c("Leistungskurse",
                                   "Grundkurse",
                                   "Oberstufenbelegungen"),
                       selected = "Leistungskurse",
                       multiple = FALSE
                     ),

                     br(),
                     shinyBS::bsPopover(id="ih_schule_mint_2", title="",
                                        content = paste0("In der ersten Einstellung ist zu sehen, dass im Jahr 2022 in Deutschland 33 % aller gewählten Leistungskurse aus dem Bereich MINT sind."),
                                        trigger = "hover"),
                     tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_schule_mint_2")

    ),

    conditionalPanel(condition = "input.ansicht_kurse_einstieg_comparison ==
                     'Gruppenvergleich - Balkendiagramm'",
                     ns = ns,

                     br(),
                     shinyBS::bsPopover(id="ih_schule_mint_2b", title="",
                                        content = paste0("In der ersten Einstellung ist zu sehen, dass im Jahr 2022 in Deutschland 24 % aller gewählten Grundkurse aus dem Bereich MINT sind. Bei Leistungskursen liegt der Anteil im Jahr 2022 bei 33 %."),
                                        trigger = "hover"),
                     tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_schule_mint_2b")
    )



  )
}

#' schule_kurse_einstieg_comparison Server Functions
#'
#' @noRd
mod_schule_kurse_einstieg_comparison_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$ansicht_kurse_einstieg_comparison, {
      r$ansicht_kurse_einstieg_comparison <- input$ansicht_kurse_einstieg_comparison
    })

    observeEvent(input$date_kurse_einstieg_comparison, {
      r$date_kurse_einstieg_comparison <- input$date_kurse_einstieg_comparison
    })

    observeEvent(input$region_kurse_einstieg_comparsion, {
      r$region_kurse_einstieg_comparsion <- input$region_kurse_einstieg_comparsion
    })

    observeEvent(input$indikator_kurse_einstieg_comparison, {
      r$indikator_kurse_einstieg_comparison <- input$indikator_kurse_einstieg_comparison
    })

  })
}

## To be copied in the UI
# mod_schule_kurse_einstieg_comparison_ui("schule_kurse_einstieg_comparison_1")

## To be copied in the server
# mod_schule_kurse_einstieg_comparison_server("schule_kurse_einstieg_comparison_1")
