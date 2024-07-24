#' schule_kurse_multiple_mint UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_schule_kurse_multiple_mint_ui <- function(id){
  ns <- NS(id)
  tagList(

    p("Betrachtungsart:"),
    shiny::radioButtons(
      inputId = ns("ansicht_kurse_mint"),
      label = NULL,
      choices = c("Einzelansicht - Kuchendiagramm", "Gruppenvergleich - Balkendiagramm"),
      selected = "Einzelansicht - Kuchendiagramm"
    ),
    p("Jahr:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_kurse_mint"),
      label = NULL,
      choices = 2013:2022,
      selected = 2022
    ),
    p("Kursniveau:"),
    shinyWidgets::pickerInput(
      inputId = ns("indikator_kurse_mint"),
      choices = c("Grundkurse", "Leistungskurse", "Oberstufenbelegungen"),
      selected = "Leistungskurse"
    ),
    p("Region:"),
    shinyWidgets::pickerInput(
      inputId = ns("region_kurse_mint"),
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
      multiple = F,
      selected = "Deutschland"
    ),
    conditionalPanel(condition = "input.ansicht_kurse_mint ==
                     'Einzelansicht - Kuchendiagramm'",
        ns = ns,
        p("Fächer-Ebene:"),
        shinyWidgets::pickerInput(
          inputId = ns("ebene_kurse_mint"),
          choices = c("MINT-Fachbereiche", "MINT-Fächer"),
          selected = "MINT-Fachbereiche"
        )
      ),
    br(),
    shinyBS::bsPopover(id="ih_schule_mint_1", title="",
                       content = paste0("In der ersten Einstellung ist zu sehen, dass im Jahr 2021 in Deutschland 24 % aller gewählten Grundkurse aus dem Bereich MINT sind. Bei Leistungskursen liegt der Anteil im Jahr 2021 bei 33 %. Außerdem sieht man, welche MINT-Fächer dabei einen wie großen Teil ausmachen."),
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_schule_mint_1")

  )
}

#' schule_kurse_multiple_mint Server Functions
#'
#' @noRd
mod_schule_kurse_multiple_mint_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$ansicht_kurse_mint, {
      r$ansicht_kurse_mint <- input$ansicht_kurse_mint
    })

    observeEvent(input$date_kurse_mint, {
      r$date_kurse_mint <- input$date_kurse_mint
    })

    observeEvent(input$indikator_kurse_mint, {
      r$indikator_kurse_mint <- input$indikator_kurse_mint
    })

    observeEvent(input$region_kurse_mint, {
      r$region_kurse_mint <- input$region_kurse_mint
    })

    observeEvent(input$ebene_kurse_mint, {
      r$ebene_kurse_mint <- input$ebene_kurse_mint
    })


  })
}

## To be copied in the UI
# mod_schule_kurse_multiple_mint_ui("schule_kurse_multiple_mint_1")

## To be copied in the server
# mod_schule_kurse_multiple_mint_server("schule_kurse_multiple_mint_1")
