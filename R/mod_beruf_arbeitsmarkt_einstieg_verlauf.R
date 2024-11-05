#' beruf_arbeitsmarkt_einstieg_verlauf UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_beruf_arbeitsmarkt_einstieg_verlauf_ui <- function(id){
  ns <- NS(id)
  tagList(

    #farbe auswahl
    tags$head(
      tags$style(HTML("
        .dropdown-menu .bs-actionsbox .btn-group .btn {
          background-color: #e7f1ff !important;  /* Hellblau für die Alle auswählen/abwählen Buttons */
          color: #000000 !important;
        }
        .dropdown-menu .bs-actionsbox .btn-group .btn:hover {
          background-color: #d0e8ff !important;  /* Etwas dunkleres Blau beim Hover */
          color: #000000 !important;
        }
      "))
    ),


    p("Jahre:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_arbeitsmarkt_einstieg_verlauf"),
      label = NULL,
      choices = 2013:2023,
      selected = c(2017, 2023)
    ),
    p("Region:"),
    shinyWidgets::pickerInput(
      inputId = ns("region_arbeitsmarkt_einstieg_verlauf"),
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
    p("Beschäftigtengruppe:"),
    shinyWidgets::pickerInput(
      inputId = ns("indikator_arbeitsmarkt_einstieg_verlauf"),
      choices = c("Auszubildende",
                  "Auszubildende mit neuem Lehrvertrag" = "Auszubildende (1. Jahr)",
                  "Beschäftigte",
                  "ausländische Auszubildende",
                  "ausländische Beschäftigte",
                  "Beschäftigte 25-55",
                  "Beschäftigte u25",
                  "Beschäftigte ü55"),
      multiple = TRUE,
      options = list(`actions-box` = TRUE,
                     `deselect-all-text` = "Alle abwählen",
                     `select-all-text` = "Alle auswählen"),
      selected = c("Beschäftigte", "Auszubildende"),
    ),
    p("Darstellungsart:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("abs_zahlen_arbeitsmarkt_einstieg_verlauf"),
      choices = c("In Prozent", "Anzahl"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
    ),
    br(),
    shinyBS::bsPopover(id="ih_beruf_mint_2", title="",
                       content = paste0("In der ersten Einstellung ist zu sehen, dass der Anteil an Auszubildenden in MINT von 2020 auf 2021 leicht sinkt. Betrachtet man die absolute Anzahl, sieht man, dass es deutschlandweit 2022 ca. 50.000 Auszubildende weniger in MINT gibt als noch 2020."),
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_beruf_mint_2")
  )

}

#' beruf_arbeitsmarkt_einstieg_verlauf Server Functions
#'
#' @noRd
mod_beruf_arbeitsmarkt_einstieg_verlauf_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$date_arbeitsmarkt_einstieg_verlauf, {
      r$date_arbeitsmarkt_einstieg_verlauf <- input$date_arbeitsmarkt_einstieg_verlauf
    })

    observeEvent(input$region_arbeitsmarkt_einstieg_verlauf, {
      r$region_arbeitsmarkt_einstieg_verlauf <- input$region_arbeitsmarkt_einstieg_verlauf
    })

    observeEvent(input$indikator_arbeitsmarkt_einstieg_verlauf, {
      r$indikator_arbeitsmarkt_einstieg_verlauf <- input$indikator_arbeitsmarkt_einstieg_verlauf
    })

    observeEvent(input$abs_zahlen_arbeitsmarkt_einstieg_verlauf, {
      r$abs_zahlen_arbeitsmarkt_einstieg_verlauf <- input$abs_zahlen_arbeitsmarkt_einstieg_verlauf
    })


  })
}

## To be copied in the UI
# mod_beruf_arbeitsmarkt_einstieg_verlauf_ui("beruf_arbeitsmarkt_einstieg_verlauf_1")

## To be copied in the server
# mod_beruf_arbeitsmarkt_einstieg_verlauf_server("beruf_arbeitsmarkt_einstieg_verlauf_1")
