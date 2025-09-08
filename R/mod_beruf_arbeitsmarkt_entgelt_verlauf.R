#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_beruf_arbeitsmarkt_entgelt_verlauf_ui <- function(id){
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
      inputId = ns("date_arbeitsmarkt_entgelt_verlauf"),
      label = NULL,
      choices = 2013:2023,
      selected = c(2017, 2023)
    ),
    p("Region:"),
    shinyWidgets::pickerInput(
      inputId = ns("region_arbeitsmarkt_entgelt_verlauf"),
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
      inputId = ns("indikator_arbeitsmarkt_entgelt_verlauf_2"),
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
      inputId = ns("abs_zahlen_arbeitsmarkt_entgelt_verlauf"),
      choices = c("In Prozent", "Anzahl"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
    ),
    br(),
    shinyBS::bsPopover(id="ih_beruf_mint_2_entge", title="",
                       content = paste0(""),
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_beruf_mint_2_entge")
  )

}

#' beruf_arbeitsmarkt_entgelt_verlauf Server Functions
#'
#' @noRd
mod_beruf_arbeitsmarkt_entgelt_verlauf_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$date_arbeitsmarkt_entgelt_verlauf, {
      r$date_arbeitsmarkt_entgelt_verlauf <- input$date_arbeitsmarkt_entgelt_verlauf
    })

    observeEvent(input$region_arbeitsmarkt_entgelt_verlauf, {
      r$region_arbeitsmarkt_entgelt_verlauf <- input$region_arbeitsmarkt_entgelt_verlauf
    })

    observeEvent(input$indikator_arbeitsmarkt_entgelt_verlauf_2, {
      r$indikator_arbeitsmarkt_entgelt_verlauf_2 <- input$indikator_arbeitsmarkt_entgelt_verlauf_2
    })

    observeEvent(input$abs_zahlen_arbeitsmarkt_entgelt_verlauf, {
      r$abs_zahlen_arbeitsmarkt_entgelt_verlauf <- input$abs_zahlen_arbeitsmarkt_entgelt_verlauf
    })


  })
}

## To be copied in the UI
# mod_beruf_arbeitsmarkt_entgelt_verlauf_ui("beruf_arbeitsmarkt_entgelt_verlauf_1")

## To be copied in the server
# mod_beruf_arbeitsmarkt_entgelt_verlauf_server("beruf_arbeitsmarkt_entgelt_verlauf_1")
