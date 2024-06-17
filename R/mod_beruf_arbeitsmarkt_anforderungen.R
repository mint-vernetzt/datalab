#' beruf_arbeitsmarkt_anforderungen UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_beruf_arbeitsmarkt_anforderungen_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Auswahl des Jahres:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_arbeitsmarkt_anforderungen"),
      label = NULL,
      choices = c(2021, 2022),
      selected = 2022
    ),

    # shinyWidgets::pickerInput(
    #                      inputId = ns("indikator_arbeitsmarkt_anforderungen"),
    #                      choices = c("Beschäftigte",
    #                                  "Auszubildende",
    #                              #    "Auszubildende (1. Jahr)",
    #                                  "ausländische Beschäftigte",
    #                                  "ausländische Auszubildende"),
    #                      selected = c("Beschäftigte", "Auszubildende"),
    #                      multiple = TRUE,
    #                      options =  list(
    #                        "max-options" = 2,
    #                        "max-options-text" = "Bitte nur maximal 2 Bereiche auswählen"
    #                      )),

    p("Beschäftigungsform:"),
    conditionalPanel(condition = "input.date_arbeitsmarkt_anforderungen == '2022'",
                     ns = ns,
    shinyWidgets::pickerInput(
      inputId = ns("indikator_arbeitsmarkt_anforderungen_22"),
      choices = c("Beschäftigte",
                  "Auszubildende",
                  "Auszubildende mit neuem Lehrvertrag" = "Auszubildende (1. Jahr)",
                  "ausländische Beschäftigte",
                  "ausländische Auszubildende"),
      selected = c("Beschäftigte", "Auszubildende"),
      multiple = TRUE,
      options =  list(
        "max-options" = 2,
        "max-options-text" = "Bitte nur maximal 2 Bereiche auswählen"
      ))),
    conditionalPanel(condition = "input.date_arbeitsmarkt_anforderungen == '2021'",
                     ns = ns,
                     shinyWidgets::pickerInput(
                       inputId = ns("indikator_arbeitsmarkt_anforderungen_21"),
                       choices = c("Beschäftigte",
                                   "Auszubildende",
                                   "Auszubildende mit neuem Lehrvertrag" = "Auszubildende (1. Jahr)",
                                   "ausländische Beschäftigte",
                                   "ausländische Auszubildende"),
                       selected = c("Beschäftigte", "Auszubildende"),
                       multiple = TRUE,
                       options =  list(
                         "max-options" = 2,
                         "max-options-text" = "Bitte nur maximal 2 Bereiche auswählen"
                       ))),

    br(),
    shinyBS::bsPopover(id="ih_beruf_mint_1", title="",
                       content = paste0("In der ersten Einstellung ist zu sehen, dass in Deutschland im Jahr 2022 23 % der Beschäftigten in MINT beschäftigt waren (2 + 3 + 18 = 23 %). Bei den Auszubildenden waren dies 30 %."),
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_beruf_mint_1")

    )
}

#' beruf_arbeitsmarkt_anforderungen Server Functions
#'
#' @noRd
mod_beruf_arbeitsmarkt_anforderungen_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_arbeitsmarkt_anforderungen, {
      r$date_arbeitsmarkt_anforderungen <- input$date_arbeitsmarkt_anforderungen
    })
    observeEvent(input$indikator_arbeitsmarkt_anforderungen_21, {
      r$indikator_arbeitsmarkt_anforderungen_21 <- input$indikator_arbeitsmarkt_anforderungen_21
    })
    observeEvent(input$indikator_arbeitsmarkt_anforderungen_22, {
      r$indikator_arbeitsmarkt_anforderungen_22 <- input$indikator_arbeitsmarkt_anforderungen_22
    })
    # observeEvent(input$indikator_arbeitsmarkt_anforderungen, {
    #     r$indikator_arbeitsmarkt_anforderungen <- input$indikator_arbeitsmarkt_anforderungen
    #   })

  })
}

## To be copied in the UI
# mod_beruf_arbeitsmarkt_anforderungen_ui("beruf_arbeitsmarkt_anforderungen_1")

## To be copied in the server
# mod_beruf_arbeitsmarkt_anforderungen_server("beruf_arbeitsmarkt_anforderungen_1")
