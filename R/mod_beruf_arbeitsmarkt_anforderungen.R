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
    # Auswahl Jahr entfernt, da in detailliertem Datensatz nur 2021 vorliegt

    # p("Auswahl des Jahres:"),
    # shinyWidgets::sliderTextInput(
    #   inputId = ns("date_arbeitsmarkt_anforderungen"),
    #   label = NULL,
    #   choices = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021),
    #   selected = 2021
    # )

    # Begonnen mit Anpassung, dass beliegige Waffles vergleichen werden können
    # noch nicht implementierbar - Plot Fkt muss noch angepasst werden

    p("Beschäftigungsform:"),
    shinyWidgets::pickerInput(
      inputId = ns("indikator_arbeitsmarkt_anforderungen"),
      choices = c("Beschäftigte",
                  "Auszubildende",
                  "Auszubildende (1. Jahr)",
                  "ausländische Beschäftigte",
                  "ausländische Auszubildende"),
      selected = c("Beschäftigte", "Auszubildende"),
      multiple = TRUE,
      options =  list(
        "max-options" = 2,
        "max-options-text" = "Bitte nur maximal 2 Bereiche auswählen"
      )),
    br(),
    shinyBS::bsPopover(id="ih_beruf_mint_1", title="",
                       content = paste0("In der ersten Einstellung ist zu sehen, dass in Deutschland im Jahr 2021 23 % der Beschäftigten in MINT beschäftigt waren (2 + 3 + 18 = 23 %). Bei den Auszubildenden waren dies 30 % bzw. 31 % (je nach Rundung)."),
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_beruf_mint_1")

    )
}

#' beruf_arbeitsmarkt_anforderungen Server Functions
#'
#' @noRd
mod_beruf_arbeitsmarkt_anforderungen_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    # observeEvent(input$date_arbeitsmarkt_anforderungen, {
    #   r$date_arbeitsmarkt_anforderungen <- input$date_arbeitsmarkt_anforderungen
    # })
    observeEvent(input$indikator_arbeitsmarkt_anforderungen, {
      r$indikator_arbeitsmarkt_anforderungen <- input$indikator_arbeitsmarkt_anforderungen
    })

  })
}

## To be copied in the UI
# mod_beruf_arbeitsmarkt_anforderungen_ui("beruf_arbeitsmarkt_anforderungen_1")

## To be copied in the server
# mod_beruf_arbeitsmarkt_anforderungen_server("beruf_arbeitsmarkt_anforderungen_1")
