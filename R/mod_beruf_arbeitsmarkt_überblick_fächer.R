#' beruf_arbeitsmarkt_überblick_fächer UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_beruf_arbeitsmarkt_überblick_fächer_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Auswahl des Jahres:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_arbeitsmarkt_überblick_fächer"),
      label = NULL,
      choices = c(2021, 2022),
      selected = 2022
    ),

    p("Region:"),
    shinyWidgets::pickerInput(
      inputId = ns("state_arbeitsmarkt_überblick_fächer"),
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
                  "Ostdeutschland (einschl. Berlin)"
                  ),
      multiple = FALSE,
      selected = "Sachsen-Anhalt"
    ),
    p("Beschäftigungsform:"),
    conditionalPanel(condition = "input.date_arbeitsmarkt_überblick_fächer == '2022'",
                     ns = ns,
                     shinyWidgets::pickerInput(
                       inputId = ns("indikator_arbeitsmarkt_überblick_fächer_22"),
                       choices = c("Auszubildende",
                                   #"Auszubildende (1. Jahr)", vorerst raus fehlt noch
                                   "Beschäftigte",
                                   "ausländische Beschäftigte",
                                   "ausländische Auszubildende",
                                   "Beschäftigte u25",
                                   "Beschäftigte 25-55",
                                   "Beschäftigte ü55"),
                       selected = "Beschäftigte",
                       multiple = FALSE
                     )),
    conditionalPanel(condition = "input.date_arbeitsmarkt_überblick_fächer == '2021'",
                     ns = ns,
                     shinyWidgets::pickerInput(
                       inputId = ns("indikator_arbeitsmarkt_überblick_fächer_21"),
                       choices = c(
                                   "Auszubildende",
                                   "Auszubildende (1. Jahr)",
                                   "Beschäftigte",
                                   "ausländische Beschäftigte",
                                   "ausländische Auszubildende",
                                   "Beschäftigte u25",
                                   "Beschäftigte 25-55",
                                   "Beschäftigte ü55"),
                       selected = "Beschäftigte",
                       multiple = FALSE)),

    br(),
    shinyBS::bsPopover(id="ih_beruf_fach_2", title="",
                       content = paste0("Die Grafik mit der ersten Einstellung zeigt, dass in Sachsen-Anhalt 21,1 % der Beschäftigten in MINT arbeiten. Den größten Anteil machen dabei die ca. 143.000 Beschäftigten im Berufsfeld Technik aus."),
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_beruf_fach_2")
    )

}

#' beruf_arbeitsmarkt_überblick_fächer Server Functions
#'
#' @noRd
mod_beruf_arbeitsmarkt_überblick_fächer_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_arbeitsmarkt_überblick_fächer, {
      r$date_arbeitsmarkt_überblick_fächer <- input$date_arbeitsmarkt_überblick_fächer
    })

    observeEvent(input$state_arbeitsmarkt_überblick_fächer, {
      r$state_arbeitsmarkt_überblick_fächer <- input$state_arbeitsmarkt_überblick_fächer
    })

    observeEvent(input$indikator_arbeitsmarkt_überblick_fächer_21, {
      r$indikator_arbeitsmarkt_überblick_fächer_21 <- input$indikator_arbeitsmarkt_überblick_fächer_21
    })

    observeEvent(input$indikator_arbeitsmarkt_überblick_fächer_22, {
      r$indikator_arbeitsmarkt_überblick_fächer_22 <- input$indikator_arbeitsmarkt_überblick_fächer_22
    })

  })
}

## To be copied in the UI
# mod_beruf_arbeitsmarkt_überblick_fächer_ui

## To be copied in the server
# mod_beruf_arbeitsmarkt_überblick_fächer_server
