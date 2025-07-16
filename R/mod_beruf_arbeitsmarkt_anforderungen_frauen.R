#' beruf_arbeitsmarkt_anforderungen UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_beruf_arbeitsmarkt_anforderungen_frauen_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Auswahl des Jahres:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_arbeitsmarkt_fach_vergleich_frauen"),
      label = NULL,
      choices = 2013:2023,
      selected = 2023
    ),
    p("Region:"),
    shinyWidgets::pickerInput(
      inputId = ns("region_arbeitsmarkt_fach_vergleich_frauen"),
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
                       inputId = ns("indikator_arbeitsmarkt_fach_vergleich_balken_frauen"),
                       choices = c("weibliche Auszubildende",
                                   "weibliche Auszubildende mit neuem Lehrvertrag" = "Auszubildende (1. Jahr)",
                                   "weibliche Beschäftigte",
                                   "weibliche ausländische Auszubildende",
                                   "weibliche ausländische Beschäftigte"),
                       selected = "weibliche Beschäftigte",
                       multiple = FALSE
                     ),
                     br(),
                     shinyBS::bsPopover(id="ih_beruf_fach_2_frr", title="",
                                        content = paste0("Die Grafik mit der ersten Einstellung zeigt, dass in Deutschland im Jahr 2023 rund 77 % der Beschäftigten in Nicht-MINT Berufen tätig sind. In den MINT-Berufen dominiert die Technik: 18.1 % der Beschäftigten gehen beruflich einer Tätigkeit in der Technik nach."),
                                        trigger = "hover"),
                     tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_beruf_fach_2_frr")

  )
}

#' beruf_arbeitsmarkt_anforderungen Server Functions
#'
#' @noRd
mod_beruf_arbeitsmarkt_anforderungen_frauen_server <- function(id, r){
  moduleServer( id, function(input, output, session){


    observeEvent(input$date_arbeitsmarkt_fach_vergleich_frauen, {
      r$date_arbeitsmarkt_fach_vergleich_frauen <- input$date_arbeitsmarkt_fach_vergleich_frauen
    })
    observeEvent(input$region_arbeitsmarkt_fach_vergleich_frauen, {
      r$region_arbeitsmarkt_fach_vergleich_frauen <- input$region_arbeitsmarkt_fach_vergleich_frauen
    })

    observeEvent(input$indikator_arbeitsmarkt_fach_vergleich_balken_frauen, {
      r$indikator_arbeitsmarkt_fach_vergleich_balken_frauen <- input$indikator_arbeitsmarkt_fach_vergleich_balken_frauen
    })

  })
}
