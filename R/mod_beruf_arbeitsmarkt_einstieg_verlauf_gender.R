#' beruf_arbeitsmarkt_einstieg_verlauf_gender UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_beruf_arbeitsmarkt_einstieg_verlauf_gender_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Jahre:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_arbeitsmarkt_verlauf_gender"),
      label = NULL,
      choices = 2013:2024,
      selected = c(2017, 2024)
    ),
    p("Beschäftigtengruppe:"),
    shinyWidgets::pickerInput(
      inputId = ns("indikator_arbeitsmarkt_verlauf_gender"),
      choices = c("Auszubildende",
                  "Auszubildende mit neuem Lehrvertrag" = "Auszubildende (1. Jahr)",
                  "Beschäftigte",
                  "ausländische Auszubildende",
                  "ausländische Beschäftigte"),
      selected = c("Beschäftigte", "Auszubildende"),
      multiple = TRUE
    ),
    p("Berufsfeld:"),
    shinyWidgets::pickerInput(
      inputId = ns("fachbereich_arbeitsmarkt_verlauf_gender"),
      choices = c( "MINT",
                   "Mathematik/ Naturwissenschaften" = "Mathematik, Naturwissenschaften",
                   "Informatik",
                   "Technik (gesamt)",
                   "Bau- und Gebäudetechnik",
                   "Gesundheitstechnik",
                   "Landtechnik",
                   "Produktionstechnik",
                   "Verkehrs-, Sicherheits- u. Veranstaltungstechnik"
      ),
      selected = "MINT"
    ),
    p("Region:"),
    shinyWidgets::pickerInput(
      inputId = ns("region_arbeitsmarkt_verlauf_gender"),
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
    p("Darstellungsart:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("abs_zahlen_arbeitsmarkt_verlauf_gender"),
      choices = c("In Prozent", "Anzahl"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
    ),
    br(),
    shinyBS::bsPopover(id="ih_beruf_frauen_2", title="",
                       content = paste0("Der Zeitverlauf zeigt, dass der Frauenanteil in MINT in den letzten Jahren etwas gestiegen ist (ca.1 Prozentpunkt). Vergleicht man die absolute Anzahl kann man ablesen, dass 2023 im Vergleich zu 2017 rund 200.000 Frauen mehr in MINT gibt."),
                       trigger = "hover", placement = "top"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_beruf_frauen_2")
  )

}

#' beruf_arbeitsmarkt_einstieg_verlauf_gender Server Functions
#'
#' @noRd
mod_beruf_arbeitsmarkt_einstieg_verlauf_gender_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_arbeitsmarkt_verlauf_gender, {
      r$date_arbeitsmarkt_verlauf_gender <- input$date_arbeitsmarkt_verlauf_gender
    })

    observeEvent(input$indikator_arbeitsmarkt_verlauf_gender, {
      r$indikator_arbeitsmarkt_verlauf_gender <- input$indikator_arbeitsmarkt_verlauf_gender
    })

    observeEvent(input$fachbereich_arbeitsmarkt_verlauf_gender, {
      r$fachbereich_arbeitsmarkt_verlauf_gender <- input$fachbereich_arbeitsmarkt_verlauf_gender
    })

    observeEvent(input$region_arbeitsmarkt_verlauf_gender, {
      r$region_arbeitsmarkt_verlauf_gender <- input$region_arbeitsmarkt_verlauf_gender
    })

    observeEvent(input$abs_zahlen_arbeitsmarkt_verlauf_gender, {
      r$abs_zahlen_arbeitsmarkt_verlauf_gender <- input$abs_zahlen_arbeitsmarkt_verlauf_gender
    })

  })
}

## To be copied in the UI
# mod_beruf_arbeitsmarkt_einstieg_verlauf_gender_ui("beruf_arbeitsmarkt_einstieg_verlauf_gender_1")

## To be copied in the server
# mod_beruf_arbeitsmarkt_einstieg_verlauf_gender_server("beruf_arbeitsmarkt_einstieg_verlauf_gender_1")
