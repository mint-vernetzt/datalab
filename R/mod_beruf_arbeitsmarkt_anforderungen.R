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
    p("Betrachtungsart:"),
    shiny::radioButtons(
      inputId = ns("ansicht_arbeitsmarkt_fach_vergleich"),
      label = NULL,
      choices = c("Einzelansicht - Kuchendiagramm", "Gruppenvergleich - Balkendiagramm"),
      selected = "Einzelansicht - Kuchendiagramm"
    ),
    p("Auswahl des Jahres:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_arbeitsmarkt_fach_vergleich"),
      label = NULL,
      choices = 2021:2022,
      selected = 2022
    ),
    p("Region:"),
    shinyWidgets::pickerInput(
      inputId = ns("region_arbeitsmarkt_fach_vergleich"),
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

    conditionalPanel(condition = "input.ansicht_arbeitsmarkt_fach_vergleich == 'Gruppenvergleich - Balkendiagramm'",
                     ns = ns,
                     p("Beschäftigtengruppe:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("indikator_arbeitsmarkt_fach_vergleich_balken"),
                       choices = c("Auszubildende",
                                   "Auszubildende mit neuem Lehrvertrag" = "Auszubildende (1. Jahr)",
                                   "Beschäftigte",
                                   "ausländische Auszubildende",
                                   "ausländische Beschäftigte",
                                   "Beschäftigte 25-55",
                                   "Beschäftigte u25",
                                   "Beschäftigte ü55"),
                       selected = "Beschäftigte",
                       multiple = FALSE
                     ),
                     br(),
                     shinyBS::bsPopover(id="ih_beruf_fach_2", title="",
                                        content = paste0("Die Grafik mit der ersten Einstellung zeigt, dass in Sachsen-Anhalt im Jahr 2022 21 % der Beschäftigten in MINT arbeiten. Den größten Anteil machen dabei die ca. 142.000 Beschäftigten im Berufsfeld Technik aus."),
                                        trigger = "hover"),
                     tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_beruf_fach_2")

    ),

    conditionalPanel(condition = "input.ansicht_arbeitsmarkt_fach_vergleich == 'Einzelansicht - Kuchendiagramm'",
                     ns = ns,
                     p("Beschäftigtengruppe (max. 2):"),
                     shinyWidgets::pickerInput(
                       inputId = ns("indikator_arbeitsmarkt_fach_vergleich_pies"),
                       choices = c("Auszubildende",
                                   "Auszubildende mit neuem Lehrvertrag" = "Auszubildende (1. Jahr)",
                                   "Beschäftigte",
                                   "ausländische Auszubildende",
                                   "ausländische Beschäftigte",
                                   "Beschäftigte 25-55",
                                   "Beschäftigte u25",
                                   "Beschäftigte ü55"),
                       selected = "Beschäftigte",
                       multiple = TRUE,
                       options =  list(
                         "max-options" = 2,
                         "max-options-text" = "Maximal 2 Indikatoren auswählen")
                     ),
                     p("Nicht-MINT Berufsfelder mit anzeigen?", style = "color: #b16fab;"),
                     shinyWidgets::radioGroupButtons(
                       inputId = ns("gegenwert_arbeitsmarkt_fach_vergleich"),
                       choices = c("Ja", "Nein"),
                       selected = "Ja",
                       justified = TRUE,
                       checkIcon = list(yes = icon("ok",
                                                   lib = "glyphicon"))
                     ),
                     br(),
                     shinyBS::bsPopover(id="ih_beruf_mint_1", title="",
                                        content = paste0("In der ersten Einstellung ist zu sehen, dass in Deutschland im Jahr 2022 23 % der Beschäftigten in MINT beschäftigt waren (2 + 3 + 18 = 23 %). Bei den Auszubildenden waren dies 30 %."),
                                        trigger = "hover"),
                     tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_beruf_mint_1")

    )
  )
}

#' beruf_arbeitsmarkt_anforderungen Server Functions
#'
#' @noRd
mod_beruf_arbeitsmarkt_anforderungen_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$ansicht_arbeitsmarkt_fach_vergleich, {
      r$ansicht_arbeitsmarkt_fach_vergleich <- input$ansicht_arbeitsmarkt_fach_vergleich
    })
    observeEvent(input$date_arbeitsmarkt_fach_vergleich, {
      r$date_arbeitsmarkt_fach_vergleich <- input$date_arbeitsmarkt_fach_vergleich
    })
    observeEvent(input$region_arbeitsmarkt_fach_vergleich, {
      r$region_arbeitsmarkt_fach_vergleich <- input$region_arbeitsmarkt_fach_vergleich
    })
    observeEvent(input$indikator_arbeitsmarkt_fach_vergleich_pies, {
      r$indikator_arbeitsmarkt_fach_vergleich_pies <- input$indikator_arbeitsmarkt_fach_vergleich_pies
    })
    observeEvent(input$indikator_arbeitsmarkt_fach_vergleich_balken, {
      r$indikator_arbeitsmarkt_fach_vergleich_balken <- input$indikator_arbeitsmarkt_fach_vergleich_balken
    })
    observeEvent(input$gegenwert_arbeitsmarkt_fach_vergleich, {
      r$gegenwert_arbeitsmarkt_fach_vergleich <- input$gegenwert_arbeitsmarkt_fach_vergleich
    })

  })
}

## To be copied in the UI
# mod_beruf_arbeitsmarkt_anforderungen_ui("beruf_arbeitsmarkt_anforderungen_1")

## To be copied in the server
# mod_beruf_arbeitsmarkt_anforderungen_server("beruf_arbeitsmarkt_anforderungen_1")
