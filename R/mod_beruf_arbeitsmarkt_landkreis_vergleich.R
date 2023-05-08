#' beruf_arbeitsmarkt_landkreis_vergleich UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_beruf_arbeitsmarkt_landkreis_vergleich_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Auswahl Bundesland:"),
    shinyWidgets::pickerInput(
      inputId = ns("states_beruf_arbeitsmarkt_landkreis_vergleich"),
      choices = c("Baden-Württemberg",
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
                  "Thüringen"
      ),
      multiple = FALSE,
      selected = c("Hessen")
    ),
    hr(),
    p("Auswahl:"),
    shinyWidgets::pickerInput(
      inputId = ns("kategorie_beruf_arbeitsmarkt_landkreis_vergleich"),
      choices = c("Auszubildende",
                  "Beschäftigte"
      ),
      multiple = FALSE,
      selected = "Beschäftigte"
    ),
    shinyWidgets::pickerInput(
      inputId = ns("fachbereich_beruf_arbeitsmarkt_landkreis_vergleich"),
      choices = c("Gesamt" = "Alle",
                  "in MINT" = "MINT",
                  "im Bereich Mathematik / Naturwissenschaften" = "Mathematik, Naturwissenschaften",
                  "im Bereich Informatik" = "Informatik",
                  "im Bereich Technik" = "Technik (gesamt)",
                  "im Bereich Technik - Landtechnik" = "Landtechnik",
                  "im Bereich Technik - Produktionstechnik" = "Produktionstechnik",
                  "im Bereich Technik - Bau- und Gebäudetechnik" = "Bau- und Gebäudetechnik",
                  "im Bereich Technik - Verkehrs-, Sicherheits- und Veranstaltungstechnik" = "Verkehrs-, Sicherheits- u. Veranstaltungstechnik",
                  "im Bereich Technik - Gesundheitstechnik" = "Gesundheitstechnik"
      ),
      selected = "MINT",
      multiple = FALSE
    ),
    conditionalPanel(condition = "input.kategorie_beruf_arbeitsmarkt_landkreis_vergleich == 'Auszubildende'",
                     ns = ns,
                     shinyWidgets::pickerInput(
                       inputId = ns("indikator1_beruf_arbeitsmarkt_landkreis_vergleich"),
                       choices = c("Gesamt (alle der Hauptkategorie)" = "Auszubildende",
                                   "nur weiblich" = "Frauen",
                                   "nur ausländisch" = "ausländische Auszubildende",
                                   "nur Auszubildende im 1. Lehrjahr" = "Auszubildende (1. Jahr)"
                       ),
                       selected = "Gesamt (alle der Hauptkategorie)",
                       multiple = FALSE
                     )),
    conditionalPanel(condition = "input.kategorie_beruf_arbeitsmarkt_landkreis_vergleich == 'Beschäftigte'",
                     ns = ns,
                     shinyWidgets::pickerInput(
                       inputId = ns("indikator2_beruf_arbeitsmarkt_landkreis_vergleich"),
                       choices = c("Gesamt (alle der Hauptkategorie)" = "Beschäftigte",
                                   "nur weiblich" = "Frauen",
                                   "nur ausländisch" = "ausländische Beschäftigte",
                                   "nur Beschäftigte u25" = "Beschäftigte u25",
                                   "nur Beschäftigte 25-55" = "Beschäftigte 25-55",
                                   "nur Beschäftigte ü55" =  "Beschäftigte ü55"
                       ),
                       selected = "Gesamt (alle der Hauptkategorie)",
                       multiple = FALSE
                     )),
    p("Form der Darstellung:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("darstellung_beruf_arbeitsmarkt_landkreis_vergleich"),
      choices = c("In Prozent", "Anzahl"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
    ),
  )
}

#' beruf_arbeitsmarkt_landkreis_vergleich Server Functions
#'
#' @noRd
mod_beruf_arbeitsmarkt_landkreis_vergleich_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$states_beruf_arbeitsmarkt_landkreis_vergleich, {
      r$states_beruf_arbeitsmarkt_landkreis_vergleich <- input$states_beruf_arbeitsmarkt_landkreis_vergleich
    })

    observeEvent(input$kategorie_beruf_arbeitsmarkt_landkreis_vergleich, {
      r$kategorie_beruf_arbeitsmarkt_landkreis_vergleich <- input$kategorie_beruf_arbeitsmarkt_landkreis_vergleich
    })

    observeEvent(input$fachbereich_beruf_arbeitsmarkt_landkreis_vergleich, {
      r$fachbereich_beruf_arbeitsmarkt_landkreis_vergleich <- input$fachbereich_beruf_arbeitsmarkt_landkreis_vergleich
    })

    observeEvent(input$indikator1_beruf_arbeitsmarkt_landkreis_vergleich, {
      r$indikator1_beruf_arbeitsmarkt_landkreis_vergleich <- input$indikator1_beruf_arbeitsmarkt_landkreis_vergleich
    })

    observeEvent(input$indikator2_beruf_arbeitsmarkt_landkreis_vergleich, {
      r$indikator2_beruf_arbeitsmarkt_landkreis_vergleich <- input$indikator2_beruf_arbeitsmarkt_landkreis_vergleich
    })

    observeEvent(input$darstellung_beruf_arbeitsmarkt_landkreis_vergleich, {
      r$darstellung_beruf_arbeitsmarkt_landkreis_vergleich <- input$darstellung_beruf_arbeitsmarkt_landkreis_vergleich
    })

  })
}

## To be copied in the UI
# mod_beruf_arbeitsmarkt_landkreis_vergleich_ui("beruf_arbeitsmarkt_landkreis_vergleich_1")

## To be copied in the server
# mod_beruf_arbeitsmarkt_landkreis_vergleich_server("beruf_arbeitsmarkt_landkreis_vergleich_1")
