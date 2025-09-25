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
    p("Auswahl des Jahres:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_beruf_arbeitsmarkt_landkreis_vergleich"),
      label = NULL,
      choices = 2013:2024,
      selected = 2024
    ),
    p("Region:"),
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
    p("Indikatoren:"),
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
                                   "nur Auszubildende mit neuem Lehrvertrag" = "Auszubildende (1. Jahr)"
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
    hr(),
    p("Form der Darstellung:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("darstellung_beruf_arbeitsmarkt_landkreis_vergleich"),
      choices = c("In Prozent", "Anzahl"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
    ),
    br(),
    shinyBS::bsPopover(id="ih_beruf_regional_2", title="",
                       content = paste0("In der ersten Darstellung sieht man, welche Landkreise in Hessen 2022 einen Anteil an MINT-Beschäftigten haben, der über oder unter dem Durchschnitt des Bundeslands liegt. Frankfurt am Main hat beispielsweise einen weniger hohen Anteil an MINT-Beschäftigten als die meisten anderen Kreise und kreisfreien Städte in Hessen. "),
                       placement = "top",
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_beruf_regional_2"),
    br(),
  )
}

#' beruf_arbeitsmarkt_landkreis_vergleich Server Functions
#'
#' @noRd
mod_beruf_arbeitsmarkt_landkreis_vergleich_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$date_beruf_arbeitsmarkt_landkreis_vergleich, {
      r$date_beruf_arbeitsmarkt_landkreis_vergleich <- input$date_beruf_arbeitsmarkt_landkreis_vergleich
    })

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
