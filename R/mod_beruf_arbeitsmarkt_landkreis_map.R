#' beruf_arbeitsmarkt_landkreis_map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_beruf_arbeitsmarkt_landkreis_map_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Auswahl des Jahres:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_beruf_arbeitsmarkt_landkreis_karte"),
      label = NULL,
      choices = 2013:2022,
      selected = 2022
    ),
    p("Region:"),
    shinyWidgets::pickerInput(
      inputId = ns("states_beruf_arbeitsmarkt_landkreis_karte"),
      choices = c("Baden-Württemberg",
                  "Bayern",
                  #"Berlin",
                  "Brandenburg",
                  #"Bremen",
                  #"Hamburg",
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
      selected = c("Rheinland-Pfalz")
    ),
    hr(),
    p("Beschäftigtengruppen:"),
    shinyWidgets::pickerInput(
      inputId = ns("kategorie_beruf_arbeitsmarkt_landkreis_karte1"),
      choices = c("Auszubildende",
                  "Beschäftigte"
      ),
      multiple = FALSE,
      selected = "Beschäftigte"
    ),
    p("Gruppeneigenschaft:"),
    conditionalPanel(condition = "input.kategorie_beruf_arbeitsmarkt_landkreis_karte1 == 'Auszubildende'",
                     ns = ns,
                     shinyWidgets::pickerInput(
                       inputId = ns("indikator1_beruf_arbeitsmarkt_landkreis_karte1"),
                       choices = c("Gesamt (alle der Hauptkategorie)" = "Auszubildende",
                                   "nur weiblich" = "Frauen",
                                   "nur ausländisch" = "ausländische Auszubildende",
                                   "nur Auszubildende mit neuem Lehrvertrag" = "Auszubildende (1. Jahr)"
                                   ),
                       selected = "Gesamt (alle der Hauptkategorie)",
                       multiple = FALSE
                     )),
    conditionalPanel(condition = "input.kategorie_beruf_arbeitsmarkt_landkreis_karte1 == 'Beschäftigte'",
                     ns = ns,
                     shinyWidgets::pickerInput(
                       inputId = ns("indikator2_beruf_arbeitsmarkt_landkreis_karte1"),
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
    p("Fachbereich:"),
    shinyWidgets::pickerInput(
      inputId = ns("fachbereich_beruf_arbeitsmarkt_landkreis_karte1"),
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
    # ,
    # hr(),
    # p("Indikatoren Darstellung rechts:"),
    # shinyWidgets::pickerInput(
    #   inputId = ns("kategorie_beruf_arbeitsmarkt_landkreis_karte2"),
    #   choices = c("Auszubildende",
    #               "Beschäftigte"
    #   ),
    #   multiple = FALSE,
    #   selected = "Beschäftigte"
    # ),
    # shinyWidgets::pickerInput(
    #   inputId = ns("fachbereich_beruf_arbeitsmarkt_landkreis_karte2"),
    #   choices = c("Gesamt" = "Alle",
    #               "in MINT" = "MINT",
    #               "im Bereich Mathematik / Naturwissenschaften" = "Mathematik, Naturwissenschaften",
    #               "im Bereich Informatik" = "Informatik",
    #               "im Bereich Technik" = "Technik (gesamt)",
    #               "im Bereich Technik - Landtechnik" = "Landtechnik",
    #               "im Bereich Technik - Produktionstechnik" = "Produktionstechnik",
    #               "im Bereich Technik - Bau- und Gebäudetechnik" = "Bau- und Gebäudetechnik",
    #               "im Bereich Technik - Verkehrs-, Sicherheits- und Veranstaltungstechnik" = "Verkehrs-, Sicherheits- u. Veranstaltungstechnik",
    #               "im Bereich Technik - Gesundheitstechnik" = "Gesundheitstechnik"
    #               ),
    #   selected = "MINT",
    #   multiple = FALSE
    # ),
    # conditionalPanel(condition = "input.kategorie_beruf_arbeitsmarkt_landkreis_karte2 == 'Auszubildende'",
    #                  ns = ns,
    #                  shinyWidgets::pickerInput(
    #                    inputId = ns("indikator1_beruf_arbeitsmarkt_landkreis_karte2"),
    #                    choices = c("Gesamt (alle der Hauptkategorie)" = "Auszubildende",
    #                                "nur weiblich" = "Frauen",
    #                                "nur ausländisch" = "ausländische Auszubildende",
    #                                "nur Auszubildende mit neuem Lehrvertrag" = "Auszubildende (1. Jahr)"
    #                                ),
    #                    selected = "Frauen",
    #                    multiple = FALSE
    #                  )),
    # conditionalPanel(condition = "input.kategorie_beruf_arbeitsmarkt_landkreis_karte2 == 'Beschäftigte'",
    #                  ns = ns,
    #                  shinyWidgets::pickerInput(
    #                    inputId = ns("indikator2_beruf_arbeitsmarkt_landkreis_karte2"),
    #                    choices = c("Gesamt (alle der Hauptkategorie)" = "Beschäftigte",
    #                                "nur weiblich" = "Frauen",
    #                                "nur ausländisch" = "ausländische Beschäftigte",
    #                                "nur Beschäftigte u25" = "Beschäftigte u25",
    #                                "nur Beschäftigte 25-55" = "Beschäftigte 25-55",
    #                                "nur Beschäftigte ü55" =  "Beschäftigte ü55"
    #                    ),
    #                    selected = "Frauen",
    #                    multiple = FALSE
    #                  )),
    br(),
    shinyBS::bsPopover(id="dh_beruf_regional_1", title = "",
                       content = paste0("Falls die Grafiken abgeschnitten dargestellt werden, bitte das gesamte Ansichtsfenster einmal verkleinern und dann wieder maximieren. Dann stellt sich das Seitenverhältnis des Desktops richtig ein."),
                       placement = "top",
                       trigger = "hover"),
    tags$a(paste0("Probleme bei der Darstellung"), icon("question-circle"), id = "dh_beruf_regional_1"),

    br(),
    br(),
    shinyBS::bsPopover(id="ih_beruf_regional_1", title="",
                       content = paste0("Die Karte in der ersten Einstellung zeigt beispielsweise, dass 2022 in Ludwigshafen am Rhein (unten, seitlich rechts, dunkelblaue Stelle) mit 38% der größte Anteil an MINT-Beschäftigten in Rheinland-Pfalz arbeitet."),
                       placement = "top",
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_beruf_regional_1")
  )
}

#' beruf_arbeitsmarkt_landkreis_map Server Functions
#'
#' @noRd
mod_beruf_arbeitsmarkt_landkreis_map_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$states_beruf_arbeitsmarkt_landkreis_karte, {
      r$states_beruf_arbeitsmarkt_landkreis_karte <- input$states_beruf_arbeitsmarkt_landkreis_karte
    })

    observeEvent(input$date_beruf_arbeitsmarkt_landkreis_karte, {
      r$date_beruf_arbeitsmarkt_landkreis_karte <- input$date_beruf_arbeitsmarkt_landkreis_karte
    })

    # first map
    observeEvent(input$kategorie_beruf_arbeitsmarkt_landkreis_karte1, {
      r$kategorie_beruf_arbeitsmarkt_landkreis_karte1 <- input$kategorie_beruf_arbeitsmarkt_landkreis_karte1
    })

    observeEvent(input$fachbereich_beruf_arbeitsmarkt_landkreis_karte1, {
      r$fachbereich_beruf_arbeitsmarkt_landkreis_karte1 <- input$fachbereich_beruf_arbeitsmarkt_landkreis_karte1
    })

    observeEvent(input$indikator1_beruf_arbeitsmarkt_landkreis_karte1, {
      r$indikator1_beruf_arbeitsmarkt_landkreis_karte1 <- input$indikator1_beruf_arbeitsmarkt_landkreis_karte1
    })

    observeEvent(input$indikator2_beruf_arbeitsmarkt_landkreis_karte1, {
      r$indikator2_beruf_arbeitsmarkt_landkreis_karte1 <- input$indikator2_beruf_arbeitsmarkt_landkreis_karte1
    })

    # second map
    # observeEvent(input$kategorie_beruf_arbeitsmarkt_landkreis_karte2, {
    #   r$kategorie_beruf_arbeitsmarkt_landkreis_karte2 <- input$kategorie_beruf_arbeitsmarkt_landkreis_karte2
    # })
    #
    # observeEvent(input$fachbereich_beruf_arbeitsmarkt_landkreis_karte2, {
    #   r$fachbereich_beruf_arbeitsmarkt_landkreis_karte2 <- input$fachbereich_beruf_arbeitsmarkt_landkreis_karte2
    # })
    #
    # observeEvent(input$indikator1_beruf_arbeitsmarkt_landkreis_karte2, {
    #   r$indikator1_beruf_arbeitsmarkt_landkreis_karte2 <- input$indikator1_beruf_arbeitsmarkt_landkreis_karte2
    # })
    #
    # observeEvent(input$indikator2_beruf_arbeitsmarkt_landkreis_karte2, {
    #   r$indikator2_beruf_arbeitsmarkt_landkreis_karte2 <- input$indikator2_beruf_arbeitsmarkt_landkreis_karte2
    # })


  })
}

## To be copied in the UI
# mod_beruf_arbeitsmarkt_landkreis_map_ui("beruf_arbeitsmarkt_landkreis_map_1")

## To be copied in the server
# mod_beruf_arbeitsmarkt_landkreis_map_server("beruf_arbeitsmarkt_landkreis_map_1")
