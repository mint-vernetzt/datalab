#' beruf_arbeitsmarkt_bl_vergleich UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_beruf_arbeitsmarkt_bl_vergleich_ui <- function(id){
  ns <- NS(id)
  tagList(

    p("Auswahl des Jahres:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_bl_vergl"),
      label = NULL,
      choices = 2021:2022,
      selected = 2022
    ),
    p("Beschäftigungsform:"),
    conditionalPanel(condition = "input.date_bl_vergl == '2022'",
                     ns = ns,
                     shinyWidgets::pickerInput(
                       inputId = ns("indikator_bl_vergl_22"),
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
                     )),
    conditionalPanel(condition = "input.date_bl_vergl == '2021'",
                     ns = ns,
                     shinyWidgets::pickerInput(
                       inputId = ns("indikator_bl_vergl_21"),
                       choices = c("Auszubildende",
                                   "Auszubildende mit neuem Lehrvertrag" = "Auszubildende (1. Jahr)",
                                   "Beschäftigte",
                                   "ausländische Auszubildende",
                                   "ausländische Beschäftigte",
                                   "Beschäftigte 25-55",
                                   "Beschäftigte u25",
                                   "Beschäftigte ü55"),
                       selected = "Beschäftigte",
                       multiple = FALSE)),

    p("Berufsfeld:"),
    shinyWidgets::pickerInput(
      inputId = ns("feld_bl_vergl"),
      choices = c(
        "MINT",
        "Mathematik/ Naturwissenschaften" = "Mathematik, Naturwissenschaften",
        "Informatik",
        "Technik (gesamt)",
        "Bau- und Gebäudetechnik",
        "Landtechnik",
        "Produktionstechnik",
        "Gesundheitstechnik",
         "Verkehrs-, Sicherheits- u. Veranstaltungstechnik"),
      # justified = TRUE,
      # checkIcon = list(yes = icon("ok",
      #                             lib = "glyphicon")),
      selected="Informatik"
    ),
    br(),
    shinyBS::bsPopover(id="ih_beruf_fach_3", title="",
                       content = paste0("Diese Darstellung gibt einen Überblick darürber, wie hoch der Anteil von MINT-Beschäftigten in den Bundesländern ist. Beispielsweise sind 2022 etwa 3,5 % der Beschäftigten in Bayern im Bereich Informatik tätig. Damit liegt Bayern etwas über dem gesamtdeutschen Durchschnitt von knapp 3 %."),
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_beruf_fach_3")
    # p("Auswahl des Anforderungsniveaus:"),
    # shinyWidgets::pickerInput(
    #   inputId = ns("anforderungsniveau_beruf_arbeitsmarkt_bl_vergleich"),
    #   choices = c("Gesamt", "Fachkraft",  "Spezialist:in"="Spezialist", "Expert:in"="Experte"),
    #   selected = "Gesamt"
    # )
  )
}

#' beruf_arbeitsmarkt_bl_vergleich Server Functions
#'
#' @noRd
mod_beruf_arbeitsmarkt_bl_vergleich_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$feld_bl_vergl, {
      r$feld_bl_vergl <- input$feld_bl_vergl
    })

    observeEvent(input$indikator_bl_vergl_21, {
      r$indikator_bl_vergl_21 <- input$indikator_bl_vergl_21
    })

    observeEvent(input$indikator_bl_vergl_22, {
      r$indikator_bl_vergl_22 <- input$indikator_bl_vergl_22
    })

    observeEvent(input$date_bl_vergl, {
      r$date_bl_vergl <- input$date_bl_vergl
    })

    # observeEvent(input$anforderungsniveau_beruf_arbeitsmarkt_bl_vergleich, {
    #   r$anforderungsniveau_beruf_arbeitsmarkt_bl_vergleich <- input$anforderungsniveau_beruf_arbeitsmarkt_bl_vergleich
    # })

  })
}

## To be copied in the UI
# mod_beruf_arbeitsmarkt_bl_vergleich_ui("beruf_arbeitsmarkt_bl_vergleich_1")

## To be copied in the server
# mod_beruf_arbeitsmarkt_bl_vergleich_server("beruf_arbeitsmarkt_bl_vergleich_1")
