#' beruf_arbeitsmarkt_entgelt_vergleich UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd ################################################################### TabEins - Balkendiagramm
#'
#' @importFrom shiny NS tagList
mod_beruf_arbeitsmarkt_entgelt_vergleich_ui <- function(id){
  ns <- NS(id)
  tagList(


    #farbe auswahl
    tags$head(
      tags$style(HTML("
        .dropdown-menu .bs-actionsbox .btn-group .btn {
          background-color: #e7f1ff !important;  /* Hellblau für die Alle auswählen/abwählen Buttons */
          color: #000000 !important;
        }
        .dropdown-menu .bs-actionsbox .btn-group .btn:hover {
          background-color: #d0e8ff !important;  /* Etwas dunkleres Blau beim Hover */
          color: #000000 !important;
        }
      "))
    ),


    p("Auswahl des Jahres:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_arbeitsmarkt_entgelt_vergleich"),
      label = NULL,
      choices = 2013:2024,
      selected =2024
    ),
    p("Region:"),
    shinyWidgets::pickerInput(
      inputId = ns("region_arbeitsmarkt_entgelt_vergleich"),
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
    p("Geschlecht:"),
    shinyWidgets::pickerInput(
      inputId = ns("beruf_arbeitsmarkt_entgel_geschlecht"),
      choices = c("Insgesamt",
                  "Frauen",
                  "Männer"),
      multiple = FALSE,
      selected = "Insgesamt"
    ),
    p("Berufslevel:"),
    shinyWidgets::pickerInput(
      inputId = ns("beruf_arbeitsmarkt_entgelt_berufslev"),
      choices = c("Gesamt",
                  "Fachkraft",
                  "Spezialist",
                  "Experte"),
      selected = c("Gesamt"),
      multiple = FALSE
    ),



                     # p("Darstellungsart:"),
                     # shinyWidgets::radioGroupButtons(
                     #   inputId = ns("abs_zahlen_arbeitsmarkt_entgelt_vergleich"),
                     #   choices = c("In Prozent", "Anzahl"),
                     #   justified = TRUE,
                     #   checkIcon = list(yes = icon("ok",
                     #                               lib = "glyphicon"))
                     # ),
                     shinyBS::bsPopover(id="ih_beruf_mint_3_entgel", title="",
                                        content = paste0("Das höchste mittlere Brutto-Entgelt verdient man in der Informatik (5.902 Euro). Das liegt, genauso wie das mittlere Entgelt in MINT, über dem deutschen Durchschnitt von 4.013 Euro."),
                                        trigger = "hover"),
                     tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_beruf_mint_3_entgel")

  )
}

#' beruf_arbeitsmarkt_entgelt_vergleich Server Functions
#'
#' @noRd
mod_beruf_arbeitsmarkt_entgelt_vergleich_server <- function(id, r){
  moduleServer( id, function(input, output, session){


    observeEvent(input$date_arbeitsmarkt_entgelt_vergleich, {
      r$date_arbeitsmarkt_entgelt_vergleich <- input$date_arbeitsmarkt_entgelt_vergleich
    })


    observeEvent(input$region_arbeitsmarkt_entgelt_vergleich, {
      r$region_arbeitsmarkt_entgelt_vergleich <- input$region_arbeitsmarkt_entgelt_vergleich
    })

    observeEvent(input$beruf_arbeitsmarkt_entgelt_berufslev, {
      r$beruf_arbeitsmarkt_entgelt_berufslev <- input$beruf_arbeitsmarkt_entgelt_berufslev
    })

    observeEvent(input$beruf_arbeitsmarkt_entgel_geschlecht, {
      r$beruf_arbeitsmarkt_entgel_geschlecht <- input$beruf_arbeitsmarkt_entgel_geschlecht
    })





    # observeEvent(input$indikator_arbeitsmarkt_einsteig_vergleich_balken, {
    #   r$indikator_arbeitsmarkt_einsteig_vergleich_balken <- input$indikator_arbeitsmarkt_einsteig_vergleich_balken
    # })
    #

    # observeEvent(input$indikator_arbeitsmarkt_einsteig_vergleich_kuchen, {
    #   r$indikator_arbeitsmarkt_einsteig_vergleich_kuchen <- input$indikator_arbeitsmarkt_einsteig_vergleich_kuchen
    # })


    # observeEvent(input$abs_zahlen_arbeitsmarkt_entgelt_vergleich, {
    #   r$abs_zahlen_arbeitsmarkt_entgelt_vergleich <- input$abs_zahlen_arbeitsmarkt_entgelt_vergleich
    # })

  })
}

## To be copied in the UI
# mod_beruf_arbeitsmarkt_entgelt_vergleich_ui("beruf_arbeitsmarkt_entgelt_vergleich_1")

## To be copied in the server
# mod_beruf_arbeitsmarkt_entgelt_vergleich_server("beruf_arbeitsmarkt_entgelt_vergleich_1")
