
#' beruf_arbeitsmarkt_landkreis_map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'
mod_beruf_arbeitsmarkt_regional_verlauf_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Auswahl des Jahres:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_beruf_arbeitsmarkt_landkreis_verlauf"),
      label = NULL,
      choices = 2013:2023,
      selected = c(2017, 2023)
    ),
    p("Bundesland:"),
    shinyWidgets::pickerInput(
      inputId = ns("states_beruf_arbeitsmarkt_landkreis_verlauf"),
      choices = c("Baden-Württemberg",
                  "Bayern",
                  #"Berlin",
                  "Brandenburg",
                  "Bremen",
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

    p("Landkreise:"),
    shinyWidgets::pickerInput(
      inputId = ns("kreise_beruf_arbeitsmarkt_landkreis_verlauf"),
      choices = NULL, # Initial leer
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `deselect-all-text` = "Alle abwählen",
        `select-all-text` = "Alle auswählen",
        `live-search` = TRUE
      )
    ),

    hr(),
    p("Beschäftigtengruppen:"),
    shinyWidgets::pickerInput(
      inputId = ns("kategorie_beruf_arbeitsmarkt_landkreis_verlauf"),
      choices = c("Auszubildende",
                  "Auszubildende mit neuem Lehrvertrag" = "Auszubildende (1. Jahr)",
                  "Beschäftigte",
                  "weibliche Beschäftigte",
                  "weibliche Auszubildende",
                  "ausländische Auszubildende",
                  "ausländische Beschäftigte",
                  "Beschäftigte u25",
                  "Beschäftigte 25-55",
                  "Beschäftigte ü55"),
      multiple = FALSE,
      selected = "Beschäftigte"
    ),

    p("Fachbereich:"),
    shinyWidgets::pickerInput(
      inputId = ns("fachbereich_beruf_arbeitsmarkt_landkreis_verlauf"),
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

    p("Darstellungsart:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("abs_zahlen_beruf_arbeitsmarkt_landkreis_verlauf"),
      choices = c("In Prozent", "Anzahl"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
    ),

    br(),

    shinyBS::bsPopover(id="ih_beruf_regional_v1", title="",
                       content = paste0("Die Karte in der ersten Einstellung zeigt beispielsweise, dass 2022 in Ludwigshafen am Rhein (unten, seitlich rechts, dunkelblaue Stelle) mit 38% der größte Anteil an MINT-Beschäftigten in Rheinland-Pfalz arbeitet."),
                       placement = "top",
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_beruf_regional_v1")
  )
}

#' beruf_arbeitsmarkt_landkreis_map Server Functions
#'
#' @noRd
mod_beruf_arbeitsmarkt_regional_verlauf_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    selected_region <- reactive({
      input$states_beruf_arbeitsmarkt_landkreis_verlauf
    })

    # output$lk_auswahl <- renderUI({
    #
    #   regio <- selected_region()
    #   lk_auswahl <- get_lks(regio)
    #
    #     shinyWidgets::pickerInput(
    #       inputId = "kreise_beruf_arbeitsmarkt_landkreis_verlauf",
    #       choices = lk_auswahl,
    #       multiple = TRUE,
    #       options = list(`actions-box` = TRUE,
    #                      `deselect-all-text` = "Alle abwählen",
    #                      `select-all-text` = "Alle auswählen"),
    #       selected = lk_auswahl
    #     )
    #
    # })

    observeEvent(input$date_beruf_arbeitsmarkt_landkreis_verlauf, {
      r$date_beruf_arbeitsmarkt_landkreis_verlauf <- input$date_beruf_arbeitsmarkt_landkreis_verlauf
    })

    observeEvent(input$states_beruf_arbeitsmarkt_landkreis_verlauf, {
      r$states_beruf_arbeitsmarkt_landkreis_verlauf <- input$states_beruf_arbeitsmarkt_landkreis_verlauf

      regio <- selected_region()
      lk_auswahl <- get_lks(regio)

          shinyWidgets::updatePickerInput(
            session= session,
            inputId = "kreise_beruf_arbeitsmarkt_landkreis_verlauf",
            choices = lk_auswahl,
            selected = lk_auswahl[1:6]
          )
    })

    observeEvent(input$kreise_beruf_arbeitsmarkt_landkreis_verlauf, {
      r$kreise_beruf_arbeitsmarkt_landkreis_verlauf <- input$kreise_beruf_arbeitsmarkt_landkreis_verlauf
    })

    observeEvent(input$kategorie_beruf_arbeitsmarkt_landkreis_verlauf, {
      r$kategorie_beruf_arbeitsmarkt_landkreis_verlauf <- input$kategorie_beruf_arbeitsmarkt_landkreis_verlauf
    })

    observeEvent(input$fachbereich_beruf_arbeitsmarkt_landkreis_verlauf, {
      r$fachbereich_beruf_arbeitsmarkt_landkreis_verlauf <- input$fachbereich_beruf_arbeitsmarkt_landkreis_verlauf
    })

    observeEvent(input$abs_zahlen_beruf_arbeitsmarkt_landkreis_verlauf, {
      r$abs_zahlen_beruf_arbeitsmarkt_landkreis_verlauf <- input$abs_zahlen_beruf_arbeitsmarkt_landkreis_verlauf
    })


  })
}

## To be copied in the UI
# mod_beruf_arbeitsmarkt_landkreis_map_ui("beruf_arbeitsmarkt_landkreis_map_1")

## To be copied in the server
# mod_beruf_arbeitsmarkt_landkreis_map_server("beruf_arbeitsmarkt_landkreis_map_1")
