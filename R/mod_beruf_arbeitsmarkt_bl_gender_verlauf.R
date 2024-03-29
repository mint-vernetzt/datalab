#' beruf_arbeitsmarkt_bl_gender_verlauf UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_beruf_arbeitsmarkt_bl_gender_verlauf_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Jahre:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_beruf_arbeitsmarkt_bl_gender_verlauf"),
      label = NULL,
      choices = c("2013", "2014", "2015", "2016", "2017",
                  "2018","2019", "2020", "2021", "2022"),
      selected = c("2017", "2022")
    ),
    p("Beschäftigungsform"),
    shinyWidgets::pickerInput(
      inputId = ns("indikator_beruf_arbeitsmarkt_bl_gender_verlauf"),
      choices = c("Auszubildende", "Beschäftigte"),
    ),
    # p("Auswahl des Anforderungsniveaus:"),
    # shinyWidgets::pickerInput(
    #   inputId = ns("anforderungsniveau_beruf_arbeitsmarkt_bl_gender_verlauf"),
    #   choices = c("Gesamt", "Fachkraft",  "Spezialist:in"="Spezialist", "Expert:in"="Experte"),
    #   selected = "Gesamt"
    # ),
    p("Regionen:"),
    shinyWidgets::pickerInput(
      inputId = ns("states_beruf_arbeitsmarkt_bl_gender_verlauf"),
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
                  "Thüringen"
                  ,
                  "Westdeutschland (o. Berlin)",
                  "Ostdeutschland (inkl. Berlin)"
                  ),
      multiple = TRUE,
      options = list(`actions-box` = TRUE,
                     `deselect-all-text` = "Alle abwählen",
                     `select-all-text` = "Alle auswählen"),
      selected = c("Deutschland", "Niedersachsen")
    ),
    p("Betrachtung:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("abs_zahlen_beruf_arbeitsmarkt_bl_gender_verlauf"),
      choices = c("In Prozent", "Anzahl"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
    ),

    br(),
    shinyBS::bsPopover(id="ih_beruf_mint_6", title="",
                       content = paste0("Die erste Einstellung zeigt beispielsweise, dass in Niedersachsen im Vergleich zu Deutschland der Anteil an Frauen, die eine MINT-Tätigkeit ergreifen, um ein bis zwei Prozentpunkte höher liegt."),
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_beruf_mint_6")
  )


}

#' beruf_arbeitsmarkt_bl_gender_verlauf Server Functions
#'
#' @noRd
mod_beruf_arbeitsmarkt_bl_gender_verlauf_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_beruf_arbeitsmarkt_bl_gender_verlauf, {
      r$date_beruf_arbeitsmarkt_bl_gender_verlauf <- input$date_beruf_arbeitsmarkt_bl_gender_verlauf
    })

    observeEvent(input$abs_zahlen_beruf_arbeitsmarkt_bl_gender_verlauf, {
      r$abs_zahlen_beruf_arbeitsmarkt_bl_gender_verlauf <- input$abs_zahlen_beruf_arbeitsmarkt_bl_gender_verlauf
    })


    observeEvent(input$indikator_beruf_arbeitsmarkt_bl_gender_verlauf, {
      r$indikator_beruf_arbeitsmarkt_bl_gender_verlauf <- input$indikator_beruf_arbeitsmarkt_bl_gender_verlauf
    })

    observeEvent(input$anforderungsniveau_beruf_arbeitsmarkt_bl_gender_verlauf, {
      r$anforderungsniveau_beruf_arbeitsmarkt_bl_gender_verlauf <- input$anforderungsniveau_beruf_arbeitsmarkt_bl_gender_verlauf
    })

    observeEvent(input$states_beruf_arbeitsmarkt_bl_gender_verlauf, {
      r$states_beruf_arbeitsmarkt_bl_gender_verlauf <- input$states_beruf_arbeitsmarkt_bl_gender_verlauf
    })

  })
}

## To be copied in the UI
# mod_beruf_arbeitsmarkt_bl_gender_verlauf_ui("beruf_arbeitsmarkt_bl_gender_verlauf_1")

## To be copied in the server
# mod_beruf_arbeitsmarkt_bl_gender_verlauf_server("beruf_arbeitsmarkt_bl_gender_verlauf_1")
