#' beruf_arbeitsmarkt_bl_verlauf UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_beruf_arbeitsmarkt_bl_verlauf_ui <- function(id){
  ns <- NS(id)
  tagList(

    p("Jahre:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_beruf_arbeitsmarkt_bl_verlauf"),
      label = NULL,
      choices = c("2013", "2014", "2015", "2016", "2017",
                  "2018","2019", "2020", "2021", "2022"),
      selected = c("2017", "2022")
    ),
    p("Beschäftigungsform"),
    shinyWidgets::pickerInput(
      inputId = ns("niveau"),
      choices = c(
                   "Auszubildende",

                   "Beschäftigte"

                   ),
      # justified = TRUE,
      # checkIcon = list(yes = icon("ok",
      #                             lib = "glyphicon")),
      selected= "Beschäftigte",

    ),
    # p("Auswahl des Fachs:"),
    # shinyWidgets::pickerInput(
    #   inputId = ns("pick_i"),
    #   choices = c( "Bau- und Gebäudetechnik",  "Gesundheitstechnik",
    #
    #                "Informatik",    "Landtechnik", "Mathematik, Naturwissenschaften",
    #
    #                "MINT",  "Produktionstechnik","Technik (gesamt)",
    #
    #                "Verkehrs-, Sicherheits- u. Veranstaltungstechnik"
    #   ),
    #   selected = "Informatik"
    # ),
    # p("Auswahl des Anforderungsniveaus:"),
    # shinyWidgets::pickerInput(
    #   inputId = ns("anforderungsniveau_beruf_arbeitsmarkt_bl_verlauf"),
    #   choices = c("Gesamt", "Fachkraft",  "Spezialist:in"="Spezialist", "Expert:in"="Experte"), kab
    #   selected = "Gesamt"
    # ),
    p("Regionen:"),
    shinyWidgets::pickerInput(
      inputId = ns("states_beruf_arbeitsmarkt_bl_verlauf"),
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
      selected = c("Ostdeutschland (inkl. Berlin)", "Nordrhein-Westfalen")

    ),
    p("Betrachtung:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("abs_zahlen_4"),
      choices = c("In Prozent", "Anzahl"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
    ),

    br(),
    shinyBS::bsPopover(id="ih_beruf_mint_7", title="",
                       content = paste0("Die erste Darstellung zeigt z. B., dass sich der Anteil von Beschäftigten in MINT an allen Beschäftigten deutschlandweit in den ostdeutschen Bundesländern und Nordrhein-Westfalen ählich entwickelt. Der Anteil bleibt relativ konstant und nimmt von 2020 auf 2021 um ca. einen Prozentpunkt ab."),
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_beruf_mint_7")
  )

}

#' beruf_arbeitsmarkt_bl_verlauf Server Functions
#'
#' @noRd
mod_beruf_arbeitsmarkt_bl_verlauf_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$pick_i, {
      r$pick_i <- input$pick_i
    })

    observeEvent(input$niveau, {
      r$niveau <- input$niveau
    })

    observeEvent(input$abs_zahlen_4, {
      r$abs_zahlen_4 <- input$abs_zahlen_4
    })

    # observeEvent(input$anforderungsniveau_beruf_arbeitsmarkt_bl_verlauf, {
    #   r$anforderungsniveau_beruf_arbeitsmarkt_bl_verlauf <- input$anforderungsniveau_beruf_arbeitsmarkt_bl_verlauf
    # }) kab

    observeEvent(input$states_beruf_arbeitsmarkt_bl_verlauf, {
      r$states_beruf_arbeitsmarkt_bl_verlauf <- input$states_beruf_arbeitsmarkt_bl_verlauf
    })

    observeEvent(input$date_beruf_arbeitsmarkt_bl_verlauf, {
      r$date_beruf_arbeitsmarkt_bl_verlauf <- input$date_beruf_arbeitsmarkt_bl_verlauf
    })



  })
}

## To be copied in the UI
# mod_beruf_arbeitsmarkt_bl_verlauf_ui("beruf_arbeitsmarkt_bl_verlauf_1")

## To be copied in the server
# mod_beruf_arbeitsmarkt_bl_verlauf_server("beruf_arbeitsmarkt_bl_verlauf_1")
