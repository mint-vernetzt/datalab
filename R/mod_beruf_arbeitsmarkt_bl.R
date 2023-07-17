#' beruf_arbeitsmarkt_bl UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_beruf_arbeitsmarkt_bl_ui <- function(id){
  ns <- NS(id)
  tagList(
    # p("Auswahl des Jahres:"),
    # shinyWidgets::sliderTextInput(
    #   inputId = ns("date_arbeitsmarkt_bl"),
    #   label = NULL,
    #   choices = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021),
    #   selected = 2021
    # ),
    p("Berufsfeld:"),
    shinyWidgets::pickerInput(
      inputId = ns("pick_i"),
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
      selected = "Technik (gesamt)"
    ),
    br(),
    shinyBS::bsPopover(id="dh_beruf_fach_1", title = "",
                       content = paste0("Falls die Grafiken nicht direkt angezeit werden, bitte einmal zwischen zwei Auswahloptionen des Berufsfelds (z. B. zwischen Technik und MINT) hin und her wechseln.<br> <br> Ein Verkleinern und wieder Maximieren des Ansichtsfensters hilft außerdem dabei, dass sich die Darstellungen korrekt an die Fenstergröße anpassen."),
                       trigger = "hover"),
    tags$a(paste0("Probleme bei der Darstellung"), icon("question-circle"), id = "dh_beruf_fach_1"),

    br(),
    br(),
    shinyBS::bsPopover(id="ih_beruf_fach_1", title="",
                       content = paste0("Die Karten in der ersten Einstellung zeigen beispielsweise, dass 2021 der Anteil an Auszubildenden und Beschäftigten in Technik von allen Bundesländern in Berlin am geringsten ausfällt. In Thüringen lernt dagegen rund ein Drittel der Auszubildenden im Bereich Technik. Den höchsten Anteil an Beschäftigten in Technik weist noch knapp vor Thüringen Baden-Württemberg auf (21,9 %)."),
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_beruf_fach_1")



    # p("Auswahl des Anforderungsniveaus:"),
    # shinyWidgets::pickerInput(
    #   inputId = ns("anforderungsniveau_arbeitsmarkt_bl"),
    #   choices = c("Gesamt", "Fachkraft",  "Spezialist:in"="Spezialist", "Expert:in"="Experte")
    # ) kab
  )
}

#' beruf_arbeitsmarkt_bl Server Functions
#'
#' @noRd
mod_beruf_arbeitsmarkt_bl_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_arbeitsmarkt_bl, {
      r$date_arbeitsmarkt_bl <- input$date_arbeitsmarkt_bl
    })

    observeEvent(input$pick_i, {
      r$pick_i <- input$pick_i
    })

  })
}

## To be copied in the UI
# mod_beruf_arbeitsmarkt_bl_ui("beruf_arbeitsmarkt_bl_1")

## To be copied in the server
# mod_beruf_arbeitsmarkt_bl_server("beruf_arbeitsmarkt_bl_1")
