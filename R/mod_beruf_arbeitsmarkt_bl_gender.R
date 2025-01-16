#' beruf_arbeitsmarkt_bl_gender UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_beruf_arbeitsmarkt_bl_gender_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Auswahl des Jahres:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_arbeitsmarkt_bl_gender"),
      label = NULL,
      choices = c(2021, 2022),
      selected = 2022
    ),
    p("Beschäftigungsform:"),
    conditionalPanel(condition = "input.date_arbeitsmarkt_bl_gender == '2022'",
                     ns = ns,
                     shinyWidgets::pickerInput(
                       inputId = ns("level_arbeitsmarkt_bl_gender_22"),
                       choices = c("Beschäftigte",
                                   "Auszubildende",
                                   "Auszubildende mit neuem Lehrvertrag" =  "Auszubildende (1. Jahr)", #auskommentiert bis für 2022 auch da
                                   "ausländische Beschäftigte",
                                   "ausländische Auszubildende"),
                       selected = "Beschäftigte",
                       multiple = FALSE
                     )),
    conditionalPanel(condition = "input.date_arbeitsmarkt_bl_gender == '2021'",
                     ns = ns,
                     shinyWidgets::pickerInput(
                       inputId = ns("level_arbeitsmarkt_bl_gender_21"),
                       choices = c("Beschäftigte",
                                   "Auszubildende",
                                   "Auszubildende mit neuem Lehrvertrag" = "Auszubildende (1. Jahr)",
                                   "ausländische Beschäftigte",
                                   "ausländische Auszubildende"),
                       selected = "Beschäftigte",
                       multiple = FALSE)),

    p("Berufsfeld:"),
    shinyWidgets::pickerInput(
      inputId = ns("fach_arbeitsmarkt_bl_gender"),
      choices = c("MINT",
                  "Mathematik/ Naturwissenschaften" = "Mathematik, Naturwissenschaften",
                  "Informatik",
                  "Technik (gesamt)",
                  "Alle Berufsfelder außer MINT" = "Andere Berufsgruppen"),
      multiple = FALSE,
      selected = "MINT"),
    br(),
    darstellung(id="dh_beruf_mint_6"),
    br(),
    br(),
    shinyBS::bsPopover(id="ih_beruf_mint_5", title="",
                       content = paste0("Vergleicht man die Legenden der Karten sieht man, dass der Anteil von Frauen, die in MINT-Berufen arbeiten, weitaus geringer ist als der von Männern. In Rheinland-Pfalz arbeiten beispielsweise 2022 nur 7 % aller berufstätigen Frauen in MINT, dagegen aber 35 % der Männer."),
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_beruf_mint_5")

  )
}

#' beruf_arbeitsmarkt_bl_gender Server Functions
#'
#' @noRd
mod_beruf_arbeitsmarkt_bl_gender_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_arbeitsmarkt_bl_gender, {
      r$date_arbeitsmarkt_bl_gender <- input$date_arbeitsmarkt_bl_gender
    })

    # observeEvent(input$anforderungsniveau_arbeitsmarkt_bl_gender, {
    #   r$anforderungsniveau_arbeitsmarkt_bl_gender <- input$anforderungsniveau_arbeitsmarkt_bl_gender
    # })

    observeEvent(input$level_arbeitsmarkt_bl_gender_21, {
      r$level_arbeitsmarkt_bl_gender_21 <- input$level_arbeitsmarkt_bl_gender_21
    })

    observeEvent(input$level_arbeitsmarkt_bl_gender_22, {
      r$level_arbeitsmarkt_bl_gender_22 <- input$level_arbeitsmarkt_bl_gender_22
    })

    observeEvent(input$fach_arbeitsmarkt_bl_gender, {
      r$fach_arbeitsmarkt_bl_gender <- input$fach_arbeitsmarkt_bl_gender
    })

  })
}

## To be copied in the UI
# mod_beruf_arbeitsmarkt_bl_gender_ui("beruf_arbeitsmarkt_bl_gender_1")

## To be copied in the server
# mod_beruf_arbeitsmarkt_bl_gender_server("beruf_arbeitsmarkt_bl_gender_1")
