#' beruf_arbeitsmarkt_anforderungen_gender UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_beruf_arbeitsmarkt_anforderungen_gender_ui <- function(id){
  ns <- NS(id)
  tagList(

    p("Auswahl des Jahres:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_arbeitsmarkt_anforderungen_gender"),
      label = NULL,
      choices = c(2021, 2022),
      selected = 2022
    ),
    # p("Auswahl der Beschäftigungsform:"),
    # shinyWidgets::radioGroupButtons(
    #   inputId = ns("level_arbeitsmarkt_anforderungen_gender"),
    #   choices = c("Auszubildende", "Beschäftigte"),
    #   justified = TRUE,
    #   checkIcon = list(yes = icon("ok",
    #                               lib = "glyphicon"))
    # ),

    # Auswahlform zu Dropdown geändert - alle möglichen neuen Indikatore können einfach hier ergänzt werden

    p("Beschäftigungsform:"),
    conditionalPanel(condition = "input.date_arbeitsmarkt_anforderungen_gender == '2022'",
                     ns = ns,
                     shinyWidgets::pickerInput(
                       inputId = ns("level_arbeitsmarkt_anforderungen_gender_22"),
                       choices = c("Beschäftigte",
                                   "Auszubildende",
                                   # "Auszubildende (1. Jahr)", #auskommentiert bis für 2022 auch da
                                   "ausländische Beschäftigte",
                                   "ausländische Auszubildende"),
                       selected = "Beschäftigte",
                       multiple = FALSE
                       )),
    conditionalPanel(condition = "input.date_arbeitsmarkt_anforderungen_gender == '2021'",
                     ns = ns,
                     shinyWidgets::pickerInput(
                       inputId = ns("level_arbeitsmarkt_anforderungen_gender_21"),
                       choices = c("Beschäftigte",
                                   "Auszubildende",
                                   "Auszubildende (1. Jahr)",
                                   "ausländische Beschäftigte",
                                   "ausländische Auszubildende"),
                       selected = "Beschäftigte",
                       multiple = FALSE)),

    br(),

    shinyBS::bsPopover(id="ih_beruf_mint_4", title="",
                       content = paste0("Die erste Einstellung zeigt, dass von allen weiblichen Beschäftigten 2022 nur 9 % eine MINT-Tätigkeit ausüben. Bei den Männern arbeitet ein weitaus größerer Anteil von rund 35 % in der MINT-Branche. Dabei sind die meisten Männer im Bereich Technik tätig (29 %)."),
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_beruf_mint_4")


  )
}

#' beruf_arbeitsmarkt_anforderungen_gender Server Functions
#'
#' @noRd
mod_beruf_arbeitsmarkt_anforderungen_gender_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_arbeitsmarkt_anforderungen_gender, {
      r$date_arbeitsmarkt_anforderungen_gender <- input$date_arbeitsmarkt_anforderungen_gender
    })

    observeEvent(input$level_arbeitsmarkt_anforderungen_gender_21, {
      r$level_arbeitsmarkt_anforderungen_gender_21 <- input$level_arbeitsmarkt_anforderungen_gender_21
    })

    observeEvent(input$level_arbeitsmarkt_anforderungen_gender_22, {
      r$level_arbeitsmarkt_anforderungen_gender_22 <- input$level_arbeitsmarkt_anforderungen_gender_22
    })

  })
}

## To be copied in the UI
# mod_beruf_arbeitsmarkt_anforderungen_gender_ui("beruf_arbeitsmarkt_anforderungen_gender_1")

## To be copied in the server
# mod_beruf_arbeitsmarkt_anforderungen_gender_server("beruf_arbeitsmarkt_anforderungen_gender_1")
