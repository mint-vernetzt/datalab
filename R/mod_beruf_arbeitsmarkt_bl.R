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
    p("Auswahl des Fachs:"),
    shinyWidgets::pickerInput(
      inputId = ns("pick_i"),
      choices = c( "Bau- und Gebäudetechnik",  "Gesundheitstechnik",

                   "Informatik",    "Landtechnik", "Mathematik, Naturwissenschaften",

                   "MINT",  "Produktionstechnik","Technik (gesamt)",

                   "Verkehrs-, Sicherheits- u. Veranstaltungstechnik"
                    ),
      selected = "Technik (gesamt)"
    )



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
