#' beruf_arbeitsmarkt_einstieg_gender UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_beruf_arbeitsmarkt_einstieg_gender_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Jahr:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_arbeitsmarkt_einstieg_gender"),
      label = NULL,
      choices = c("2013", "2014", "2015", "2016", "2017",
                  "2018","2019", "2020", "2021", "2022"),
      selected = "2022"
    ),
    br(),
    shinyBS::bsPopover(id="dh_beruf_frauen_1", title = "",
                       content = paste0("Falls die Grafik abgeschnitten dargestellt wird, bitte das gesamte Ansichtsfenster einmal verkleinern und dann wieder maximieren. Dann stellt sich das Seitenverhältnis des Desktops richtig ein."),
                       trigger = "hover"),
    tags$a(paste0("Probleme bei der Darstellung"), icon("question-circle"), id = "dh_beruf_frauen_1"),

    br(),
    br(),
    shinyBS::bsPopover(id="ih_beruf_frauen_1", title="",
                       content = paste0("Der Anteil von Frauen an MINT-Auszubildenden in Deutschland beträgt 13 % im Jahr 2022. Bei den MINT-Beschäftigten beträgt dieser Anteil 17 %. Dagegen machen Frauen in anderen, Nicht-MINT-Berufen mehr als die Hälfte aller Auszubildenden und Beschäftigten aus (56 bzw. 55 %)."),
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_beruf_frauen_1")


  )
}

#' beruf_arbeitsmarkt_einstieg_gender Server Functions
#'
#' @noRd
mod_beruf_arbeitsmarkt_einstieg_gender_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_arbeitsmarkt_einstieg_gender, {
      r$date_arbeitsmarkt_einstieg_gender <- input$date_arbeitsmarkt_einstieg_gender
    })

  })
}

## To be copied in the UI
# mod_beruf_arbeitsmarkt_einstieg_gender_ui("beruf_arbeitsmarkt_einstieg_gender_1")

## To be copied in the server
# mod_beruf_arbeitsmarkt_einstieg_gender_server("beruf_arbeitsmarkt_einstieg_gender_1")
