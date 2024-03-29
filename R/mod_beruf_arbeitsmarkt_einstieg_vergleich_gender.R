#' beruf_arbeitsmarkt_einstieg_vergleich_gender UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_beruf_arbeitsmarkt_einstieg_vergleich_gender_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Auswahl des Jahres:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_arbeitsmarkt_einstieg_vergleich_gender"),
      label = NULL,
      choices = c(2021, 2022),
      selected = 2022
    ),

    p("Berufsfeld:"),
    shinyWidgets::pickerInput(
      inputId = ns("fach_arbeitsmarkt_einstieg_vergleich_gender"),
      choices = c("MINT",
                  "Mathematik/ Naturwissenschaften" = "Mathematik, Naturwissenschaften",
                   "Informatik",
                   "Technik"="Technik (gesamt)"
                    ),
        multiple = FALSE,
        selected = "MINT")


    ,

    p("Region:"),
    shinyWidgets::pickerInput(
      inputId = ns("BULA_arbeitsmarkt_einstieg_vergleich_gender"),
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
                  "Ostdeutschland (einschl. Berlin)"
                  ),
      multiple = FALSE,
      selected = "Niedersachsen"),
    br(),
    shinyBS::bsPopover(id="ih_beruf_frauen_3", title="",
                       content = paste0("In der ersten Einstellung sieht man beispielsweise im oberen Teil der Grafik, dass in Niedersachsen 2022 Frauen nur 14 % der Auszubildenden in MINT ausmachen. Dagegen sind Frauen in allen anderen Ausbildungen zusammen betrachtet in der Mehrheit."),
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_beruf_frauen_3")


  )
}

#' beruf_arbeitsmarkt_einstieg_vergleich_gender Server Functions
#'
#' @noRd
mod_beruf_arbeitsmarkt_einstieg_vergleich_gender_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_arbeitsmarkt_einstieg_vergleich_gender, {
      r$date_arbeitsmarkt_einstieg_vergleich_gender <- input$date_arbeitsmarkt_einstieg_vergleich_gender
    })

    observeEvent(input$fach_arbeitsmarkt_einstieg_vergleich_gender, {
      r$fach_arbeitsmarkt_einstieg_vergleich_gender <- input$fach_arbeitsmarkt_einstieg_vergleich_gender
    })

    observeEvent(input$BULA_arbeitsmarkt_einstieg_vergleich_gender, {
      r$BULA_arbeitsmarkt_einstieg_vergleich_gender <- input$BULA_arbeitsmarkt_einstieg_vergleich_gender
    })

  })
}

## To be copied in the UI
# mod_beruf_arbeitsmarkt_einstieg_vergleich_gender_ui("beruf_arbeitsmarkt_einstieg_vergleich_gender_1")

## To be copied in the server
# mod_beruf_arbeitsmarkt_einstieg_vergleich_gender_server("beruf_arbeitsmarkt_einstieg_vergleich_gender_1")
