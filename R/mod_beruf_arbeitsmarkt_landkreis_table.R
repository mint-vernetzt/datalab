#' beruf_arbeitsmarkt_landkreis_table_lk UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_beruf_arbeitsmarkt_landkreis_table_lk_ui <- function(id){
  ns <- NS(id)

  tagList(
    p("Bundesland:"),
    shinyWidgets::pickerInput(
      inputId = ns("states_beruf_arbeitsmarkt_landkreis_table"),
      choices = c("Baden-Württemberg",
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
      ),
      multiple = FALSE,
      selected = c("Hessen")
    ),
    p("Landkreis:"),
    uiOutput(ns("controls")),

  )
}

#' beruf_arbeitsmarkt_landkreis_table_lk Server Functions
#'
#' @noRd
mod_beruf_arbeitsmarkt_landkreis_table_lk_server <- function(id, r, data){
  moduleServer( id,
                function(input, output, session) {
                  ns <- session$ns

                  output$controls <- renderUI({

                    region <- data %>%
                      dplyr::filter(bundesland == input$states_beruf_arbeitsmarkt_landkreis_table) %>%
                      dplyr::pull(landkreis) %>%
                      unique()

                    shinyWidgets::pickerInput(
                      inputId = ns("region_beruf_arbeitsmarkt_landkreis_table"),
                      choices = region,
                      multiple = FALSE,
                      selected = c("Hessen")
                    )
                  })

                  observeEvent(input$region_beruf_arbeitsmarkt_landkreis_table, {
                    ns <- NS(id)

                    r[[ns("states_beruf_arbeitsmarkt_landkreis_table")]] <- input$states_beruf_arbeitsmarkt_landkreis_table

                    r[[ns("region_beruf_arbeitsmarkt_landkreis_table")]] <- input$region_beruf_arbeitsmarkt_landkreis_table
                  })


                }
  )
}

## To be copied in the UI
# mod_beruf_arbeitsmarkt_landkreis_table_lk_ui("beruf_arbeitsmarkt_landkreis_table_lk_1")

## To be copied in the server
# mod_beruf_arbeitsmarkt_landkreis_table_lk_server("beruf_arbeitsmarkt_landkreis_table_lk_1")
