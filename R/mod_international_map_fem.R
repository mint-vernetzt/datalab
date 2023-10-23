#' international_map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_international_map_fem_ui <- function(id) {

  ns <- NS(id)
  tagList(
    p("Region:"),
    shinyWidgets::pickerInput(
      inputId = ns("map_l_f"),
      choices = c("EU", "OECD"),
      selected = "OECD",
      multiple = FALSE#,
      # options =  list(
      #   "max-options" = 2,
      #   "max-options-text" = "Maximal 2 Indikatoren auswählen")
    ),

    #Conditional Panel, um für Lehramt nur sinnvollere Fächer auswählen zu lassen

    conditionalPanel(condition = "input.map_l_f == 'EU'",
                     ns = ns,
                     p("Jahr:"),
                     shinyWidgets::sliderTextInput(
                       inputId = ns("map_y_eu_f"),
                       label = NULL,
                       choices = international_ui_years(region = "EU"),
                       selected = "2021"
                     ),

                     p("Fachbereich:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("map_f_eu_f"),
                       choices = international_ui_faecher(region = "EU"),
                       selected = c("Alle MINT-Fächer"),
                       multiple = FALSE
                     )),
    conditionalPanel(condition = "input.map_l_f == 'OECD' ",
                     ns = ns,
                     p("Jahr:"),
                     shinyWidgets::sliderTextInput(
                       inputId = ns("map_y_oecd_f"),
                       label = NULL,
                       choices = international_ui_years(region = "OECD"),
                       selected = "2020"
                     ),

                     p("Fachbereich:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("map_f_oecd_f"),
                       choices = international_ui_faecher(region = "OECD"),
                       selected = c("MINT"),
                       multiple = FALSE
                     ),
                     p("Anforderungsniveau:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("map_lev_oecd_f"),
                       choices = c("Bachelor oder vergleichbar (akademisch)",
                                   "Master oder vergleichbar (akademisch)",
                                   "Promotion (ISCED 8)"),
                       selected = c("Promotion (ISCED 8)"),
                       multiple = FALSE)
    ))


}

#' international_map Server Functions
#'
#' @noRd
mod_international_map_fem_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # region change updates respective sub inputs, which will otherwise
    # still be the last values.
    observeEvent(input$map_l_f, {
      r$map_l_f <- input$map_l_f
      if (input$map_l_f == "EU") {
        r$map_y_f <- input$map_y_eu_f
        r$map_f_f <- input$map_f_eu_f

      }
      if (input$map_l_f == "OECD") {
        r$map_y_f <- input$map_y_oecd_f
        r$map_f_f <- input$map_f_oecd_f
        r$map_le_f <- input$map_lev_oecd_f
      }
    })


  })
}

## To be copied in the UI
# mod_international_map_ui("international_map_1")

## To be copied in the server
# mod_international_map_server("international_map_1")
