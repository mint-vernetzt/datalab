#' international_map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_international_map_ui <- function(id) {

  # logger::log_debug("start mod_international_map_ui")

  ns <- NS(id)
  tagList(
    p("Region:"),
    shinyWidgets::pickerInput(
      inputId = ns("map_l_int_studium"),
      choices = c("Europa" = "EU", "OECD", "Weltweit"),
      selected = "Europa",
      multiple = FALSE#,

    ),

    #Conditional Panel

    conditionalPanel(condition = "input.map_l_int_studium == 'EU'",
                     ns = ns,
                     p("Jahr:"),
                     shinyWidgets::sliderTextInput(
                       inputId = ns("map_y_eu"),
                       label = NULL,
                       choices = international_ui_years(region = "EU"),
                       selected = "2021"
                     ),

                     p("Fachbereich:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("map_f_eu"),
                       choices = international_ui_faecher(region = "EU"),
                       selected = c("Alle MINT-Fächer"),
                       multiple = FALSE#,

                     )),
    conditionalPanel(condition = "input.map_l_int_studium == 'OECD' ",
                     ns = ns,
                     p("Jahr:"),
                     shinyWidgets::sliderTextInput(
                       inputId = ns("map_y_oecd"),
                       label = NULL,
                       choices = international_ui_years(region = "OECD"),
                       selected = "2024"
                     ),

                     p("Fachbereich:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("map_f_oecd"),
                       choices = international_ui_faecher(region = "OECD"),
                       selected = c("MINT"),
                       multiple = FALSE,

                     )),
    conditionalPanel(condition = "input.map_l_int_studium == 'Weltweit' ",
                     ns = ns,
                     p("Jahr:"),
                     shinyWidgets::sliderTextInput(
                       inputId = ns("map_y_ww"),
                       label = NULL,
                       choices = international_ui_years(region = "Weltweit"),
                       selected = "2020"
                     )),

    br(),
    darstellung(id="dh_international_map2"),
    br(),
    br(),
    shinyBS::bsPopover(id="ih_international_map1", title="",
                       content = paste0("Die Karte zeigt, dass Deutschland im Vergleich zu anderen Ländern Europas einen relativ hohen Anteil an MINT-Studierenden aufweist."),
                       placement = "top",
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_international_map1")
  )

}

#' international_map Server Functions
#'
#' @noRd
mod_international_map_server <- function(id, r){

  # logger::log_debug("start mod_international_map_server")

  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # region change updates respective sub inputs, which will otherwise
    # still be the last values.
    observeEvent(input$map_l_int_studium, {
      r$map_l_int_studium <- input$map_l_int_studium
      if (input$map_l_int_studium == "EU") {
        r$map_y_int_studium <- input$map_y_eu
        r$map_f_int_studium <- input$map_f_eu
      }
      if (input$map_l_int_studium == "OECD") {
        r$map_y_int_studium <- input$map_y_oecd
        r$map_f_int_studium <- input$map_f_oecd
      }
      if (input$map_l_int_studium == "Weltweit"){
        r$map_y_int_studium <- input$map_y_ww
      }
    }, ignoreInit = TRUE)

    observeEvent(input$map_y_oecd, {
      r$map_y_int_studium <- input$map_y_oecd
    }, ignoreInit = TRUE)

    observeEvent(input$map_f_oecd, {
      r$map_f_int_studium_oec_d <- input$map_f_oecd
    }, ignoreInit = TRUE)

    observeEvent(input$map_y_ww, {
      r$map_y_int_studium <- input$map_y_ww
    }, ignoreInit = TRUE)

    # eu check should be after oecd check, since it is the default and will
    # otherwise be overwritten on initial load up
    observeEvent(input$map_y_eu, {
      r$map_y_int_studium <- input$map_y_eu
    }, ignoreInit = TRUE)

    observeEvent(input$map_f_eu, {
      r$map_f_int_studium_e_u <- input$map_f_eu
    }, ignoreInit = TRUE)

  })
}

## To be copied in the UI
# mod_international_map_ui("international_map_1")

## To be copied in the server
# mod_international_map_server("international_map_1")
