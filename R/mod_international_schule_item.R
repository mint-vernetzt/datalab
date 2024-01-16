#' international_schule_item UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_international_schule_item_ui <- function(id){

  logger::log_debug("start mod_international_schule_item_ui")

  ns <- NS(id)
  tagList(

    p("Fach:"),
    shinyWidgets::pickerInput(
      inputId = ns("item_f_timss_int_schule"),
      choices = c("Mathematik", "Naturwissenschaften"),
      selected = c("Mathematik"),
      multiple = FALSE
    ),

    p("Jahr:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("item_y_timss_int_schule"),
      label = NULL,
      choices = international_ui_years(region = "TIMSS"),
      selected = "2019"
    )
  )
}

#' international_schule_item Server Functions
#'
#' @noRd
mod_international_schule_item_server <- function(id, r){

  logger::log_debug("start mod_international_schule_item_server")

  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$item_f_timss_int_schule, {
      r$item_f_int_schule <- input$item_f_timss_int_schule
    })

    observeEvent(input$item_y_timss_int_schule, {
      r$item_y_int_schule <- input$item_y_timss_int_schule
    })

  })
}

## To be copied in the UI
# mod_international_schule_item_ui("international_schule_item_1")

## To be copied in the server
# mod_international_schule_item_server("international_schule_item_1")
