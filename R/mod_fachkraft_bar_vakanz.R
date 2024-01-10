#' fachkraft_bar_vakanz UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_fachkraft_bar_vakanz_ui <- function(id){

  logger::log_debug("start mod_fachkraft_bar_vakanz_ui")
  ns <- NS(id)
  tagList(
    p("Indikator:"),
    shinyWidgets::pickerInput(
      inputId = ns("map_ind_fachkraft_arbeit_bar"),
      choices = c("Abgeschlossene Vakanzzeit", "Arbeitslosen-Stellen-Relation"),
      selected = "Arbeitslosen-Stellen-Relation",
      multiple = FALSE
    ),

    #Conditional Panel, da verschiede Datensätze zu Grunde liegen
    conditionalPanel(condition = "input.map_ind %in% c('Abgeschlossene Vakanzzeit', 'Arbeitslosen-Stellen-Relation')",
                     ns = ns,
                     p("Jahr:"),
                     shinyWidgets::sliderTextInput(
                       inputId = ns("map_y_vakanz_fachkraft_arbeit_bar"),
                       label = NULL,
                       choices = arbeit_fachkraft_ui_years(),
                       selected = "2021"
                     ),

                     p("Region:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("map_reg_vakanz_fachkraft_arbeit_bar"),
                       choices = arbeit_fachkraft_ui_region(),
                       selected = c("Deutschland"),
                       multiple = FALSE
                     )),

    p("Berufslevel:"),
    shinyWidgets::pickerInput(
      inputId = ns("map_bl_fachkraft_arbeit_bar"),
      choices = c("Gesamt",
                  "Fachkräfte",
                  "Spezialist*innen",
                  "Expert*innen"),
      selected = c("Fachkräfte"),
      multiple = FALSE
    ),
  )
}

#' fachkraft_bar_vakanz Server Functions
#'
#' @noRd
mod_fachkraft_bar_vakanz_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    observeEvent(input$map_ind_fachkraft_arbeit_bar, {
      r$map_ind_fachkraft_arbeit_bar <- input$map_ind_fachkraft_arbeit_bar
      if (input$map_ind_fachkraft_arbeit_bar %in% c("Abgeschlossene Vakanzzeit", "Arbeitslosen-Stellen-Relation")) {
        r$map_y_fachkraft_arbeit_bar <- input$map_y_vakanz_fachkraft_arbeit_bar
        r$map_reg_fachkraft_arbeit_bar <- input$map_reg_vakanz_fachkraft_arbeit_bar
      } else  {
        # TODO add selection here for EPA later
      }


    })

    observeEvent(input$map_y_vakanz_fachkraft_arbeit_bar, {
      r$map_y_fachkraft_arbeit_bar <- input$map_y_vakanz_fachkraft_arbeit_bar
    })

    observeEvent(input$map_reg_vakanz_fachkraft_arbeit_bar, {
      r$map_reg_fachkraft_arbeit_bar <- input$map_reg_vakanz_fachkraft_arbeit_bar
    })

    observeEvent(input$map_bl_fachkraft_arbeit_bar, {
      r$map_bl_fachkraft_arbeit_bar <- input$map_bl_fachkraft_arbeit_bar
    })



  })
}

## To be copied in the UI
# mod_fachkraft_bar_vakanz_ui("fachkraft_bar_vakanz_1")

## To be copied in the server
# mod_fachkraft_bar_vakanz_server("fachkraft_bar_vakanz_1")
