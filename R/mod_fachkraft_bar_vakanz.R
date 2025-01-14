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

  #logger::log_debug("start mod_fachkraft_bar_vakanz_ui")
  ns <- NS(id)
  tagList(
    p("Indikator:"),
    shinyWidgets::pickerInput(
      inputId = ns("map_ind_fachkraft_arbeit_bar"),
      choices = c("Abgeschlossene Vakanzzeit", "Arbeitslosen-Stellen-Relation"),
      selected = "Arbeitslosen-Stellen-Relation",
      multiple = FALSE
    ),

    p("Jahr:"),
     shinyWidgets::sliderTextInput(
       inputId = ns("map_y_vakanz_fachkraft_arbeit_bar"),
       label = NULL,
       choices = arbeit_fachkraft_ui_years(),
       selected = "2022"
     ),

     p("Region:"),
     shinyWidgets::pickerInput(
       inputId = ns("map_reg_vakanz_fachkraft_arbeit_bar"),
       choices = arbeit_fachkraft_ui_region(),
       selected = c("Deutschland"),
       multiple = FALSE
     ),

    p("Berufslevel:"),
    shinyWidgets::pickerInput(
      inputId = ns("map_bl_fachkraft_arbeit_bar"),
      choices = c("Gesamt",
                  "Fachkräfte",
                  "Spezialist*innen",
                  "Expert*innen"),
      selected = c("Gesamt"),
      multiple = FALSE
    ),

    br(),

    shinyBS::bsPopover(id="ih_fachkraft_asr_vz_1", title="",
                       content = paste0("Die erste Einstellung zeigt einen erhöhten Mangel an Arbeitskräften in MINT vs. Nicht-MINT. Deutschlandweit kommen 2022 auf eine offene Stelle in MINT nur 1,13 arbeitssuchende Personen. Im Nicht-MINT-Bereich sieht diese Relation besser aus: Hier kommen etwas mehr als 3 Arbeitssuchende auf eine offene Stelle."),
                       placement = "top",
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_fachkraft_asr_vz_1")

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
    })

    # observeEvent(input$map_ind_fachkraft_arbeit_bar, {
    #   r$map_ind_fachkraft_arbeit_bar <- input$map_ind_fachkraft_arbeit_bar
    #   if (input$map_ind_fachkraft_arbeit_bar %in% c("Abgeschlossene Vakanzzeit", "Arbeitslosen-Stellen-Relation")) {
    #     r$map_y_fachkraft_arbeit_bar <- input$map_y_vakanz_fachkraft_arbeit_bar
    #     r$map_reg_fachkraft_arbeit_bar <- input$map_reg_vakanz_fachkraft_arbeit_bar
    #   } else  {
    #     # TODO add selection here for EPA later
    #   }
    # })

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
