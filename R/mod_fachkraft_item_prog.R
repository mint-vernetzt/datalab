#' fachkraft_item_prog UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_fachkraft_item_prog_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Wirkhebel:"),
    shinyWidgets::pickerInput(
      inputId = ns("fachkraft_item_prog_wirkhebel"),
      choices = fachkraft_ui_wirkhebel(),
      selected = "Gesamteffekt",
      multiple = FALSE
    ),

    p("Szenario:"),
    shinyWidgets::pickerInput(
      inputId = ns("fachkraft_item_prog_scenario"),
      choices = NULL,
      # selected = "MINT-Bildung",
      multiple = FALSE
    ),

    # uiOutput(ns("prog_scenario_picker_ui")),

    # p("Berufslevel:"),
    # shinyWidgets::pickerInput(
    #   inputId = ns("fachkraft_item_prog_berufslevel"),
    #   choices = fachkraft_ui_berufslevel(),
    #   selected = c("Gesamt"),
    #   multiple = FALSE
    # )
    # ,

    br(),

    shinyBS::bsPopover(id="ih_fachkraft_prog_1", title="",
                       content = paste0("Die erste Einstellung zeigt, dass die MINT-Fachkräfte - bleibt alles so wie jetzt - bis 2037 etwa gleich bleiben bzw. sogar leicht abnehmen werden, auf 7,8 Millionen. Schafft man eine Verbesserung in der MINT-Bildung und in der Beteiligung von Frauen, internationalen Beschäftigten und Älteren, könnten die Zahlen der MINT-Fachkräfte bis 2037 auf über 9 Millionen ansteigen."),
                       placement = "top",
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_fachkraft_prog_1")

  )
}

#' fachkraft_item_prog Server Functions
#'
#' @noRd
mod_fachkraft_item_prog_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$fachkraft_item_prog_wirkhebel, {
      r$fachkraft_item_prog_wirkhebel <- input$fachkraft_item_prog_wirkhebel
    })

    observeEvent(input$fachkraft_item_prog_scenario, {
      r$fachkraft_item_prog_scenario <- input$fachkraft_item_prog_scenario
    })

    observeEvent(input$fachkraft_item_prog_berufslevel, {
      r$fachkraft_item_prog_berufslevel <- input$fachkraft_item_prog_berufslevel
    })

    selected_prog_wirkhebel <- reactive({
      input$fachkraft_item_prog_wirkhebel
    })

    # output$prog_scenario_picker_ui <- renderUI({
    #
    #   shinyWidgets::pickerInput(
    #     inputId = ns("fachkraft_item_prog_scenario"),
    #     choices = fachkraft_ui_scenario(wirkhebel = selected_prog_wirkhebel()),
    #     selected = c("Verbesserung"),
    #     multiple = FALSE
    #   )
    # })

    observeEvent(input$fachkraft_item_prog_wirkhebel, {

      wirkhebel <- selected_prog_wirkhebel()

        shinyWidgets::updatePickerInput(
          session,
          inputId = "fachkraft_item_prog_scenario",
          choices = fachkraft_ui_scenario(wirkhebel),
          selected = ifelse(wirkhebel == "Frauen in MINT", "starke Verbesserung",
                            "Verbesserung")
        )
    })



  })
}

## To be copied in the UI
# mod_fachkraft_item_prog_ui("fachkraft_item_prog_1")

## To be copied in the server
# mod_fachkraft_item_prog_server("fachkraft_item_prog_1")

