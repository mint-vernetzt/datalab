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
      selected = "MINT-Bildung",
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

    p("Berufslevel:"),
    shinyWidgets::pickerInput(
      inputId = ns("fachkraft_item_prog_berufslevel"),
      choices = fachkraft_ui_berufslevel(),
      selected = c("Gesamt"),
      multiple = FALSE
    ),

    br(),

    # TODO extract into own module, since this is repeated on a lot of modules

    shinyBS::bsPopover(id="dh_fachkraft_prog", title = "",
                       content = paste0("TODO."),
                       placement = "top",
                       trigger = "hover"),
    tags$a(paste0("Probleme bei der Darstellung"), icon("question-circle"), id = "dh_fachkraft_prog"),
    br(),
    br(),
    shinyBS::bsPopover(id="ih_fachkraft_prog", title="",
                       content = paste0("TODO."),
                       placement = "top",
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_fachkraft_prog")

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
        shinyWidgets::updatePickerInput(
          session,
          inputId = "fachkraft_item_prog_scenario",
          choices = fachkraft_ui_scenario(wirkhebel = selected_prog_wirkhebel()),
          selected = "Verbesserung",
        )
    })



  })
}

## To be copied in the UI
# mod_fachkraft_item_prog_ui("fachkraft_item_prog_1")

## To be copied in the server
# mod_fachkraft_item_prog_server("fachkraft_item_prog_1")

