#' fachkraft_item_prog_detail UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_fachkraft_item_prog_detail_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Wirkhebel:"),
    shinyWidgets::pickerInput(
      inputId = ns("fachkraft_item_prog_detail_wirkhebel"),
      # choices = c("Basis-Szenario", fachkraft_ui_wirkhebel()),
      # selected = "Basis-Szenario",
      choices = fachkraft_ui_wirkhebel(),
      selected = "MINT-Bildung",
      multiple = FALSE
    ),

    p("Szenario:"),
    shinyWidgets::pickerInput(
      inputId = ns("fachkraft_item_prog_detail_scenario"),
      choices = NULL,
      # selected = "Basis-Szenario",
      multiple = FALSE
    ),

    # uiOutput(ns("prog_detail_scenario_picker_ui")),

    p("Beschäftigte betrachtet nach:"),
    shinyWidgets::pickerInput(
      inputId = ns("fachkraft_item_prog_detail_gruppe"),
      choices = fachkraft_ui_prognose_gruppen(),
      selected = c("Berufslevel"),
      multiple = FALSE
    ),

    br(),

    # TODO extract into own module, since this is repeated on a lot of modules

    shinyBS::bsPopover(id="dh_fachkraft_prog_detail", title = "",
                       content = paste0("TODO."),
                       placement = "top",
                       trigger = "hover"),
    tags$a(paste0("Probleme bei der Darstellung"), icon("question-circle"), id = "dh_fachkraft_prog_detail"),
    br(),
    br(),
    shinyBS::bsPopover(id="ih_fachkraft_prog_detail", title="",
                       content = paste0("TODO."),
                       placement = "top",
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_fachkraft_prog_detail")

  )
}

#' fachkraft_item_prog_detail Server Functions
#'
#' @noRd
mod_fachkraft_item_prog_detail_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$fachkraft_item_prog_detail_wirkhebel, {
      r$fachkraft_item_prog_detail_wirkhebel <- input$fachkraft_item_prog_detail_wirkhebel
    })

    observeEvent(input$fachkraft_item_prog_detail_scenario, {
      r$fachkraft_item_prog_detail_scenario <- input$fachkraft_item_prog_detail_scenario
    })

    observeEvent(input$fachkraft_item_prog_detail_gruppe, {
      r$fachkraft_item_prog_detail_gruppe <- input$fachkraft_item_prog_detail_gruppe
    })

    observeEvent(input$fachkraft_item_prog_detail_wirkhebel, {
      if (input$fachkraft_item_prog_detail_wirkhebel == "Basis-Szenario") {
        shinyWidgets::updatePickerInput(
          session,
          "fachkraft_item_prog_detail_scenario",
          choices = fachkraft_ui_scenario(wirkhebel = selected_prog_detail_wirkhebel()),
          selected = "Status-quo",
        )
      } else {
        shinyWidgets::updatePickerInput(
          session,
          "fachkraft_item_prog_detail_scenario",
          choices = fachkraft_ui_scenario(wirkhebel = selected_prog_detail_wirkhebel()),
          selected = "Verbesserung",
        )
      }
    })


    selected_prog_detail_wirkhebel <- reactive({
      input$fachkraft_item_prog_detail_wirkhebel
    })

  })
}

## To be copied in the UI
# mod_fachkraft_item_prog_detail_ui("fachkraft_item_prog_detail_1")

## To be copied in the server
# mod_fachkraft_item_prog_detail_server("fachkraft_item_prog_detail_1")

