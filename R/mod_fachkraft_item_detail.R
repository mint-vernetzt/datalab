#' fachkraft_item_detail UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_fachkraft_item_detail_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Jahr:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("map_y_fachkraft_arbeit_detail"),
      label = NULL,
      choices = fachkraft_ui_years(),
      selected = "2024"
    ),

    p("Berufslevel:"),
    shinyWidgets::pickerInput(
      inputId = ns("map_bl_fachkraft_arbeit_detail"),
      choices = c("Fachkräfte",
                  "Spezialist*innen",
                  "Expert*innen"), #fachkraft_ui_berufslevel(),
      selected = c("Fachkräfte"),
      multiple = FALSE
    ),
    #"Fachkräfte" "Spezialist*innen" "Expert*innen"
    p("Beruf:"),
    shinyWidgets::pickerInput(
      inputId = ns("map_b_fachkraft_arbeit_detail"),
      choices = NULL,
      # selected = "Basis-Szenario",

     options = list(`actions-box` = TRUE,
                    `live-search` = TRUE),
      multiple = FALSE
    ),


    br(),


    shinyBS::bsPopover(id="ih_fachkraft_epa", title="",
                       content = paste0("Die erste Einstlellung zeigt: "),
                       placement = "top",
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_fachkraft_epa")


  )
}

#' fachkraft_item_detail Server Functions
#'
#' @noRd
mod_fachkraft_item_detail_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$map_y_fachkraft_arbeit_detail, {
      r$map_y_fachkraft_arbeit_detail <- input$map_y_fachkraft_arbeit_detail
    })

    observeEvent(input$map_bl_fachkraft_arbeit_detail, {
      r$map_bl_fachkraft_arbeit_detail <- input$map_bl_fachkraft_arbeit_detail
    })

    observeEvent(input$map_b_fachkraft_arbeit_detail, {
      r$map_b_fachkraft_arbeit_detail <- input$map_b_fachkraft_arbeit_detail
    })

    observeEvent(input$map_bl_fachkraft_arbeit_detail, {

      level <- selected_level()
      zeitpunkt <- selected_zeitpunkt()
      shinyWidgets::updatePickerInput(
        session,
        inputId = "map_b_fachkraft_arbeit_detail",
        choices = fachkraft_ui_berufe(level, zeitpunkt),
        selected = "Berufe Sanitär-, Heizungs-, Klimatechnik"
      )
    })

    selected_level <- reactive({
      input$map_bl_fachkraft_arbeit_detail
    })

    selected_zeitpunkt <- reactive({
      input$map_y_fachkraft_arbeit_detail
    })


  })
}

## To be copied in the UI
# mod_fachkraft_item_detail_ui("fachkraft_item_detail_1")

## To be copied in the server
# mod_fachkraft_item_detail_server("fachkraft_item_detail_1")
