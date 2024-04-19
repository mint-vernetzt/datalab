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
      selected = "2022"
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
    # p("Beruf:"),
    # conditionalPanel(condition = "input.map_bl_fachkraft_arbeit_detail == 'Fachkräfte'",
    #                  ns = ns,
    #
    #                  shinyWidgets::pickerInput(
    #                    inputId = ns("map_b_fachkraft_arbeit_detail_fach"),
    #                    choices = fachkraft_ui_berufe(level = "Fachkräfte"),
    #                    selected = "Gesamt",
    #                    options = list(`actions-box` = TRUE,
    #                                   `live-search` = TRUE),
    #                    multiple = FALSE
    #                  )),
    # conditionalPanel(condition = "input.map_bl_fachkraft_arbeit_detail == 'Spezialist*innen'",
    #                  ns = ns,
    #
    #                  shinyWidgets::pickerInput(
    #                    inputId = ns("map_b_fachkraft_arbeit_detail_spez"),
    #                    choices = fachkraft_ui_berufe(level = "Spezialist*innen"),
    #                    selected = "Gesamt",
    #                    options = list(`actions-box` = TRUE,
    #                                   `live-search` = TRUE),
    #                    multiple = FALSE
    #                  )),
    # conditionalPanel(condition = "input.map_bl_fachkraft_arbeit_detail == 'Expert*innen'",
    #                  ns = ns,
    #
    #                  shinyWidgets::pickerInput(
    #                    inputId = ns("map_b_fachkraft_arbeit_detail_expert"),
    #                    choices = fachkraft_ui_berufe(level = "Expert*innen"),
    #                    selected = "Gesamt",
    #                    options = list(`actions-box` = TRUE,
    #                                   `live-search` = TRUE),
    #                    multiple = FALSE
    #                  )),


    br(),


    shinyBS::bsPopover(id="ih_fachkraft_epa", title="",
                       content = paste0("Die linke Karte der ersten Einstellung zeigt, dass die beiden Bundesländer mit dem höchsten Anteil von Informatik-Studierenden Bayern und Schleswig-Holstein mit jeweils 10 % sind."),
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
        # choices = ifelse(level == "Fachkräfte", fachkraft_ui_berufe(level = "Fachkräfte"),
        #                  ifelse(level == "Spezialist*innen", fachkraft_ui_berufe(level = "Spezialist*innen"),
        #                         fachkraft_ui_berufe(level = "Expert*innen"))),
        choices = fachkraft_ui_berufe(level, zeitpunkt),
        selected = "Gesamt"
      )
    })

    selected_level <- reactive({
      input$map_bl_fachkraft_arbeit_detail
    })

    selected_zeitpunkt <- reactive({
      input$map_y_fachkraft_arbeit_detail
    })


# observeEvent(input$map_bl_fachkraft_arbeit_detail, {
#   r$map_bl_fachkraft_arbeit_detail <- input$map_bl_fachkraft_arbeit_detail
#   if (input$map_bl_fachkraft_arbeit_detail == "Fachkräfte") {
#     r$map_b_fachkraft_arbeit_detail <- input$map_b_fachkraft_arbeit_detail_fach
#   }
#   if (input$map_bl_fachkraft_arbeit_detail == "Spezialist*innen") {
#     r$map_b_fachkraft_arbeit_detail <- input$map_b_fachkraft_arbeit_detail_spez
#   }
#   if (input$map_bl_fachkraft_arbeit_detail == "Expert*innen") {
#     r$map_b_fachkraft_arbeit_detail <- input$map_b_fachkraft_arbeit_detail_expert
#   }
#
# })
#
#     # Berufswahl
#     observeEvent(input$map_b_fachkraft_arbeit_detail_fach, {
#       r$map_b_fachkraft_arbeit_detail_fach <- input$map_b_fachkraft_arbeit_detail_fach
#     })
#     observeEvent(input$map_b_fachkraft_arbeit_detail_spez, {
#       r$map_b_fachkraft_arbeit_detail_spez <- input$map_b_fachkraft_arbeit_detail_spez
#     })
#     observeEvent(input$map_b_fachkraft_arbeit_detail_expert, {
#       r$map_b_fachkraft_arbeit_detail_expert <- input$map_b_fachkraft_arbeit_detail_expert
#     })
  })
}

## To be copied in the UI
# mod_fachkraft_item_detail_ui("fachkraft_item_detail_1")

## To be copied in the server
# mod_fachkraft_item_detail_server("fachkraft_item_detail_1")
