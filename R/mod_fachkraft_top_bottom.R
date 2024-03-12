#' fachkraft_top_bottom UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_fachkraft_top_bottom_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Indikator:"),
    shinyWidgets::pickerInput(
      inputId = ns("fachkraft_arbeit_top_bottom_ind"),
      choices = c("Abgeschlossene Vakanzzeit", "Arbeitslosen-Stellen-Relation", "Engpassrisiko"),
      selected = "Arbeitslosen-Stellen-Relation",
      multiple = FALSE
    ),



    #Conditional Panel, da verschiede Datensätze zu Grunde liegen
    # conditionalPanel(condition = "input.ind_fachkraft_arbeit_top_bottom %in% c('Abgeschlossene Vakanzzeit', 'Arbeitslosen-Stellen-Relation')",
    #                  ns = ns,
    #                  p("Jahr:"),
    #                  shinyWidgets::sliderTextInput(
    #                    inputId = ns("fachkraft_arbeit_top_bottom_year"),
    #                    label = NULL,
    #                    choices = arbeit_fachkraft_ui_years(),
    #                    selected = "2022"
    #                  ),
    #
    #                  # p("Region:"),
    #                  # shinyWidgets::pickerInput(
    #                  #   inputId = ns("fachkraft_arbeit_top_bottom_region"),
    #                  #   choices = arbeit_fachkraft_ui_region(),
    #                  #   selected = c("Deutschland"),
    #                  #   multiple = FALSE
    #                  # )
    #                  ),

    p("Jahr:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("fachkraft_arbeit_top_bottom_year"),
      label = NULL,
      choices = arbeit_fachkraft_ui_years(),
      selected = 2022
    ),
    p("Region"),
    shinyWidgets::pickerInput(
      inputId = ns("fachkraft_arbeit_top_bottom_region"),
      choices = NULL,
      # selected = "MINT-Bildung",
      multiple = FALSE
    ),

    p("Berufslevel:"),
    shinyWidgets::pickerInput(
      inputId = ns("fachkraft_arbeit_top_bottom_level"),
      choices = c("Gesamt",
                  "Fachkräfte",
                  "Spezialist*innen",
                  "Expert*innen"),
      selected = c("Fachkräfte"),
      multiple = FALSE
    ),
    p("Durchschnitt anzeigen:"),
    shinyWidgets::materialSwitch(
      inputId = ns("fachkraft_arbeit_top_bottom_average"),
    )
  )
}

#' fachkraft_top_bottom Server Functions
#'
#' @noRd
mod_fachkraft_top_bottom_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$fachkraft_arbeit_top_bottom_ind, {
      r$fachkraft_arbeit_top_bottom_ind <- input$fachkraft_arbeit_top_bottom_ind
    })

    observeEvent(input$fachkraft_arbeit_top_bottom_year, {
      r$fachkraft_arbeit_top_bottom_year <- input$fachkraft_arbeit_top_bottom_year
    })

    observeEvent(input$fachkraft_arbeit_top_bottom_region, {
      r$fachkraft_arbeit_top_bottom_region <- input$fachkraft_arbeit_top_bottom_region
    })

    observeEvent(input$fachkraft_arbeit_top_bottom_level, {
      r$fachkraft_arbeit_top_bottom_level <- input$fachkraft_arbeit_top_bottom_level
    })

    observeEvent(input$fachkraft_arbeit_top_bottom_average, {
      r$fachkraft_arbeit_top_bottom_average <- input$fachkraft_arbeit_top_bottom_average
    })


    selected_fachkraft_arbeit_top_bottom_ind <- reactive({
      input$fachkraft_arbeit_top_bottom_ind
    })


    observeEvent(input$fachkraft_arbeit_top_bottom_ind, {

      shinyWidgets::updatePickerInput(
        session,
        inputId = "fachkraft_arbeit_top_bottom_region",
        choices = fachkraft_arbeit_top_bottom_region_selection(indikator = selected_fachkraft_arbeit_top_bottom_ind()),
      )

    })

    # observeEvent(input$fachkraft_arbeit_top_bottom_ind, {
    #   shiny::updateSliderInput(
    #     session,
    #     inputId = "fachkraft_arbeit_top_bottom_year",
    #     min = min(fachkraft_arbeit_top_bottom_year_selection(indikator = selected_fachkraft_arbeit_top_bottom_ind())),
    #     max = max(fachkraft_arbeit_top_bottom_year_selection(indikator = selected_fachkraft_arbeit_top_bottom_ind()))#,
    #     # selected = "2022",
    #   )
    #   print(min(fachkraft_arbeit_top_bottom_year_selection(indikator = selected_fachkraft_arbeit_top_bottom_ind())))
    #
    # })

    observe({
      # Update the choices of sliderTextInput based on the selected indicator
      if (input$fachkraft_arbeit_top_bottom_ind == "Engpassrisiko") {
        shinyWidgets::updateSliderTextInput(
          session,
          inputId = "fachkraft_arbeit_top_bottom_year",
          choices = 2019:2022,
          selected = 2022
        )
      } else {
        shinyWidgets::updateSliderTextInput(
          session,
          inputId = "fachkraft_arbeit_top_bottom_year",
          choices = 2013:2022,
          selected = 2022
        )
      }
    })


  })
}

## To be copied in the UI
# mod_fachkraft_top_bottom_ui("fachkraft_top_bottom_1")

## To be copied in the server
# mod_fachkraft_top_bottom_server("fachkraft_top_bottom_1")

