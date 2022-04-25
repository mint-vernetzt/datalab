#' studium_compare UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_studium_compare_ui <- function(id){
  ns <- NS(id)
  tagList(
    br(),br(),br(),
    fluidRow(
    mod_studium_compare_choice_ui("mod_studium_compare_choice_ui_1"),
    ),
    fluidRow(
      column(6, actionButton(ns("add_graph"), "Add Graph"))
      #column(6, actionButton("reset_graph", "Reset Graphs"))
    ),
    br(),br(),br(),br(),
    fluidRow(
      shiny::column(width = 12, plotOutput(ns("plot_compare"))
      )
    )
  )
}

#' studium_compare Server Functions
#'
#' @noRd
mod_studium_compare_server <- function(id, r, r_abschluss,
                                       r_studienzahl, r_habil){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    plot <- eventReactive(
      input$add_graph, {

        if(isTruthy(r_abschluss$indikator_compare_1) &
           isTruthy(r_studienzahl$indikator_compare_2)){

          validate(need(r_abschluss$durchgefallen_compare != "",
                        'Welchen Status soll der Abschluss haben ?'))

          validate(need(r_abschluss$ing_natwi_compare_3 != "",
                        'Abschlusszahlen: Wähle ein Fach oder mehrere Fächer'))

          validate(need(r_studienzahl$ing_natwi_compare_2 != "",
                        'Studierendenzahlen: Wähle ein Fach oder mehrere Fächer'))

          comparer_plot(data,r,r_abschluss, r_studienzahl, r_habil)

        }else if(isTruthy(r_abschluss$indikator_compare_1) &
                !isTruthy(r_studienzahl$indikator_compare_2)){

          validate(need(r_abschluss$durchgefallen_compare != "",
                        'Welchen Status soll der Abschluss haben ?'))

          validate(need(r_abschluss$ing_natwi_compare_3 != "",
                        'Abschlusszahlen: Wähle ein Fach oder mehrere Fächer'))

          comparer_plot(data,r,r_abschluss, r_studienzahl, r_habil)

        }else if(!isTruthy(r_abschluss$indikator_compare_1) &
                  isTruthy(r_studienzahl$indikator_compare_2)){

          validate(need(r_studienzahl$ing_natwi_compare_2 != "",
                        'Studierendenzahlen: Wähle ein Fach oder mehrere Fächer'))

          comparer_plot(data,r,r_abschluss, r_studienzahl, r_habil)

        } else{

          comparer_plot(data,r,r_abschluss, r_studienzahl, r_habil)
      }
    }
  )

    output$plot_compare <- renderPlot({plot()})

  })
}

## To be copied in the UI
# mod_studium_compare_ui("studium_compare_1")

## To be copied in the server
# mod_studium_compare_server("studium_compare_1")
