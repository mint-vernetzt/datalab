#' international_top10_mint_gender UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_international_top10_mint_intl_ui <- function(id){

  ns <- NS(id)
  tagList(

                     p("Jahr:"),
                     shinyWidgets::sliderTextInput(
                       inputId = ns("map_y_ti"),
                       label = NULL,
                       choices = international_ui_years(region = "EU"),
                       selected = "2021"
                     ),



    p("Durchschnitt anzeigen:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("show_avg_top10_mint_int_line"),
      choices = c("Ja", "Nein"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
    ),


    br(),

    # # TODO extract into own module, since this is repeated on a lot of modules
    #
    # shinyBS::bsPopover(id="dh_international_map", title = "",
    #                    content = paste0("Falls die Grafiken abgeschnitten dargestellt werden, bitte das gesamte Ansichtsfenster einmal verkleinern und dann wieder maximieren. Dann stellt sich das Seitenverhältnis des Desktops richtig ein."),
    #                    placement = "top",
    #                    trigger = "hover"),
    # tags$a(paste0("Probleme bei der Darstellung"), icon("question-circle"), id = "dh_international_map"),
    # br(),
    # br(),
    shinyBS::bsPopover(id="ih_international_map5", title="",
                       content = paste0("Diese Grafik gibt eine Übersicht über die Länder mit den höchsten und geringsten Zahlen internationaler Studierender in MINT. Deutschland ist hier Spitzenreiter, wobei dies unter anderem auch mit der Gesamtstudierendenzahl Deutschlands zusammenhängt."),
                       placement = "top",
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_international_map5")
  )
}

#' international_top10_mint_gender Server Functions
#'
#' @noRd
mod_international_top10_mint_intl_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$show_avg_top10_mint_int_line, {
      r$show_avg_ti <- input$show_avg_top10_mint_int_line
    })

    observeEvent(input$map_y_ti, {
      r$map_y_ti <- input$map_y_ti
    })

    observeEvent(input$map_f_ti, {
      r$map_f_ti <- input$map_f_ti
    })




  })
}

## To be copied in the UI
# mod_international_top10_mint_gender_ui("international_top10_mint_gender_1")

## To be copied in the server
# mod_international_top10_mint_gender_server("international_top10_mint_gender_1")
