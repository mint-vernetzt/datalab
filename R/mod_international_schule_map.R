#' international_schule_map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_international_schule_map_ui <- function(id){

  # logger::log_debug("start mod_international_schule_map_ui")

  ns <- NS(id)
  tagList(
    p("Erhebung:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("map_l_int_schule"),
      choices = c("TIMSS", "PISA"),
      justified = TRUE#,
      # checkIcon = list(yes = icon("ok",
      #                             lib = "glyphicon"))
    ),

    #Conditional Panel, um für Lehramt nur sinnvollere Fächer auswählen zu lassen

    conditionalPanel(condition = "input.map_l_int_schule == 'TIMSS'",
                     ns = ns,
                     p("Jahr:"),
                     shinyWidgets::sliderTextInput(
                       inputId = ns("map_y_timss_int_schule"),
                       label = NULL,
                       choices = international_ui_years(region = "TIMSS"),
                       selected = "2019"
                     ),

                     p("Fachbereich:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("map_f_timss_int_schule"),
                       choices = c("Mathematik", "Naturwissenschaften"),
                       selected = c("Mathematik"),
                       multiple = FALSE
                     ),

                     p("Leistungsindikator:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("map_li_timss_int_schule"),
                       choices = c("Test-Punktzahl",
                                   "Mittlerer internationaler Standard" =  "Mittlerer Standard erreicht"),
                       selected = c("Test-Punktzahl"),
                       multiple = FALSE
                     )),

    conditionalPanel(condition = "input.map_l_int_schule == 'PISA'",
                     ns = ns,
                     p("Jahr:"),
                     shinyWidgets::sliderTextInput(
                       inputId = ns("map_y_pisa_int_schule"),
                       label = NULL,
                       choices = international_ui_years(region = "PISA"),
                       selected = "2022"
                     ),

                     p("Fachbereich:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("map_f_pisa_int_schule"),
                       choices = c("Mathematik", "Naturwissenschaften"),
                       selected = c("Mathematik"),
                       multiple = FALSE
                     ),

                     # p("Leistungsindikator"),
                     # shinyWidgets::pickerInput(
                     #   inputId = ns("map_li_pisa_int_schule"),
                     #   choices = c("Test-Punktzahl"),
                     #   selected = c("Test-Punktzahl"),
                     #   multiple = FALSE
                     # )
                     ),

    br(),

    # TODO extract into own module, since this is repeated on a lot of modules

    # shinyBS::bsPopover(id="dh_international_schule_map", title = "",
    #                    content = paste0("Falls die Grafiken abgeschnitten dargestellt werden, bitte das gesamte Ansichtsfenster einmal verkleinern und dann wieder maximieren. Dann stellt sich das Seitenverhältnis des Desktops richtig ein."),
    #                    placement = "top",
    #                    trigger = "hover"),
    # tags$a(paste0("Probleme bei der Darstellung"), icon("question-circle"), id = "dh_international_schule_map"),
    # br(),
    # br(),
    shinyBS::bsPopover(id="ih_international_schule_map", title="",
                       content = paste0("Die erste Einstellung der Karte zeigt, das Schüler:innen in Deutschland 2019 im Durchschnitt 521 Punkte im Mathematiktest von TIMSS erziehlen. Damit liegen sie z. B. etwas hinter Schüler*innen aus den Niederlande oder den USA."),
                       placement = "top",
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_international_schule_map")
  )
}

#' international_schule_map Server Functions
#'
#' @noRd
mod_international_schule_map_server <- function(id, r){

  # logger::log_debug("start mod_international_schule_map_server")

  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # region change updates respective sub inputs, which will otherwise
    # still be the last values.
    observeEvent(input$map_l_int_schule, {
      r$map_l_int_schule <- input$map_l_int_schule
      if (input$map_l_int_schule == "TIMSS") {
        r$map_y_int_schule <- input$map_y_timss_int_schule
        r$map_f_int_schule <- input$map_f_timss_int_schule
        r$map_li_int_schule <- input$map_li_timss_int_schule
      }
      if (input$map_l_int_schule == "PISA") {
        r$map_y_int_schule <- input$map_y_pisa_int_schule
        r$map_f_int_schule <- input$map_f_pisa_int_schule
        # r$map_li_int_schule <- input$map_li_pisa_int_schule
      }
    })

    observeEvent(input$map_y_pisa_int_schule, {
      r$map_y_int_schule <- input$map_y_pisa_int_schule
    })

    observeEvent(input$map_f_pisa_int_schule, {
      r$map_f_int_schule <- input$map_f_pisa_int_schule
    })

    # observeEvent(input$map_li_pisa_int_schule, {
    #   r$map_li_int_schule <- input$map_li_pisa_int_schule
    # })

    # eu check should be after oecd check, since it is the default and will
    # otherwise be overwritten on initial load up
    observeEvent(input$map_y_timss_int_schule, {
      r$map_y_int_schule<- input$map_y_timss_int_schule
    })

    observeEvent(input$map_f_timss_int_schule, {
      r$map_f_int_schule <- input$map_f_timss_int_schule
    })

    observeEvent(input$map_li_timss_int_schule, {
      r$map_li_int_schule <- input$map_li_timss_int_schule
    })


  })
}

## To be copied in the UI
# mod_international_schule_map_ui("international_schule_map_1")

## To be copied in the server
# mod_international_schule_map_server("international_schule_map_1")
