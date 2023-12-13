#' international_schule_migration UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_international_schule_migration_ui <- function(id){

  logger::log_debug("start mod_international_schule_migration_ui")


  ns <- NS(id)
  tagList(
    p("Erhebung:"),

    p("Durchschnitt anzeigen:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("line_l_int_schule"),
      choices = c("TIMSS", "PISA"),
      justified = TRUE
    ),

    conditionalPanel(condition = "input.line_l_int_schule == 'TIMSS'",
                     ns = ns,
                     p("Jahr:"),
                     shinyWidgets::sliderTextInput(
                       inputId = ns("line_y_timss_int_schule"),
                       label = NULL,
                       choices = international_ui_years(region = "TIMSS"),
                       selected = "2019"
                     ),

                     p("Fachbereich:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("line_f_timss_int_schule"),
                       choices = c("Mathematik", "Naturwissenschaften"),
                       selected = c("Mathematik"),
                       multiple = FALSE
                     ),

                     p("Indikator"),
                     shinyWidgets::pickerInput(
                       inputId = ns("line_li_timss_int_schule"),
                       choices = c("nach Geschlecht", "nach sozialem Status"),
                       selected = c("nach Geschlecht"),
                       multiple = FALSE
                     )),

    conditionalPanel(condition = "input.line_l_int_schule == 'PISA'",
                     ns = ns,
                     p("Jahr:"),
                     shinyWidgets::sliderTextInput(
                       inputId = ns("line_y_pisa_int_schule"),
                       label = NULL,
                       choices = international_ui_years(region = "PISA"),
                       selected = "2022"
                     ),

                     p("Fachbereich:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("line_f_pisa_int_schule"),
                       choices = c("Mathematik", "Naturwissenschaften"),
                       selected = c("Mathematik"),
                       multiple = FALSE
                     ),

                     p("Indikator"),
                     shinyWidgets::pickerInput(
                       inputId = ns("line_li_pisa_int_schule"),
                       choices = c("nach Geschlecht",
                                   "nach Zuwanderungsgeschichte",
                                   "nach Bildungskapital"),
                       selected = c("nach Geschlecht"),
                       multiple = FALSE
                     )),

    br(),

    # TODO extract into own module, since this is repeated on a lot of modules

    shinyBS::bsPopover(id="dh_international_schule_migration", title = "",
                       content = paste0("Falls die Grafiken abgeschnitten dargestellt werden, bitte das gesamte Ansichtsfenster einmal verkleinern und dann wieder maximieren. Dann stellt sich das Seitenverhältnis des Desktops richtig ein."),
                       placement = "top",
                       trigger = "hover"),
    tags$a(paste0("Probleme bei der Darstellung"), icon("question-circle"), id = "dh_international_schule_migration"),
    br(),
    br(),
    shinyBS::bsPopover(id="ih_international_schule_migration", title="",
                       content = paste0("Die linke Karte der ersten Einstellung zeigt, dass die beiden Bundesländer mit dem höchsten Anteil von Informatik-Studierenden Bayern und Schleswig-Holstein mit jeweils 10 % sind."),
                       placement = "top",
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_international_migration")
  )

}

#' international_schule_migration Server Functions
#'
#' @noRd
mod_international_schule_migration_server <- function(id, r){

  logger::log_debug("start mod_international_schule_migration_server")

  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # region change updates respective sub inputs, which will otherwise
    # still be the last values.
    observeEvent(input$line_l_int_schule, {
      r$line_l_int_schule <- input$line_l_int_schule
      if (input$line_l_int_schule == "TIMSS") {
        r$line_y_int_schule <- input$line_y_timss_int_schule
        r$line_f_int_schule <- input$line_f_timss_int_schule
        r$line_li_int_schule <- input$line_li_timss_int_schule
      }
      if (input$line_l_int_schule == "PISA") {
        r$line_y_int_schule <- input$line_y_pisa_int_schule
        r$line_f_int_schule <- input$line_f_pisa_int_schule
        r$line_li_int_schule <- input$line_li_pisa_int_schule
      }
    })

    observeEvent(input$line_y_pisa_int_schule, {
      r$line_y_int_schule <- input$line_y_pisa_int_schule
    })

    observeEvent(input$line_f_pisa_int_schule, {
      r$line_f_int_schule <- input$line_f_pisa_int_schule
    })

    observeEvent(input$line_li_pisa_int_schule, {
      r$line_li_int_schule <- input$line_li_pisa_int_schule
    })

    # timss check should be after pisa check, since it is the default and will
    # otherwise be overwritten on initial load up
    observeEvent(input$line_y_timss_int_schule, {
      r$line_y_int_schule<- input$line_y_timss_int_schule
    })

    observeEvent(input$line_f_timss_int_schule, {
      r$line_f_int_schule <- input$line_f_timss_int_schule
    })

    observeEvent(input$line_li_timss_int_schule, {
      r$line_li_int_schule <- input$line_li_timss_int_schule
    })


  })
}

## To be copied in the UI
# mod_international_schule_migration_ui("international_schule_migration_1")

## To be copied in the server
# mod_international_schule_migration_server("international_schule_migration_1")
