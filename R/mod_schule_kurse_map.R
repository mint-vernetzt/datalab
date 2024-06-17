#' schule_kurse_map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_schule_kurse_map_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Jahr:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_map"),
      label = NULL,
      choices = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021,2022),
      selected = 2022
    ),
    p("Fach/Fächergruppe:"),
    shinyWidgets::pickerInput(
      inputId = ns("subject_map"),
      choices = c("MINT-Fächer (gesamt)",
                  "Mathematik",
                  "Informatik",
                  "Physik",
                  "Chemie",
                  "Biologie",
                  "andere Fächer (gesamt)",
                  "Deutsch",
                  "Fremdsprachen",
                  "Gesellschaftswissenschaften",
                  "Musik/Kunst",
                  "Religion/Ethik",
                  "Sport"),
      selected = "MINT-Fächer (gesamt)"
    ),
    br(),
    shinyBS::bsPopover(id="dh_schule_fach_1", title = "",
                       content = paste0("Falls die Grafiken abgeschnitten dargestellt werden, bitte das gesamte Ansichtsfenster einmal verkleinern und dann wieder maximieren. Dann stellt sich das Seitenverhältnis des Desktops richtig ein."),
                       trigger = "hover"),
    tags$a(paste0("Probleme bei der Darstellung"), icon("question-circle"), id = "dh_schule_fach_1"),
    br(),
    br(),
    shinyBS::bsPopover(id="ih_schule_fach_1", title="",
                       content = paste0("In der ersten Einstellung ist zu sehen, dass der Anteil von MINT-Fächern an den Grundkursbelegung mit 29 % in Sachsen deutschlandweit am höchsten ist. Bei den Leistungskursen ist der Anteil der MINT-Fächer in Sachsen-Anhalt mit 50 % am höchsten. Die Vergleiche zwischen den Bundesländern sind jedoch schwierig, da die Regelungen für die Wahl der Kurse in den Bundesländern sehr unterschiedlich sind."),
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_schule_fach_1")

  )
}

#' schule_kurse_map Server Functions
#'
#' @noRd
mod_schule_kurse_map_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$date_map, {
      r$date_map <- input$date_map
    })

    observeEvent(input$subject_map, {
      r$subject_map <- input$subject_map
    })

  })
}

## To be copied in the UI
# mod_schule_kurse_map_ui("schule_kurse_map_1")

## To be copied in the server
# mod_schule_kurse_map_server("schule_kurse_map_1")
