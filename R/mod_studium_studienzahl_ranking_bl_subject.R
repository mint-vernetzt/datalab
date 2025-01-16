#' studium_studienzahl_ranking_bl_subject UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_studium_studienzahl_ranking_bl_subject_ui <- function(id){
  ns <- NS(id)
  tagList(

    p("Auswahl des Zeitraum:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("rank_y"),
      label = NULL,
      choices = c("2013", "2014", "2015", "2016", "2017",
                  "2018","2019", "2020", "2021", "2022"),
      selected = c( "2022")

    ),
    p("Auswahl der Studierendengruppen:"),
    shinyWidgets::pickerInput(
      inputId = ns("rank_l"),
      choices = c("Internationale Studienanfänger:innen (1. Hochschulsemester)",
                  "Studienanfänger:innen (1. Fachsemester)",
                  "Studierende",
                  "Internationale Studierende",
                  "Studienanfänger:innen (1. Hochschulsemester)",
                  "Studierende (Lehramt)"
      ),
      selected = "Studierende"),

    p("Auswahl des Bundeslands:"),
    shinyWidgets::pickerInput(
      inputId = ns("rank_states"),
      choices = c("Deutschland",
                  "Baden-Württemberg",
                  "Bayern",
                  "Berlin",
                  "Brandenburg",
                  "Bremen",
                  "Hamburg",
                  "Hessen",
                  "Mecklenburg-Vorpommern",
                  "Niedersachsen",
                  "Nordrhein-Westfalen",
                  "Rheinland-Pfalz",
                  "Saarland",
                  "Sachsen",
                  "Sachsen-Anhalt",
                  "Schleswig-Holstein",
                  "Thüringen"
                  ,
                  "Ostdeutschland (inkl. Berlin)",
                  "Westdeutschland (o. Berlin)"
                  ),
      selected = "Rheinland-Pfalz"
    ),
    br(),
    shinyBS::bsPopover(id="ih_studium_fach_3", title="",
                       content = paste0("Die Darstellung zeigt, wie groß der Anteil Studierender in einzelnen MINT-Fächern an allen Studierenden ist. In der ersten Einstellung sieht man beispielsweise, 2021 studieren in Rheinland-Pfalz 9.085 Personen (7,5 %) Informatik."),
                       placement = "top",
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_studium_fach_3")
    )

}

#' studium_studienzahl_ranking_bl_subject Server Functions
#'
#' @noRd
mod_studium_studienzahl_ranking_bl_subject_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$rank_l, {
      r$rank_l <- input$rank_l
    })

    observeEvent(input$rank_states, {
      r$rank_states <- input$rank_states
    })


    observeEvent(input$rank_y, {
      r$rank_y <- input$rank_y
    })

    observeEvent(input$nurLehramt_studierende_ranking_bl_subject, {
      r$nurLehramt_studierende_ranking_bl_subject <- input$nurLehramt_studierende_ranking_bl_subject
    })

    observeEvent(input$hochschulform_studierende_ranking_bl_1, {
      r$hochschulform_studierende_ranking_bl_1 <- input$hochschulform_studierende_ranking_bl_1
    })

    observeEvent(input$hochschulform_studierende_ranking_bl_2, {
      r$hochschulform_studierende_ranking_bl_2 <- input$hochschulform_studierende_ranking_bl_2
    })


  })
}

## To be copied in the UI
# mod_studium_studienzahl_ranking_bl_subject_ui("studium_studienzahl_ranking_bl_subject_1")

## To be copied in the server
# mod_studium_studienzahl_ranking_bl_subject_server("studium_studienzahl_ranking_bl_subject_1")
