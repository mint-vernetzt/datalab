#' studium_studienzahl_ranking_bl_subject_gender UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_studium_studienzahl_ranking_bl_subject_gender_ui <- function(id){
  ns <- NS(id)
  tagList(

    p("Auswahl des Jahrs:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("dumb_date"),
      label = NULL,
      choices = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021),
      selected = 2021
    ),
    # p("Auswahl des Indikators:"),
    # shinyWidgets::pickerInput(
    #   inputId = ns("dumbb_l"),
    #   choices = c("Studienanfänger:innen (1.Fachsemester)",
    #               "Studienanfänger:innen (1.Hochschulsemester)",
    #               "Studienanfänger:innen (Fachhochschulen, 1.Fachsemester)",
    #               "Studienanfänger:innen (Fachhochschulen, 1.Hochschulsemester)",
    #               "Studienanfänger:innen (Lehramt, Universität, 1.Fachsemester)",
    #               "Studienanfänger:innen (Lehramt, Universität, 1.Hochschulsemester)",
    #               "Studienanfänger:innen (Universität, 1.Fachsemester)",
    #               "Studienanfänger:innen (Universität, 1.Hochschulsemester)",
    #               "Studierende",
    #               "Studierende (Fachhochschulen)",
    #               "Studierende (Lehramt, Universität)",
    #               "Studierende (Universität)"
    #   ),
      # selected = c("Studierende")
      # ,
    #   multiple = T,
    #   options =  list(
    #     "max-options" = 2,
    #     "max-options-text" = "Maximal 3 Indikatoren auswählen")
    # ),
    p("Auswahl des Bundeslands:"),
    shinyWidgets::pickerInput(
      inputId = ns("dumbb_states"),
      choices = c("Deutschland",
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
                  "Westdeutschland",
                  "Ostdeutschland"
                  ),
      selected = "Nordrhein-Westfalen"
    )
  )
}

#' studium_studienzahl_ranking_bl_subject_gender Server Functions
#'
#' @noRd
mod_studium_studienzahl_ranking_bl_subject_gender_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$dumb_date, {
      r$dumb_date <- input$dumb_date
    })

    observeEvent(input$dumbb_l, {
      r$dumbb_l <- input$dumbb_l
    })

    observeEvent(input$dumbb_states, {
      r$dumbb_states <- input$dumbb_states
    })
#
#     observeEvent(input$hochschulform_studium_ranking_bl_subject_gender_2, {
#       r$hochschulform_studium_ranking_bl_subject_gender_2 <- input$hochschulform_studium_ranking_bl_subject_gender_2
#     })
#
#     observeEvent(input$states_studium_ranking_bl_subject_gender, {
#       r$states_studium_ranking_bl_subject_gender <- input$states_studium_ranking_bl_subject_gender
#     })


  })
}

## To be copied in the UI
# mod_studium_studienzahl_ranking_bl_subject_gender_ui("studium_studienzahl_ranking_bl_subject_gender_1")

## To be copied in the server
# mod_studium_studienzahl_ranking_bl_subject_gender_server("studium_studienzahl_ranking_bl_subject_gender_1")
