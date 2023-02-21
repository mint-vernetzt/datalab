#' studium_studienzahl_verlauf_bl_subject_gender UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_studium_studienzahl_verlauf_bl_subject_gender_ui <- function(id){
  ns <- NS(id)
  tagList(

    p("Auswahl des Zeitraums:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("choice_V_y"),
      label = NULL,
      choices = c("2013", "2014", "2015", "2016", "2017", "2018","2019", "2020", "2021"),
      selected = c("2015","2021")
    ),
    p("Auswahl des Indikators:"),
    shinyWidgets::pickerInput(
      inputId = ns("choice_l_v"),
      choices = c("Studienanfänger:innen (1.Fachsemester)",
                  "Studienanfänger:innen (1.Hochschulsemester)",
                  "Studienanfänger:innen (Fachhochschulen, 1.Fachsemester)",
                  "Studienanfänger:innen (Fachhochschulen, 1.Hochschulsemester)",
                  "Studienanfänger:innen (Lehramt, Universität, 1.Fachsemester)",
                  "Studienanfänger:innen (Lehramt, Universität, 1.Hochschulsemester)",
                  "Studienanfänger:innen (Universität, 1.Fachsemester)",
                  "Studienanfänger:innen (Universität, 1.Hochschulsemester)",
                  "Studierende",
                  "Studierende (Fachhochschulen)",
                  "Studierende (Lehramt, Universität)",
                  "Studierende (Universität)"
      ),
      selected = c("Studierende", "Studienanfänger:innen (Fachhochschulen, 1.Fachsemester)")
      ,
      multiple = T,
      options =  list(
        "max-options" = 3,
        "max-options-text" = "Maximal 3 Indikatoren auswählen")
    ),
    p("Auswahl des Fachs:"),
    shinyWidgets::pickerInput(
      inputId = ns("choice_v_f"),

      choices = c("MINT","Mathematik/Naturwissenschaften", "Ingenieurwissenschaften"),

      selected = "MINT"
    ),
    p("Auswahl des Bundeslands:"),
    shinyWidgets::pickerInput(
      inputId = ns("choice_states"),
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
                  "Westen",
                  "Osten"
      ),
      selected = "Sachsen"
    ))
}

#' studium_studienzahl_verlauf_bl_subject_gender Server Functions
#'
#' @noRd
mod_studium_studienzahl_verlauf_bl_subject_gender_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$choice_states, {
      r$choice_states <- input$choice_states
    })

    observeEvent(input$choice_v_f, {
      r$choice_v_f <- input$choice_v_f
    })

    observeEvent(input$choice_l_v, {
      r$choice_l_v <- input$choice_l_v
    })

    observeEvent(input$choice_V_y, {
      r$choice_V_y <- input$choice_V_y
    })

    observeEvent(input$subject_verlauf_bl_subject_gender, {
      r$subject_verlauf_bl_subject_gender <- input$subject_verlauf_bl_subject_gender
    })

    observeEvent(input$states_verlauf_bl_subject_gender, {
      r$states_verlauf_bl_subject_gender <- input$states_verlauf_bl_subject_gender
    })


  })
}

## To be copied in the UI
# mod_studium_studienzahl_verlauf_bl_subject_gender_ui("studium_studienzahl_verlauf_bl_subject_gender_1")

## To be copied in the server
# mod_studium_studienzahl_verlauf_bl_subject_gender_server("studium_studienzahl_verlauf_bl_subject_gender_1")
