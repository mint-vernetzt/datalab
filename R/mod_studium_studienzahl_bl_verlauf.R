#' studium_studienzahl_bl_verlauf UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_studium_studienzahl_bl_verlauf_ui <- function(id){
  ns <- NS(id)
  tagList(

    p("Auswahl des Zeitraum:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_studium_studienzahl_bl_verlauf"),
      label = NULL,
      choices = c("2013", "2014", "2015", "2016", "2017",
                  "2018","2019", "2020", "2021"),
      selected = c("2015", "2021")
    ),
    # p("Nur Lehramt anzeigen:"),
    # tags$div(
    #   shinyWidgets::materialSwitch(inputId = ns("nurLehramt_studium_studienzahl_bl_verlauf"), label = "Nein", inline = TRUE),
    #   tags$span("Ja"),
    #   p("Auswahl der Hochschulform:"),
    #   conditionalPanel(condition = "input.nurLehramt_studium_studienzahl_bl_verlauf == false",
    #                    ns = ns,
    #                    shinyWidgets::pickerInput(
    #                      inputId = ns("hochschulform_studium_studienzahl_bl_verlauf1"),
    #                      choices = c("Alle Hochschulen"="insgesamt", "Universität" = "Uni", "Fachhochschule" = "FH")
    #                    )),
    #   conditionalPanel(condition = "input.nurLehramt_studium_studienzahl_bl_verlauf != false",
    #                    ns = ns,
    #                    shinyWidgets::pickerInput(
    #                      inputId = ns("hochschulform_studium_studienzahl_bl_verlauf2"),
    #                      choices = "Uni"
    #                    )),
      p("Auswahl des Fachs:"),
      shinyWidgets::pickerInput(
        inputId = ns("subject_studium_studienzahl_bl_verlauf"),
        choices = c("MINT-Fächer (gesamt)","Mathematik/Naturwissenschaften", "Ingenieurwissenschaften"),
        selected = "MINT-Fächer (gesamt)"
      )
    ,
    p("Auswahl des Indikators:"),
    shinyWidgets::pickerInput(
      inputId = ns("verl_bl_l"),
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
      selected = c("Studierende")
      ,
      multiple = F,
      options =  list(
        "max-options" = 3,
        "max-options-text" = "Maximal 3 Indikatoren auswählen")
    ),
    p("Auswahl der Bundesländer:"),
    shinyWidgets::pickerInput(
      inputId = ns("states_studium_studienzahl_bl_verlauf"),
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
      selected = c("Baden-Württemberg", "Hamburg"),
      options = list(`actions-box` = TRUE,
                     `deselect-all-text` = "Alle abwählen",
                     `select-all-text` = "Alle auswählen"),
      multiple = TRUE
    )

  )
}

#' studium_studienzahl_bl_verlauf Server Functions
#'
#' @noRd
mod_studium_studienzahl_bl_verlauf_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$date_studium_studienzahl_bl_verlauf, {
      r$date_studium_studienzahl_bl_verlauf <- input$date_studium_studienzahl_bl_verlauf
    })

    observeEvent(input$nurLehramt_studium_studienzahl_bl_verlauf, {
      r$nurLehramt_studium_studienzahl_bl_verlauf <- input$nurLehramt_studium_studienzahl_bl_verlauf
    })

    observeEvent(input$verl_bl_l, {
      r$verl_bl_l <- input$verl_bl_l
    })

    observeEvent(input$hochschulform_studium_studienzahl_bl_verlauf1, {
      r$hochschulform_studium_studienzahl_bl_verlauf1 <- input$hochschulform_studium_studienzahl_bl_verlauf1
    })

    observeEvent(input$hochschulform_studium_studienzahl_bl_verlauf2, {
      r$hochschulform_studium_studienzahl_bl_verlauf2 <- input$hochschulform_studium_studienzahl_bl_verlauf2
    })

    observeEvent(input$subject_studium_studienzahl_bl_verlauf, {
      r$subject_studium_studienzahl_bl_verlauf <- input$subject_studium_studienzahl_bl_verlauf
    })

    observeEvent(input$level_studium_studienzahl_bl_verlauf, {
      r$level_studium_studienzahl_bl_verlauf <- input$level_studium_studienzahl_bl_verlauf
    })

    observeEvent(input$states_studium_studienzahl_bl_verlauf, {
      r$states_studium_studienzahl_bl_verlauf <- input$states_studium_studienzahl_bl_verlauf
    })


  })
}

## To be copied in the UI
# mod_studium_studienzahl_bl_verlauf_ui("studium_studienzahl_bl_verlauf_1")

## To be copied in the server
# mod_studium_studienzahl_bl_verlauf_server("studium_studienzahl_bl_verlauf_1")
