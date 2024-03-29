#' studium_studienzahl_test UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_studium_studienzahl_test_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Jahr:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("testy"),
      label = NULL,
      choices = c("2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022"),
      selected = "2022"
    ),
    p("Indikatoren (max. 3):"),
    shinyWidgets::pickerInput(
      inputId = ns("testl"),
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
      selected = c("Studierende"
                   , "Studienanfänger:innen (1.Fachsemester)"
                   ),
      multiple = TRUE,
      options =  list(
        "max-options" = 3,
        "max-options-text" = "Maximal 3 Indikatoren auswählen")
    ),
    br(),
    shinyBS::bsPopover(id="dh_studium_mint_1", title = "",
                       content = paste0("Falls die Grafiken abgeschnitten dargestellt werden, bitte das gesamte Ansichtsfenster einmal verkleinern und dann wieder maximieren. Dann stellt sich das Seitenverhältnis des Desktops richtig ein."),
                       trigger = "hover"),
    tags$a(paste0("Probleme bei der Darstellung"), icon("question-circle"), id = "dh_studium_mint_1"),
    br(),
    br(),
    shinyBS::bsPopover(id="ih_studium_mint_1", title="",
                       content = paste0("In der ersten Einstellung ist zu sehen, dass in Deutschland 38 % der Studienanfänger:innen (1. FS) ein MINT-Fach wählen, bei den Studierenden ist dieser Anteil mit 37 % in 2021 etwas geringer, was bedeutet, dass die Abbruchsquote in MINT höher liegt als in anderen Fachbereichen."),
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_studium_mint_1")
    )

}

#' studium_choice_gender Server Functions
#'
#' @noRd
mod_studium_studienzahl_test_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$testy, {
      r$testy <- input$testy
    })

    observeEvent(input$testl, {
      r$testl <- input$testl
    })

    # observeEvent(input$hochschulform_studium_choice_gender1, {
    #   r$hochschulform_studium_choice_gender1 <- input$hochschulform_studium_choice_gender1
    # })
    #
    # observeEvent(input$hochschulform_studium_choice_gender2, {
    #   r$hochschulform_studium_choice_gender2 <- input$hochschulform_studium_choice_gender2
    # })
    #
    # observeEvent(input$level_studium_choice_gender, {
    #   r$level_studium_choice_gender <- input$level_studium_choice_gender
    # })


  })
}

## To be copied in the UI
# mod_studium_choice_gender_ui("studium_choice_gender_1")

## To be copied in the server
# mod_studium_choice_gender_server("studium_choice_gender_1")
