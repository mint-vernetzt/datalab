#' studium_studienzahl_bl_vergleich UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_studium_studienzahl_bl_vergleich_ui <- function(id){
  ns <- NS(id)
  tagList(

    p("Auswahl des Jahres:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("bl_date"),
      label = NULL,
      choices = c("2018", "2019", "2020", "2021"),
      selected = "2021"
    ),
    p("Auswahl des Indikators:"),
    shinyWidgets::pickerInput(
      inputId = ns("rank_bl_l"),
      choices = c("Auländische Studienanfänger:innen (1. Hochschulsemester)",
                  "Studienanfänger:innen (1. Fachsemester)",
                  "Studierende",
                  "Ausländische Studierende",
                  "Studienanfänger:innen (1. Hochschulsemester)",
                  "Studierende (Nur Lehramt)"
      ),
      selected = "Studierende"),
    # p("Nur Lehramt anzeigen:"),
    # tags$div(
    #   shinyWidgets::materialSwitch(inputId = ns("nurLehramt_studium_studienzahl_bl_vergleich"), label = "Nein", inline = TRUE),
    #   tags$span("Ja"),
    #   p("Auswahl der Hochschulform:"),
    #   conditionalPanel(condition = "input.nurLehramt_studium_studienzahl_bl_vergleich == false",
    #                    ns = ns,
    #                    shinyWidgets::pickerInput(
    #                      inputId = ns("hochschulform_studium_studienzahl_bl_vergleich1"),
    #                      choices = c("Alle Hochschulen"="insgesamt", "Universität" = "Uni", "Fachhochschule" = "FH")
    #                    )),
    #   conditionalPanel(condition = "input.nurLehramt_studium_studienzahl_bl_vergleich != false",
    #                    ns = ns,
    #                    shinyWidgets::pickerInput(
    #                      inputId = ns("hochschulform_studium_studienzahl_bl_vergleich2"),
    #                      choices = "Uni"
    #                    )),
      p("Auswahl des Fachs:"),
      shinyWidgets::pickerInput(
        inputId = ns("bl_f"),

        choices = c("Agrar-, Forst- und Ernährungswissenschaften, Veterinärmedizin",
                    "Biologie",
                    "Geowissenschaften und Geographie",
                    "Informatik",
                    "Maschinenbau/Verfahrenstechnik",
                    "Nicht MINT",
                    "MINT",
                    "Vermessungswesen",
                    "Architektur, Innenarchitektur",
                    "Bauingenieurwesen",
                    "Chemie",
                    "Mathematik",
                    "Materialwissenschaft und Werkstofftechnik",
                    "Humanmedizin/Gesundheitswissenschaften",
                    "Geisteswissenschaften",
                    "Ingenieurwissenschaften",
                    "Ingenieurwissenschaften ohne Informatik",
                    "Physik, Astronomie",
                    "Rechts-, Wirtschafts- und Sozialwissenschaften",
                    "Mathematik, Naturwissenschaften",
                    "Naturwissenschaften",
                    "Pharmazie",
                    "Raumplanung",
                    "Sport",
                    "Verkehrstechnik, Nautik",
                    "Wirtschaftsingenieurwesen mit ingenieurwissenschaftlichem Schwerpunkt",
                    "Kunst, Kunstwissenschaft",
                    "Elektrotechnik und Informationstechnik"),

        selected = "MINT"
      )
    # ,
    # p("Status der Student:innen:"),
    # shinyWidgets::radioGroupButtons(
    #   inputId = ns("level_studium_studienzahl_bl_vergleich"),
    #   choices = c("Studienanfänger:innen"="Studienanfänger:innen", "Studierende"),
    #   direction = "vertical",
    #   justified = TRUE,
    #   checkIcon = list(yes = icon("ok",
    #                               lib = "glyphicon"))
    # )
  )
}

#' studium_studienzahl_bl_vergleich Server Functions
#'
#' @noRd
mod_studium_studienzahl_bl_vergleich_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$bl_f, {
      r$bl_f <- input$bl_f
    })

    observeEvent(input$rank_bl_l, {
      r$rank_bl_l <- input$rank_bl_l
    })

    observeEvent(input$bl_date, {
      r$bl_date <- input$bl_date
    })

    # observeEvent(input$hochschulform_studium_studienzahl_bl_vergleich2, {
    #   r$hochschulform_studium_studienzahl_bl_vergleich2 <- input$hochschulform_studium_studienzahl_bl_vergleich2
    # })
    #
    # observeEvent(input$subject_studium_studienzahl_bl_vergleich, {
    #   r$subject_studium_studienzahl_bl_vergleich <- input$subject_studium_studienzahl_bl_vergleich
    # })
    #
    # observeEvent(input$level_studium_studienzahl_bl_vergleich, {
    #   r$level_studium_studienzahl_bl_vergleich <- input$level_studium_studienzahl_bl_vergleich
    # })

  })
}

## To be copied in the UI
# mod_studium_studienzahl_bl_vergleich_ui("studium_studienzahl_bl_vergleich_1")

## To be copied in the server
# mod_studium_studienzahl_bl_vergleich_server("studium_studienzahl_bl_vergleich_1")
