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

    p("Jahr:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("bl_date"),
      label = NULL,
      choices = 2013:2022,
      selected = 2022
    ),
    p("Indikator:"),
    shinyWidgets::pickerInput(
      inputId = ns("rank_bl_l"),
      choices = c("Internationale Studienanfänger:innen (1. Hochschulsemester)",
                  "Studienanfänger:innen (1. Fachsemester)",
                  "Studierende",
                  "Internationale Studierende",
                  "Studienanfänger:innen (1. Hochschulsemester)",
                  "Studierende (Lehramt)"
      ),
      selected = "Studierende"),

    #Conditional Panel, um für Lehramt nur sinnvollere Fächer auswählen zu lassen
      p("Fach/Fächergruppe:"),
    conditionalPanel(condition = "input.rank_bl_l == 'Studierende (Lehramt)'",
                     ns = ns,
      shinyWidgets::pickerInput(
        inputId = ns("bl_f_lehr"),
        choices = studi_det_ui_faecher(0),

        selected = "Alle MINT-Fächer"
      )),

    conditionalPanel(condition = "input.rank_bl_l == 'Internationale Studienanfänger:innen (1. Hochschulsemester)' |
                     input.rank_bl_l == 'Studienanfänger:innen (1. Fachsemester)' |
                     input.rank_bl_l == 'Studierende' |
                     input.rank_bl_l == 'Internationale Studierende' |
                     input.rank_bl_l == 'Studienanfänger:innen (1. Hochschulsemester)'",
                     ns = ns,
                     shinyWidgets::pickerInput(
                       inputId = ns("bl_f_alle"),

                       choices = studi_det_ui_faecher(spezif_i =c('Internationale Studienanfänger:innen (1. Hochschulsemester)',
                                                                 'Studienanfänger:innen (1. Fachsemester)',
                                                                 'Studierende',
                                                                 'Internationale Studierende',
                                                                 'Studienanfänger:innen (1. Hochschulsemester)')),

                       selected = "Alle MINT-Fächer"
                     )),
    br(),
    shinyBS::bsPopover(id="ih_studium_fach_5", title="",
                       content = paste0("Die Übersicht zeigt, dass der Anteil von Studierenden in MINT an allen Studierenden zwischen den Bundesländern zwischen 24 % (Thüringen, Saarland) und 42 % (Sachsen) liegt."),
                       placement = "top",
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_studium_fach_5")


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

    observeEvent(input$bl_f_lehr, {
      r$bl_f_lehr <- input$bl_f_lehr
    })

    observeEvent(input$bl_f_alle, {
      r$bl_f_alle <- input$bl_f_alle
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
