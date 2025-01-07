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
      choices = 2013:2023,
      selected = 2023
    ),
    p("Studierendengruppen:"),
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
        choices = studi_det_ui_faecher(spezif_i ='Studierende (Lehramt)'),

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


  })
}

## To be copied in the UI
# mod_studium_studienzahl_bl_vergleich_ui("studium_studienzahl_bl_vergleich_1")

## To be copied in the server
# mod_studium_studienzahl_bl_vergleich_server("studium_studienzahl_bl_vergleich_1")
