#' studium_studienzahl_bl_map UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_studium_studienzahl_bl_map_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Jahr:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("map_y"),
      label = NULL,
      choices = c("2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021", "2022"),
      selected = "2022"
    ),
    p("Indikator:"),
    shinyWidgets::pickerInput(
      inputId = ns("map_l"),
      choices = c("Internationale Studienanfänger:innen (1. Hochschulsemester)",
                  "Studienanfänger:innen (1. Fachsemester)",
                  "Studierende",
                  "Internationale Studierende",
                  "Studierende (Lehramt)"

      ),
      selected = c("Studierende")
      ,
      multiple = F,
      options =  list(
        "max-options" = 2,
        "max-options-text" = "Maximal 2 Indikatoren auswählen")
    ),
    #Conditional Panel, um für Lehramt nur sinnvollere Fächer auswählen zu lassen
    p("Fächer/Fächergruppen (max. 2):"),


    conditionalPanel(condition = "input.map_l == 'Studierende (Lehramt)'",
                     ns = ns,
                     shinyWidgets::pickerInput(
                       inputId = ns("map_f_lehr"),
                       choices = studi_det_ui_faecher(spezif_i ='Studierende (Lehramt)') ,


                       selected = c("Informatik", "Alle MINT-Fächer"),
                       multiple = TRUE,
                       options =  list(
                         "max-options" = 2,
                         "max-options-text" = "Maximal 2 Indikatoren auswählen")
                     )),

    conditionalPanel(condition = "input.map_l == 'Internationale Studienanfänger:innen (1. Hochschulsemester)' |
                     input.map_l == 'Studienanfänger:innen (1. Fachsemester)' |
                     input.map_l == 'Studierende' |
                     input.map_l == 'Internationale Studierende' |
                     input.map_l == 'Studienanfänger:innen (1. Hochschulsemester)'",
                     ns = ns,
                     shinyWidgets::pickerInput(
                       inputId = ns("map_f"),

                       choices =  studi_det_ui_faecher(spezif_i =c('Internationale Studienanfänger:innen (1. Hochschulsemester)',
                                                                   'Studienanfänger:innen (1. Fachsemester)',
                                                                   'Studierende',
                                                                   'Internationale Studierende',
                                                                   'Studienanfänger:innen (1. Hochschulsemester)')),

                       selected = c(
                         "Informatik", "Alle MINT-Fächer"),
                       multiple = TRUE,
                       options =  list(
                         "max-options" = 2,
                         "max-options-text" = "Maximal 2 Indikatoren auswählen")
                     )),
    br(),
    shinyBS::bsPopover(id="dh_studium_fach_2", title = "",
                       content = paste0("Falls die Grafiken abgeschnitten dargestellt werden, bitte das gesamte Ansichtsfenster einmal verkleinern und dann wieder maximieren. Dann stellt sich das Seitenverhältnis des Desktops richtig ein."),
                       placement = "top",
                       trigger = "hover"),
    tags$a(paste0("Probleme bei der Darstellung"), icon("question-circle"), id = "dh_studium_fach_2"),
    br(),
    br(),
    shinyBS::bsPopover(id="ih_studium_fach_2", title="",
                       content = paste0("Die linke Karte der ersten Einstellung zeigt, dass die beiden Bundesländer mit dem höchsten Anteil von Informatik-Studierenden Bayern und Schleswig-Holstein mit jeweils 10 % sind."),
                       placement = "top",
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_studium_fach_2")
  )

}

#' studium_studienzahl_bl_map Server Functions
#'
#' @noRd
mod_studium_studienzahl_bl_map_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$map_y, {
      r$map_y <- input$map_y
    })

    observeEvent(input$map_l, {
      r$map_l <- input$map_l
    })

    observeEvent(input$map_f_lehr, {
      r$map_f_lehr <- input$map_f_lehr
    })

    observeEvent(input$map_f, {
      r$map_f <- input$map_f
    })

    # observeEvent(input$hochschulform_studium_studienzahl_bl_map2, {
    #   r$hochschulform_studium_studienzahl_bl_map2 <- input$hochschulform_studium_studienzahl_bl_map2
    # })
    #
    # observeEvent(input$subject_studium_studienzahl_bl_map, {
    #   r$subject_studium_studienzahl_bl_map <- input$subject_studium_studienzahl_bl_map
    # })

  })
}

## To be copied in the UI
# mod_studium_studienzahl_bl_map_ui("studium_studienzahl_bl_map_1")

## To be copied in the server
# mod_studium_studienzahl_bl_map_server("studium_studienzahl_bl_map_1")
