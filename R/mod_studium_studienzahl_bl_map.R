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
      choices = c("2018", "2019", "2020", "2021"),
      selected = "2021"
    ),
    p("Indikator:"),
    shinyWidgets::pickerInput(
      inputId = ns("map_l"),
      choices = c("Auländische Studienanfänger:innen (1. Hochschulsemester)",
                  "Studienanfänger:innen (1. Fachsemester)",
                  "Studierende",
                  "Ausländische Studierende",
                  "Studierende (Nur Lehramt)"

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
    conditionalPanel(condition = "input.map_l == 'Studierende (Nur Lehramt)'",
                     ns = ns,
                     shinyWidgets::pickerInput(
                       inputId = ns("map_f_lehr"),
                       choices = c("Agrar-, Forst- und Ernährungswissenschaften, Veterinärmedizin",
                                   "Biologie",
                                   "Geowissenschaften und Geographie",
                                   "Informatik",
                                   "Maschinenbau/Verfahrenstechnik",
                                   "Nicht MINT",
                                   "MINT",
                                   "Chemie",
                                   "Mathematik",
                                   "Humanmedizin/Gesundheitswissenschaften",
                                   "Geisteswissenschaften",
                                   "Ingenieurwissenschaften",
                                   "Ingenieurwissenschaften ohne Informatik",
                                   "Physik, Astronomie",
                                   "Rechts-, Wirtschafts- und Sozialwissenschaften",
                                   "Mathematik, Naturwissenschaften",
                                   "Naturwissenschaften",
                                   "Sport",
                                   "Kunst, Kunstwissenschaft"),

                       selected = c(
                         "Informatik", "MINT"),
                       multiple = TRUE,
                       options =  list(
                         "max-options" = 2,
                         "max-options-text" = "Maximal 2 Indikatoren auswählen")
                     )),

    conditionalPanel(condition = "input.map_l == 'Auländische Studienanfänger:innen (1. Hochschulsemester)' |
                     input.map_l == 'Studienanfänger:innen (1. Fachsemester)' |
                     input.map_l == 'Studierende' |
                     input.map_l == 'Ausländische Studierende' |
                     input.map_l == 'Studienanfänger:innen (1. Hochschulsemester)'",
                     ns = ns,
                     shinyWidgets::pickerInput(
                       inputId = ns("map_f"),

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

                       selected = c(
                         "Informatik", "MINT"),
                       multiple = TRUE,
                       options =  list(
                         "max-options" = 2,
                         "max-options-text" = "Maximal 2 Indikatoren auswählen")
                     ))
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
