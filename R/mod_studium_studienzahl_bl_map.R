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
    p("Fächer/Fächergruppen (max. 2):"),
    shinyWidgets::pickerInput(
      inputId = ns("map_f"),
      choices = c("Agrar-, Forst- und Ernährungswissenschaften, Veterinärmedizin",
                  "Biologie",
                  "Elektrotechnik und Informationstechnik",
                  "Geowissenschaften und Geographie",
                  "Informatik",
                  "Ingenieurwissenschaften ohne Informatik",
                  "Maschinenbau/Verfahrenstechnik",
                  "Mathematik",
                  "Alle MINT-Fächer",
                  "Alle Nicht MINT-Fächer",
                  "Physik, Astronomie",
                  "Rechts-, Wirtschafts- und Sozialwissenschaften",
                  "Vermessungswesen",
                  "Architektur, Innenarchitektur",
                  "Bauingenieurwesen",
                  "Chemie",
                  "Geisteswissenschaften",
                  "Humanmedizin/Gesundheitswissenschaften",
                  "Ingenieurwissenschaften",
                  "Kunst, Kunstwissenschaft",
                  "Materialwissenschaft und Werkstofftechnik",
                  "Mathematik, Naturwissenschaften",
                  "Naturwissenschaften",
                  "Pharmazie",
                  "Sport",
                  "Verkehrstechnik, Nautik",
                  "Wirtschaftsingenieurwesen mit ingenieurwissenschaftlichem Schwerpunkt"
      ),
      selected = c(
                   "Informatik", "MINT"),
      multiple = TRUE,
      options =  list(
        "max-options" = 2,
        "max-options-text" = "Maximal 2 Indikatoren auswählen")
    )

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
