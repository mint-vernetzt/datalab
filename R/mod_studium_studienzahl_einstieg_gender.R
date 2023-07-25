#' studium_studienzahl_einstieg_gender UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_studium_studienzahl_einstieg_gender_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Jahr:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("gen_y"),
      label = NULL,
      choices = c("2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021"),
      selected = "2021"
    ),
    p("Indikatoren (max. 3):"),
    shinyWidgets::pickerInput(
      inputId = ns("gen_l"),
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
      selected = c("Studierende", "Studienanfänger:innen (1.Fachsemester)"),
      multiple = TRUE,
      options =  list(
        "max-options" = 3,
        "max-options-text" = "Maximal 3 Indikatoren auswählen")
    ),
    br(),
    shinyBS::bsPopover(id="dh_studium_frauen_1", title = "",
                       content = paste0("Falls die Grafiken abgeschnitten dargestellt werden, bitte das gesamte Ansichtsfenster einmal verkleinern und dann wieder maximieren. Dann stellt sich das Seitenverhältnis des Desktops richtig ein."),
                       trigger = "hover"),
    tags$a(paste0("Probleme bei der Darstellung"), icon("question-circle"), id = "dh_studium_frauen_1"),
    br(),
    br(),
    shinyBS::bsPopover(id="ih_studium_frauen_1", title="",
                       content = paste0("In der ersten interaktiven Grafik ist zu sehen, dass deutschlandweit 2021 der Anteil von Frauen unter den Studienanfänger:innen in MINT-Fächern 34 % ausmacht. Unter den Studierenden liegt der Frauenanteil in MINT-Fächern bei 32 % etwas darunter. Dies deutet darauf hin, dass bei weiblichen Studierenden die Abbruchquote in MINT höher ist als bei männlichen Studierenden."),
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_studium_frauen_1")
  )

}
#' studium_studienzahl_einstieg_gender Server Functions
#'
#' @noRd
mod_studium_studienzahl_einstieg_gender_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$gen_y, {
      r$gen_y <- input$gen_y
    })


    observeEvent(input$gen_l, {
      r$gen_l <- input$gen_l
    })

    observeEvent(input$hochschulform_studierende_einstieg_1_gender, {
      r$hochschulform_studierende_einstieg_1_gender <- input$hochschulform_studierende_einstieg_1_gender
    })


    observeEvent(input$hochschulform_studierende_einstieg_2_gender, {
      r$hochschulform_studierende_einstieg_2_gender <- input$hochschulform_studierende_einstieg_2_gender
    })

  })
}

## To be copied in the UI
# mod_studium_studienzahl_einstieg_gender_ui("studium_studienzahl_einstieg_gender_1")

## To be copied in the server
# mod_studium_studienzahl_einstieg_gender_server("studium_studienzahl_einstieg_gender_1")
