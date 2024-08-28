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
    p("Betrachtungsart:"),
    shiny::radioButtons(
      inputId = ns("ansicht_gen_mint"),
      label = NULL,
      choices = c("Einzelansicht - Kuchendiagramm", "Gruppenvergleich - Balkendiagramm"),
      selected = "Einzelansicht - Kuchendiagramm"
    ),
    p("Jahr:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("gen_y"),
      label = NULL,
      choices = 2013:2022,
      selected = 2022
    ),
    p("Studierendengruppe (max. 3):"),
    shinyWidgets::pickerInput(
      inputId = ns("gen_l"),
      choices = c(
                  "Studienanfänger:innen (1. Fachsemester)",
                  "Studierende",
                  "Studierende (Lehramt)"
      ),
      selected = c("Studierende", "Studienanfänger:innen (1.Fachsemester)"),
      multiple = TRUE,
      options =  list(
        "max-options" = 3,
        "max-options-text" = "Maximal 3 Indikatoren auswählen")
    ),
    #Conditional Panel, um für Lehramt nur sinnvollere Fächer auswählen zu lassen
    p("Fächergruppe:"),
    conditionalPanel(condition = "input.gen_l == 'Studierende (Lehramt)'",
                     ns = ns,
                     shinyWidgets::pickerInput(
                       inputId = ns("gen_f_lehr"),
                       choices = studi_det_ui_faecher(spezif_i ='Studierende (Lehramt)'),

                       selected = "Alle MINT-Fächer"
                     )),

    conditionalPanel(condition = "input.gen_l == 'Internationale Studienanfänger:innen (1. Hochschulsemester)' |
                     input.gen_l == 'Studienanfänger:innen (1. Fachsemester)' |
                     input.gen_l == 'Studierende' |
                     input.gen_l == 'Internationale Studierende' |
                     input.gen_l == 'Studienanfänger:innen (1. Hochschulsemester)'",
                     ns = ns,
                     shinyWidgets::pickerInput(
                       inputId = ns("gen_f_alle"),

                       choices = studi_det_ui_faecher(spezif_i =c('Internationale Studienanfänger:innen (1. Hochschulsemester)',
                                                                  'Studienanfänger:innen (1. Fachsemester)',
                                                                  'Studierende',
                                                                  'Internationale Studierende',
                                                                  'Studienanfänger:innen (1. Hochschulsemester)')),

                       selected = "Alle MINT-Fächer"
                     )),
    p("Region:"),
    shinyWidgets::pickerInput(
      inputId = ns("gen_region_mint"),
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
                  "Thüringen",
                  "Westdeutschland (o. Berlin)",
                  "Ostdeutschland (inkl. Berlin)"
      ),
      multiple = FALSE,
      selected = c("Deutschland")
    ),
    br(),
    p("Nicht-MINT als Vergleich anzeigen?", style = "color: #b16fab;"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("gen_gegenwert"),
      choices = c("Ja", "Nein"),
      selected = "Nein",
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
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

    observeEvent(input$ansicht_gen_mint, {
      r$ansicht_gen_mint <- input$ansicht_gen_mint
    })
    observeEvent(input$gen_y, {
      r$gen_y <- input$gen_y
    })
    observeEvent(input$gen_l, {
      r$gen_l <- input$gen_l
    })
    observeEvent(input$gen_f_lehr, {
      r$gen_f_lehr <- input$gen_f_lehr
    })
    observeEvent(input$gen_f_alle, {
      r$gen_f_alle <- input$gen_f_alle
    })
    observeEvent(input$gen_region_mint, {
      r$gen_region_mint <- input$gen_region_mint
    })
    observeEvent(input$gen_gegenwert, {
      r$gen_gegenwert <- input$gen_gegenwert
    })
  })
}

## To be copied in the UI
# mod_studium_studienzahl_einstieg_gender_ui("studium_studienzahl_einstieg_gender_1")

## To be copied in the server
# mod_studium_studienzahl_einstieg_gender_server("studium_studienzahl_einstieg_gender_1")
