#' studium_studienzahl_bl_map_gender UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_studium_studienzahl_ausl_zeit_ui <- function(id){
  ns <- NS(id)
  tagList(

    p("Darstellungsart:"),
    shiny::radioButtons(
      inputId = ns("ansicht_ausl_zeit"),
      label = NULL,
      choices = c("Gruppenvergleich - Balkendiagramm", "Zeitverlauf - Liniendiagramm"),
      selected = "Gruppenvergleich - Balkendiagramm"
    ),
    p("Jahr:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_ausl_zeit"),
      label = NULL,
      choices = 2013:2023,
      selected = c(2017, 2023)
    ),
    p("Region:"),
    shinyWidgets::pickerInput(
      inputId = ns("states_studium_studienzahl_ausl_zeit"),
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
                  "Ostdeutschland (inkl. Berlin)"),
      selected = "Deutschland"
    ),
    p("Fach/Fächergruppe:"),

    #Conditonal Panel, dass keine leeren Plots kommen
    conditionalPanel(condition = "input.states_studium_studienzahl_ausl_zeit == 'Deutschland' |
      input.states_studium_studienzahl_ausl_zeit == 'Baden-Württemberg' |
      input.states_studium_studienzahl_ausl_zeit == 'Bayern' |
      input.states_studium_studienzahl_ausl_zeit == 'Berlin' |
      input.states_studium_studienzahl_ausl_zeit == 'Hamburg' |
      input.states_studium_studienzahl_ausl_zeit == 'Hessen' |
      input.states_studium_studienzahl_ausl_zeit == 'Nordrhein-Westfalen' |
                     input.states_studium_studienzahl_ausl_zeit == 'Rheinland-Pfalz' |
                     input.states_studium_studienzahl_ausl_zeit == 'Sachsen' |
                     input.states_studium_studienzahl_ausl_zeit == 'Westdeutschland (o. Berlin)' |
                     input.states_studium_studienzahl_ausl_zeit == 'Ostdeutschland (inkl. Berlin)'",
                     ns = ns,
                     shinyWidgets::pickerInput(
                       inputId = ns("fach1_studium_studienzahl_ausl_zeit"),

                       choices = studi_det_ui_faecher(spezif_r=c('Deutschland','Baden-Württemberg','Bayern','Berlin',
                                                                'Hamburg', 'Hessen','Nordrhein-Westfalen', 'Rheinland-Pfalz',
                                                                'Sachsen', 'Westdeutschland (o. Berlin)', 'Ostdeutschland (inkl. Berlin)' )),
                       selected = "Alle MINT-Fächer",
                       multiple = FALSE
                     )),
    conditionalPanel(condition = "input.states_studium_studienzahl_ausl_zeit == 'Brandenburg'",
                     ns = ns,
                     shinyWidgets::pickerInput(
                       inputId = ns("fach2_studium_studienzahl_ausl_zeit"),

                       choices = studi_det_ui_faecher(spezif_r='Brandenburg'),
                       selected = "Alle MINT-Fächer",
                       multiple = FALSE
                     )),
    conditionalPanel(condition = "input.states_studium_studienzahl_ausl_zeit == 'Bremen'",
                     ns = ns,
                     shinyWidgets::pickerInput(
                       inputId = ns("fach3_studium_studienzahl_ausl_zeit"),

                       choices = studi_det_ui_faecher(spezif_r='Bremen'),
                       selected = "Alle MINT-Fächer",
                       multiple = FALSE
                     )),
    conditionalPanel(condition = "input.states_studium_studienzahl_ausl_zeit == 'Mecklenburg-Vorpommern'",
                     ns = ns,
                     shinyWidgets::pickerInput(
                       inputId = ns("fach4_studium_studienzahl_ausl_zeit"),

                       choices = studi_det_ui_faecher(spezif_r='Mecklenburg-Vorpommern'),
                       selected = "Alle MINT-Fächer",
                       multiple = FALSE
                     )),
    conditionalPanel(condition = "input.states_studium_studienzahl_ausl_zeit == 'Niedersachsen'",
                     ns = ns,
                     shinyWidgets::pickerInput(
                       inputId = ns("fach5_studium_studienzahl_ausl_zeit"),

                       choices = studi_det_ui_faecher(spezif_r='Niedersachsen'),
                       selected = "Alle MINT-Fächer",
                       multiple = FALSE
                     )),
    conditionalPanel(condition = "input.states_studium_studienzahl_ausl_zeit == 'Saarland'",
                     ns = ns,
                     shinyWidgets::pickerInput(
                       inputId = ns("fach6_studium_studienzahl_ausl_zeit"),

                       choices = studi_det_ui_faecher(spezif_r='Saarland'),
                       selected = "Alle MINT-Fächer",
                       multiple = FALSE
                     )),


    conditionalPanel(condition = "input.states_studium_studienzahl_ausl_zeit == 'Sachsen-Anhalt'",
                     ns = ns,
                     shinyWidgets::pickerInput(
                       inputId = ns("fach7_studium_studienzahl_ausl_zeit"),

                       choices = studi_det_ui_faecher(spezif_r='Sachsen-Anhalt'),
                       selected = "Alle MINT-Fächer",
                       multiple = FALSE
                     )),
    conditionalPanel(condition = "input.states_studium_studienzahl_ausl_zeit == 'Schleswig-Holstein'",
                     ns = ns,
                     shinyWidgets::pickerInput(
                       inputId = ns("fach8_studium_studienzahl_ausl_zeit"),

                       choices = studi_det_ui_faecher(spezif_r='Schleswig-Holstein'),
                       selected = "Alle MINT-Fächer",
                       multiple = FALSE
                     )),
    conditionalPanel(condition = "input.states_studium_studienzahl_ausl_zeit == 'Thüringen'",
                     ns = ns,
                     shinyWidgets::pickerInput(
                       inputId = ns("fach9_studium_studienzahl_ausl_zeit"),

                       choices =studi_det_ui_faecher(spezif_r='Thüringen'),
                       selected = "Alle MINT-Fächer",
                       multiple = FALSE
                     )),

    p("Studierendengruppen:"),
    shinyWidgets::pickerInput(
      inputId = ns("status_ausl_zeit"),
      choices = c("Studierende",
                  "Studienanfänger:innen (1. Hochschulsemester)",
                  "Absolvent:innen"
      ),
      selected = "Studierende"
    ),
    p("Darstellungsart:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("abs_zahlen_studium_studienzahl_ausl_zeit"),
      choices = c("In Prozent", "Anzahl"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))

    ),
    br(),
    shinyBS::bsPopover(id="ih_studium_international_2", title="",
                       content = paste0("Die erste Einstellung der Grafik zeigt, dass der Anteil internationaler Studierender an allen Studierenden in MINT in den letzten 7 Jahren um fast 7 Prozentpunkte angestiegen ist. Betrachtet man die &quotAnzahl&quot, zeigt sich, dass die Anzahl internationaler Studierender von 137.000 in 2017 auf 204.000 in 2023 angestiegen ist."),
                       placement = "top",
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_studium_international_2")
  )
}

#' studium_studienzahl_bl_map_gender Server Functions
#'
#' @noRd
mod_studium_studienzahl_ausl_zeit_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$ansicht_ausl_zeit, {
      r$ansicht_ausl_zeit <- input$ansicht_ausl_zeit
    })

    observeEvent(input$date_ausl_zeit, {
      r$date_ausl_zeit <- input$date_ausl_zeit
    })

    observeEvent(input$states_studium_studienzahl_ausl_zeit, {
      r$states_studium_studienzahl_ausl_zeit <- input$states_studium_studienzahl_ausl_zeit
    })

    observeEvent(input$status_ausl_zeit, {
      r$status_ausl_zeit <- input$status_ausl_zeit
    })

    observeEvent(input$fach1_studium_studienzahl_ausl_zeit, {
      r$fach1_studium_studienzahl_ausl_zeit <- input$fach1_studium_studienzahl_ausl_zeit
    })

    observeEvent(input$fach2_studium_studienzahl_ausl_zeit, {
      r$fach2_studium_studienzahl_ausl_zeit <- input$fach2_studium_studienzahl_ausl_zeit
    })
    observeEvent(input$fach3_studium_studienzahl_ausl_zeit, {
      r$fach3_studium_studienzahl_ausl_zeit <- input$fach3_studium_studienzahl_ausl_zeit
    })

    observeEvent(input$fach4_studium_studienzahl_ausl_zeit, {
      r$fach4_studium_studienzahl_ausl_zeit <- input$fach4_studium_studienzahl_ausl_zeit
    })
    observeEvent(input$fach5_studium_studienzahl_ausl_zeit, {
      r$fach5_studium_studienzahl_ausl_zeit <- input$fach5_studium_studienzahl_ausl_zeit
    })

    observeEvent(input$fach6_studium_studienzahl_ausl_zeit, {
      r$fach6_studium_studienzahl_ausl_zeit <- input$fach6_studium_studienzahl_ausl_zeit
    })
    observeEvent(input$fach7_studium_studienzahl_ausl_zeit, {
      r$fach7_studium_studienzahl_ausl_zeit <- input$fach7_studium_studienzahl_ausl_zeit
    })

    observeEvent(input$fach8_studium_studienzahl_ausl_zeit, {
      r$fach8_studium_studienzahl_ausl_zeit <- input$fach8_studium_studienzahl_ausl_zeit
    })

    observeEvent(input$fach9_studium_studienzahl_ausl_zeit, {
      r$fach9_studium_studienzahl_ausl_zeit <- input$fach9_studium_studienzahl_ausl_zeit
    })

    observeEvent(input$abs_zahlen_studium_studienzahl_ausl_zeit, {
      r$abs_zahlen_studium_studienzahl_ausl_zeit <- input$abs_zahlen_studium_studienzahl_ausl_zeit
    })

    # observeEvent(input$status_ausl, {
    #   r$status_ausl <- input$status_ausl
    # })

    # observeEvent(input$subject_studium_studienzahl_bl_gender_map, {
    #   r$subject_studium_studienzahl_bl_gender_map <- input$subject_studium_studienzahl_bl_gender_map
    # })
    #
    # observeEvent(input$level_studium_choice_gender, {
    #   r$level_studium_choice_gender <- input$level_studium_choice_gender
    # })

  })
}

## To be copied in the UI
# mod_studium_studienzahl_bl_map_gender_ui("studium_studienzahl_bl_map_gender_1")

## To be copied in the server
# mod_studium_studienzahl_bl_map_gender_server("studium_studienzahl_bl_map_gender_1")
