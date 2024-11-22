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
    p("Darstellungsart:"),
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
      choices = 2013:2023,
      selected = 2023
    ),

    conditionalPanel(condition = "input.ansicht_gen_mint == 'Einzelansicht - Kuchendiagramm'",
                     ns = ns,

        p("Studierendengruppe (max. 2):"),
        shinyWidgets::pickerInput(
          inputId = ns("gen_l"),
          choices = c("Studierende",
                      "Studierende (Lehramt)",
                      "Studienanfänger:innen (1. Hochschulsemester)",
                      "Absolvent:innen"

          ),
          selected = "Studierende",
          multiple = TRUE,
          options =  list(
            "max-options" = 2,
            "max-options-text" = "<span style='color: red;'>Maximal 2 Studierendengruppen auswählen</span>")
        ),

        p("Fächergruppe:"),
        shinyWidgets::pickerInput(
          inputId = ns("gen_f"),
          choices = studi_det_ui_faecher(),

          selected = "Alle MINT-Fächer"
        ),
        #Conditional Panel, um für Lehramt nur sinnvollere Fächer auswählen zu lassen
        # p("Fächergruppe:"),
        # conditionalPanel(condition = sprintf("input['%s'] == 'Studierende (Lehramt)'",
        #                                      ns("gen_l")),
        #                  ns = ns,
        #                  p(ns("gen_l")),
        #                  shinyWidgets::pickerInput(
        #                    inputId = ns("gen_f_lehr"),
        #                    choices = studi_det_ui_faecher(spezif_i ='Studierende (Lehramt)'),
        #
        #                    selected = "Alle MINT-Fächer"
        #                  )),
        #
        # conditionalPanel(condition = sprintf("input['%s'] == 'Internationale Studienanfänger:innen (1. Hochschulsemester)' ||
        #                  input['%s'] == 'Studienanfänger:innen (1. Fachsemester)' ||
        #                  input['%s'] == 'Studierende' ||
        #                  input['%s'] == 'Internationale Studierende' ||
        #                  input['%s'] == 'Studienanfänger:innen (1. Hochschulsemester)'",
        #                                      ns("gen_l"), ns("gen_l"), ns("gen_l"),
        #                                      ns("gen_l"),ns("gen_l")),
        #                  ns = ns,
        #                  p(ns("gen_l")),
        #                  shinyWidgets::pickerInput(
        #                    inputId = ns("gen_f_alle"),
        #
        #                    choices = studi_det_ui_faecher(spezif_i =c('Internationale Studienanfänger:innen (1. Hochschulsemester)',
        #                                                               'Studienanfänger:innen (1. Fachsemester)',
        #                                                               'Studierende',
        #                                                               'Internationale Studierende',
        #                                                               'Studienanfänger:innen (1. Hochschulsemester)')),
        #
        #                    selected = "Alle MINT-Fächer"
        #                  )),
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
        p("Nicht-MINT als Vergleich anzeigen?", style = "color: #b16fab;"),
        shinyWidgets::radioGroupButtons(
          inputId = ns("gen_gegenwert_pie"),
          choices = c("Ja", "Nein"),
          selected = "Nein",
          justified = TRUE,
          checkIcon = list(yes = icon("ok",
                                      lib = "glyphicon"))
        ),
        br(),
        shinyBS::bsPopover(id="dh_studium_frauen_1", title = "",
                           content = paste0("Falls die Grafiken abgeschnitten dargestellt werden, bitte das gesamte Ansichtsfenster einmal verkleinern und dann wieder maximieren. Dann stellt sich das Seitenverhältnis des Desktops richtig ein."),
                           trigger = "hover", placement = "top"),
        tags$a(paste0("Probleme bei der Darstellung"), icon("question-circle"), id = "dh_studium_frauen_1"),
        br(),
        br(),
        shinyBS::bsPopover(id="ih_studium_frauen_1", title="",
                           content = paste0("In der ersten interaktiven Grafik ist zu sehen, dass deutschlandweit 2023 der Anteil von Frauen unter den Studierenden in MINT-Fächern 32,6% ausmacht."),
                           trigger = "hover", placement = "top"),
        tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_studium_frauen_1")
      ),

    conditionalPanel(condition = "input.ansicht_gen_mint == 'Gruppenvergleich - Balkendiagramm'",
                     ns = ns,
                     p("Region:"),

                     shinyWidgets::pickerInput(
                       inputId = ns("gen_states_faecher"),
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
                                   "Thüringen"
                                   ,
                                   "Westdeutschland (o. Berlin)",
                                   "Ostdeutschland (inkl. Berlin)"
                       ),
                       selected = "Deutschland"
                     ),
                     p("Fach/Fächergruppe:"),

                     # #Conditonal Panel, dass keine leeren Plots kommen
                     conditionalPanel(condition = "(input.ansicht_gen_mint == 'Gruppenvergleich - Balkendiagramm' &
                                      input.gen_states_faecher == 'Deutschland') ||
                      (input.ansicht_gen_mint == 'Gruppenvergleich - Balkendiagramm' &
                                      input.gen_states_faecher == 'Baden-Württemberg') ||
                      (input.ansicht_gen_mint == 'Gruppenvergleich - Balkendiagramm' &
                                      input.gen_states_faecher == 'Bayern') ||
                      (input.ansicht_gen_mint == 'Gruppenvergleich - Balkendiagramm' &
                                      input.gen_states_faecher == 'Berlin' )||
                      (input.ansicht_gen_mint == 'Gruppenvergleich - Balkendiagramm' &
                                      input.gen_states_faecher == 'Hamburg') ||
                      (input.ansicht_gen_mint == 'Gruppenvergleich - Balkendiagramm' &
                                      input.gen_states_faecher =='Hessen' )||
                      (input.ansicht_gen_mint == 'Gruppenvergleich - Balkendiagramm' &
                                      input.gen_states_faecher == 'Nordrhein-Westfalen') ||
                     (input.ansicht_gen_mint == 'Gruppenvergleich - Balkendiagramm' &
                                      input.gen_states_faecher == 'Rheinland-Pfalz' )||
                     (input.ansicht_gen_mint == 'Gruppenvergleich - Balkendiagramm' &
                                      input.gen_states_faecher == 'Sachsen') ||
                     (input.ansicht_gen_mint == 'Gruppenvergleich - Balkendiagramm' &
                                      input.gen_states_faecher == 'Westdeutschland (o. Berlin)') ||
                     (input.ansicht_gen_mint == 'Gruppenvergleich - Balkendiagramm' &
                                      input.gen_states_faecher == 'Ostdeutschland (inkl. Berlin)')",
                                      ns = ns,
                                      shinyWidgets::pickerInput(
                                        inputId = ns("gen1_f"),

                                        choices = studi_det_ui_faecher(spezif_r = c('Deutschland', 'Baden-Württemberg',
                                                                                    'Bayern', 'Berlin','Hamburg', 'Hessen',
                                                                                    'Nordrhein-Westfalen', 'Rheinland-Pfalz','Sachsen',
                                                                                    'Westdeutschland (o. Berlin)', 'Ostdeutschland (inkl. Berlin)' )),
                                        selected = "Alle MINT-Fächer",
                                        multiple = FALSE
                                      )),
                     conditionalPanel(condition = "input.ansicht_gen_mint == 'Gruppenvergleich - Balkendiagramm' &
                                      input.gen_states_faecher == 'Brandenburg'",
                                      ns = ns,
                                      shinyWidgets::pickerInput(
                                        inputId = ns("gen2_f"),
                                        choices = studi_det_ui_faecher(spezif_r ='Brandenburg'),
                                        selected = "Alle MINT-Fächer",
                                        multiple = FALSE
                                      )),
                     conditionalPanel(condition = "input.ansicht_gen_mint == 'Gruppenvergleich - Balkendiagramm' &
                                      input.gen_states_faecher == 'Bremen'",
                                      ns = ns,
                                      shinyWidgets::pickerInput(
                                        inputId = ns("gen3_f"),
                                        choices = studi_det_ui_faecher(spezif_r ='Bremen'),
                                        selected = "Alle MINT-Fächer",
                                        multiple = FALSE
                                      )),
                     conditionalPanel(condition = "input.ansicht_gen_mint == 'Gruppenvergleich - Balkendiagramm' &
                                      input.gen_states_faecher == 'Mecklenburg-Vorpommern'",
                                      ns = ns,
                                      shinyWidgets::pickerInput(
                                        inputId = ns("gen4_f"),
                                        choices = studi_det_ui_faecher(spezif_r ='Mecklenburg-Vorpommern'),
                                        selected = "Alle MINT-Fächer",
                                        multiple = FALSE
                                      )),
                     conditionalPanel(condition = "input.ansicht_gen_mint == 'Gruppenvergleich - Balkendiagramm' &
                                      input.gen_states_faecher == 'Niedersachsen'",
                                      ns = ns,
                                      shinyWidgets::pickerInput(
                                        inputId = ns("gen5_f"),
                                        choices = studi_det_ui_faecher(spezif_r ='Niedersachsen'),
                                        selected = "Alle MINT-Fächer",
                                        multiple = FALSE
                                      )),
                     conditionalPanel(condition = "input.ansicht_gen_mint == 'Gruppenvergleich - Balkendiagramm' &
                                      input.gen_states_faecher == 'Saarland'",
                                      ns = ns,
                                      shinyWidgets::pickerInput(
                                        inputId = ns("gen6_f"),
                                        choices = studi_det_ui_faecher(spezif_r ='Saarland'),
                                        selected = "Alle MINT-Fächer",
                                        multiple = FALSE
                                      )),
                     conditionalPanel(condition = "input.ansicht_gen_mint == 'Gruppenvergleich - Balkendiagramm' &
                                      input.gen_states_faecher == 'Sachsen-Anhalt'",
                                      ns = ns,
                                      shinyWidgets::pickerInput(
                                        inputId = ns("gen7_f"),
                                        choices = studi_det_ui_faecher(spezif_r ='Sachsen-Anhalt'),
                                        selected = "Alle MINT-Fächer",
                                        multiple = FALSE
                                      )),
                     conditionalPanel(condition = "input.ansicht_gen_mint == 'Gruppenvergleich - Balkendiagramm' &
                                      input.gen_states_faecher =='Schleswig-Holstein'",
                                      ns = ns,
                                      shinyWidgets::pickerInput(
                                        inputId = ns("gen8_f"),
                                        choices = studi_det_ui_faecher(spezif_r ='Schleswig-Holstein'),
                                        selected = "Alle MINT-Fächer",
                                        multiple = FALSE
                                      )),
                     conditionalPanel(condition = "input.ansicht_gen_mint == 'Gruppenvergleich - Balkendiagramm' &
                                      input.gen_states_faecher == 'Thüringen'",
                                      ns = ns,
                                      shinyWidgets::pickerInput(
                                        inputId = ns("gen9_f"),
                                        choices = studi_det_ui_faecher(spezif_r ='Thüringen'),
                                        selected = "Alle MINT-Fächer",
                                        multiple = FALSE
                                      )),
    p("Nicht-MINT als Vergleich anzeigen?", style = "color: #b16fab;"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("gen_gegenwert_balken"),
      choices = c("Ja", "Nein"),
      selected = "Nein",
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
    ),
    br(),
                     shinyBS::bsPopover(id="ih_studium_frauen_3", title="",
                                        content = paste0("Die Übersicht zeigt unter anderem, dass deutschlandweit 2023 der Frauenanteil von Lehramtstudierenden mit einem Hauptfach in MINT bei 54% liegt. Dagegen sind nur ein Drittel der allgemeinen MINT-Studierenden weiblich."),
                                        placement = "top",
                                        trigger = "hover"),
                     tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_studium_frauen_3")
                     )
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
    observeEvent(input$gen_f, {
      r$gen_f <- input$gen_f
    })
    # observeEvent(input$gen_f_lehr, {
    #   r$gen_f_lehr <- input$gen_f_lehr
    # })
    # observeEvent(input$gen_f_alle, {
    #   r$gen_f_alle <- input$gen_f_alle
    # })
    observeEvent(input$gen_region_mint, {
      r$gen_region_mint <- input$gen_region_mint
    })
    observeEvent(input$gen_gegenwert_pie, {
      r$gen_gegenwert_pie <- input$gen_gegenwert_pie
    })

    observeEvent(input$gen_gegenwert_balken, {
      r$gen_gegenwert_balken <- input$gen_gegenwert_balken
    })

    observeEvent(input$gen_states_faecher, {
      r$gen_states_faecher <- input$gen_states_faecher
    })

    observeEvent(input$gen1_f, {
      r$gen1_f <- input$gen1_f
    })

    observeEvent(input$gen2_f, {
      r$gen2_f <- input$gen2_f
    })
    observeEvent(input$gen3_f, {
      r$gen3_f <- input$gen3_f
    })

    observeEvent(input$gen4_f, {
      r$gen4_f <- input$gen4_f
    })
    observeEvent(input$gen5_f, {
      r$gen5_f <- input$gen5_f
    })

    observeEvent(input$gen6_f, {
      r$gen6_f <- input$gen6_f
    })
    observeEvent(input$gen7_f, {
      r$gen7_f <- input$gen7_f
    })

    observeEvent(input$gen8_f, {
      r$gen8_f <- input$gen8_f
    })

    observeEvent(input$gen9_f, {
      r$gen9_f <- input$gen9_f
    })

  })
}

## To be copied in the UI
# mod_studium_studienzahl_einstieg_gender_ui("studium_studienzahl_einstieg_gender_1")

## To be copied in the server
# mod_studium_studienzahl_einstieg_gender_server("studium_studienzahl_einstieg_gender_1")
