#' studium_studienzahl_bundeslandvergleich UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_studium_studienzahl_bulas_faecher_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Darstellungsart:"),
    shiny::radioButtons(
      inputId = ns("ansicht_studium_bulas_faecher"),
      label = NULL,
      choices = c("Übersicht - Kartendiagramm", "Zeitverlauf - Liniendiagramm",
                  "Gruppenvergleich - Balkendiagramm"),
      selected = "Übersicht - Kartendiagramm"
    ),

    conditionalPanel(condition = "input.ansicht_studium_bulas_faecher == 'Übersicht - Kartendiagramm'",
                     ns = ns,

                     p("Jahr:"),
                     shinyWidgets::sliderTextInput(
                       inputId = ns("bulas_map_y_faecher"),
                       label = NULL,
                       choices = 2013:2022,
                       selected = 2022
                     ),

                     p("Studierendengruppen:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("bulas_map_l_faecher"),
                       choices = c("internationale Studienanfänger:innen (1. Hochschulsemester)",
                                   "Studienanfänger:innen (1. Fachsemester)",
                                   "Studierende",
                                   "internationale Studierende",
                                   "Studierende (Lehramt)"

                       ),
                       selected = c("Studierende")
                       ,
                       multiple = F,
                       options =  list(
                         "max-options" = 2,
                         "max-options-text" = "Maximal 2 Gruppen auswählen")
                     ),
                     #Conditional Panel, um für Lehramt nur sinnvollere Fächer auswählen zu lassen
                     p("Fächergruppe:"),
                     conditionalPanel(condition = "input.bulas_map_l_faecher == 'Studierende (Lehramt)'",
                                      ns = ns,
                                      shinyWidgets::pickerInput(
                                        inputId = ns("bl_f_lehr_faecher"),
                                        choices = studi_det_ui_faecher(spezif_i ='Studierende (Lehramt)'),

                                        selected = "Alle MINT-Fächer"
                                      )),

                     conditionalPanel(condition = "input.bulas_map_l_faecher == 'Internationale Studienanfänger:innen (1. Hochschulsemester)' |
                     input.bulas_map_l_faecher == 'Studienanfänger:innen (1. Fachsemester)' |
                     input.bulas_map_l_faecher == 'Studierende' |
                     input.bulas_map_l_faecher == 'Internationale Studierende' |
                     input.bulas_map_l_faecher == 'Studienanfänger:innen (1. Hochschulsemester)'",
                                      ns = ns,
                                      shinyWidgets::pickerInput(
                                        inputId = ns("bl_f_alle_faecher"),

                                        choices = studi_det_ui_faecher(spezif_i =c('Internationale Studienanfänger:innen (1. Hochschulsemester)',
                                                                                   'Studienanfänger:innen (1. Fachsemester)',
                                                                                   'Studierende',
                                                                                   'Internationale Studierende',
                                                                                   'Studienanfänger:innen (1. Hochschulsemester)')),

                                        selected = "Alle MINT-Fächer"
                                      )),
                     br(),
                     shinyBS::bsPopover(id="dh_studium_fach_2b", title = "",
                                        content = paste0("Falls die Grafiken abgeschnitten dargestellt werden, bitte das gesamte Ansichtsfenster einmal verkleinern und dann wieder maximieren. Dann stellt sich das Seitenverhältnis des Desktops richtig ein."),
                                        placement = "top",
                                        trigger = "hover"),
                     tags$a(paste0("Probleme bei der Darstellung"), icon("question-circle"), id = "dh_studium_fach_2b"),
                     br(),
                     br(),
                     shinyBS::bsPopover(id="ih_studium_fach_2neub", title="",
                                        content = paste0("Die Karte in der ersten Einstellung zeigt: Während Sachsen mit über 41 % MINT-Studierende den höchsten MINT-Anteil im Bundeslandvergleich hat, studieren im Nachbarland Thüringen nur 24 % aller Studierenden ein MINT-Fach."),
                                        trigger = "hover"),
                     tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_studium_fach_2neub")
    ),

    conditionalPanel(condition = "input.ansicht_studium_bulas_faecher == 'Zeitverlauf - Liniendiagramm'",
                     ns = ns,
                     p("Jahre:"),
                     shinyWidgets::sliderTextInput(
                       inputId = ns("bulas_verlauf_y_faecher"),
                       label = NULL,
                       choices = 2013:2022,
                       selected = c(2015, 2022)
                     ),

                     p("Studierendengruppen:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("bulas_verlauf_l_faecher"),
                       choices = c("internationale Studienanfänger:innen (1. Hochschulsemester)",
                                   "Studienanfänger:innen (1. Fachsemester)",
                                   "Studierende",
                                   "internationale Studierende",
                                   "Studierende (Lehramt)"
                       ),
                       selected = c("Studierende")
                       ,
                       multiple = F,
                       options =  list(
                         "max-options" = 3,
                         "max-options-text" = "Maximal 3 Gruppen auswählen")
                     ),
                     #Conditional Panel, um für Lehramt nur sinnvollere Fächer auswählen zu lassen
                     p("Fächergruppe:"),
                     conditionalPanel(condition = "input.bulas_verlauf_l_faecher == 'Studierende (Lehramt)'",
                                      ns = ns,
                                      shinyWidgets::pickerInput(
                                        inputId = ns("bl_verlauf_lehr_faecher"),
                                        choices = studi_det_ui_faecher(spezif_i ='Studierende (Lehramt)'),

                                        selected = "Alle MINT-Fächer"
                                      )),

                     conditionalPanel(condition = "input.bulas_verlauf_l_faecher == 'Internationale Studienanfänger:innen (1. Hochschulsemester)' |
                     input.bulas_verlauf_l_faecher == 'Studienanfänger:innen (1. Fachsemester)' |
                     input.bulas_verlauf_l_faecher == 'Studierende' |
                     input.bulas_verlauf_l_faecher == 'Internationale Studierende' |
                     input.bulas_verlauf_l_faecher == 'Studienanfänger:innen (1. Hochschulsemester)'",
                                      ns = ns,
                                      shinyWidgets::pickerInput(
                                        inputId = ns("bl_verlauf_alle_faecher"),

                                        choices = studi_det_ui_faecher(spezif_i =c('Internationale Studienanfänger:innen (1. Hochschulsemester)',
                                                                                   'Studienanfänger:innen (1. Fachsemester)',
                                                                                   'Studierende',
                                                                                   'Internationale Studierende',
                                                                                   'Studienanfänger:innen (1. Hochschulsemester)')),

                                        selected = "Alle MINT-Fächer"
                                      )),

                     p("Regionen:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("bulas_verlauf_regio_faecher"),
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
                       selected = c("Baden-Württemberg", "Hamburg"),
                       options = list(`actions-box` = TRUE,
                                      `deselect-all-text` = "Alle abwählen",
                                      `select-all-text` = "Alle auswählen"),
                       multiple = TRUE
                     ),
                     p("Betrachtung:"),
                     shinyWidgets::radioGroupButtons(
                       inputId = ns("bulas_verlauf_abs_rel_faecher"),
                       choices = c("In Prozent", "Anzahl"),
                       justified = TRUE,
                       checkIcon = list(yes = icon("ok",
                                                   lib = "glyphicon"))
                     ),
                     br(),
                     shinyBS::bsPopover(id="ih_studium_mint_5b", title="",
                                        content = paste0("Die erste Ansicht zeigt, dass in Baden-Württember im Vergleich zu Hamburg ein größerer Anteil an Studierenden MINT-Fächer studiert. In beiden Bundesländern bleibt der Anteil an MINT-Studierenden über die Jahre relativ konstat und sinkt in den letzten Jahren leicht ab."),
                                        placement = "top",
                                        trigger = "hover"),
                     tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_studium_mint_5b")

    ),


    conditionalPanel(condition = "input.ansicht_studium_bulas_faecher == 'Gruppenvergleich - Balkendiagramm'",
                     ns = ns,

                     p("Jahr:"),
                     shinyWidgets::sliderTextInput(
                       inputId = ns("bulas_balken_date_faecher"),
                       label = NULL,
                       choices = 2013:2022,
                       selected = 2022
                     ),
                     p("Studierendengruppen:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("bulas_balken_l_faecher"),
                       choices = c("internationale Studienanfänger:innen (1. Hochschulsemester)",
                                   "Studienanfänger:innen (1. Fachsemester)",
                                   "Studierende",
                                   "internationale Studierende",
                                   "Studienanfänger:innen (1. Hochschulsemester)",
                                   "Studierende (Lehramt)"
                       ),
                       selected = "Studierende"),

                     #Conditional Panel, um für Lehramt nur sinnvollere Fächer auswählen zu lassen
                     p("Fächergruppe:"),
                     conditionalPanel(condition = "input.bulas_balken_l_faecher == 'Studierende (Lehramt)'",
                                      ns = ns,
                                      shinyWidgets::pickerInput(
                                        inputId = ns("bl_balken_lehr_faecher"),
                                        choices = studi_det_ui_faecher(spezif_i ='Studierende (Lehramt)'),

                                        selected = "Alle MINT-Fächer"
                                      )),

                     conditionalPanel(condition = "input.bulas_balken_l_faecher == 'Internationale Studienanfänger:innen (1. Hochschulsemester)' |
                     input.bulas_balken_l_faecher == 'Studienanfänger:innen (1. Fachsemester)' |
                     input.bulas_balken_l_faecher == 'Studierende' |
                     input.bulas_balken_l_faecher == 'Internationale Studierende' |
                     input.bulas_balken_l_faecher == 'Studienanfänger:innen (1. Hochschulsemester)'",
                                      ns = ns,
                                      shinyWidgets::pickerInput(
                                        inputId = ns("bl_balken_alle_faecher"),

                                        choices = studi_det_ui_faecher(spezif_i =c('Internationale Studienanfänger:innen (1. Hochschulsemester)',
                                                                                   'Studienanfänger:innen (1. Fachsemester)',
                                                                                   'Studierende',
                                                                                   'Internationale Studierende',
                                                                                   'Studienanfänger:innen (1. Hochschulsemester)')),

                                        selected = "Alle MINT-Fächer"
                                      )),

                     p("Regionen:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("bulas_balken_regio_faecher"),
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
                                   "Ostdeutschland (inkl. Berlin)"),
                       selected = c("Deutschland",
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
                                    "Ostdeutschland (inkl. Berlin)"),
                       options = list(`actions-box` = TRUE,
                                      `deselect-all-text` = "Alle abwählen",
                                      `select-all-text` = "Alle auswählen"),
                       multiple = TRUE
                     ),

                     br(),
                     shinyBS::bsPopover(id="ih_studium_fach_5b", title="",
                                        content = paste0("Die Übersicht zeigt, dass der Anteil von Studierenden in MINT an allen Studierenden zwischen den Bundesländern zwischen 24 % (Thüringen, Saarland) und 42 % (Sachsen) liegt."),
                                        placement = "top",
                                        trigger = "hover"),
                     tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_studium_fach_5b")
    )
  )

}

#' studium_studienzahl_bundeslandvergleich Server Functions
#'
#' @noRd
mod_studium_studienzahl_bulas_faecher_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$ansicht_studium_bulas_faecher, {
      r$ansicht_studium_bulas_faecher <- input$ansicht_studium_bulas_faecher
    })

    observeEvent(input$bulas_map_y_faecher, {
      r$bulas_map_y_faecher <- input$bulas_map_y_faecher
    })

    observeEvent(input$bulas_map_l_faecher, {
      r$bulas_map_l_faecher <- input$bulas_map_l_faecher
    })

    observeEvent(input$bulas_verlauf_y_faecher, {
      r$bulas_verlauf_y_faecher <- input$bulas_verlauf_y_faecher
    })

    observeEvent(input$bulas_verlauf_l_faecher, {
      r$bulas_verlauf_l_faecher <- input$bulas_verlauf_l_faecher
    })

    observeEvent(input$bulas_verlauf_regio_faecher, {
      r$bulas_verlauf_regio_faecher <- input$bulas_verlauf_regio_faecher
    })

    observeEvent(input$bulas_verlauf_abs_rel_faecher, {
      r$bulas_verlauf_abs_rel_faecher <- input$bulas_verlauf_abs_rel_faecher
    })

    observeEvent(input$bulas_balken_date_faecher, {
      r$bulas_balken_date_faecher <- input$bulas_balken_date_faecher
    })

    observeEvent(input$bulas_balken_l_faecher, {
      r$bulas_balken_l_faecher <- input$bulas_balken_l_faecher
    })

    observeEvent(input$bl_f_alle_faecher, {
      r$bl_f_alle_faecher <- input$bl_f_alle_faecher
    })

    observeEvent(input$bl_f_lehr_faecher, {
      r$bl_f_lehr_faecher <- input$bl_f_lehr_faecher
    })

    observeEvent(input$bl_verlauf_alle_faecher, {
      r$bl_verlauf_alle_faecher <- input$bl_verlauf_alle_faecher
    })

    observeEvent(input$bl_verlauf_lehr_faecher, {
      r$bl_verlauf_lehr_faecher <- input$bl_verlauf_lehr_faecher
    })

    observeEvent(input$bl_balken_alle_faecher, {
      r$bl_balken_alle_faecher <- input$bl_balken_alle_faecher
    })

    observeEvent(input$bl_balken_lehr_faecher, {
      r$bl_balken_lehr_faecher <- input$bl_balken_lehr_faecher
    })

    observeEvent(input$bulas_balken_regio_faecher, {
      r$bulas_balken_regio_faecher <- input$bulas_balken_regio_faecher
    })

  })
}

## To be copied in the UI
# mod_studium_studienzahl_bl_map_ui("studium_studienzahl_bl_map_1")

## To be copied in the server
# mod_studium_studienzahl_bl_map_server("studium_studienzahl_bl_map_1")
