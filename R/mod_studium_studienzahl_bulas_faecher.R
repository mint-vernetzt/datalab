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

    tags$head(
      tags$style(HTML("
        .dropdown-menu .bs-actionsbox .btn-group .btn {
          background-color: #e7f1ff !important;  /* Hellblau für die Alle auswählen/abwählen Buttons */
          color: #000000 !important;
        }
        .dropdown-menu .bs-actionsbox .btn-group .btn:hover {
          background-color: #d0e8ff !important;  /* Etwas dunkleres Blau beim Hover */
          color: #000000 !important;
        }
      "))
    ),

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
                       choices = 2013:2023,
                       selected = 2023
                     ),

                     p("Studierendengruppen:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("bulas_map_l_faecher"),
                       choices = c("Studierende",
                                   "Studienanfänger:innen (1. Hochschulsemester)",
                                   "Absolvent:innen",
                                   "internationale Studierende",
                                   "internationale Studienanfänger:innen (1. Hochschulsemester)",
                                   "internationale Absolvent:innen",
                                   "Studierende im Lehramt" = "Studierende (Lehramt)",
                                   "Absolvent:innen im Lehramt" ="Absolvent:innen (Lehramt)"),
                       selected = c("Studierende")
                       ,
                       multiple = F,
                       options =  list(
                         "max-options" = 2,
                         "max-options-text" = "<span style='color: red;'>Maximal 2 Gruppen auswählen</span>")
                     ),
                     #Conditional Panel, um für Lehramt nur sinnvollere Fächer auswählen zu lassen
                     p("Fächergruppe:"),
                     conditionalPanel(condition = "input.bulas_map_l_faecher == 'Studierende (Lehramt)' |
                                      input.bulas_map_l_faecher == 'Absolvent:innen (Lehramt)'",
                                      ns = ns,
                                      shinyWidgets::pickerInput(
                                        inputId = ns("bl_f_lehr_faecher"),
                                        choices = studi_det_ui_faecher(spezif_i =c('Studierende (Lehramt)',
                                                                                   'Absolvent:innen (Lehramt)')),

                                        selected = "Alle MINT-Fächer"
                                      )),

                     conditionalPanel(condition = "input.bulas_map_l_faecher == 'internationale Studienanfänger:innen (1. Hochschulsemester)' |
                     input.bulas_map_l_faecher == 'Studierende' |
                     input.bulas_map_l_faecher == 'Internationale Studierende' |
                     input.bulas_map_l_faecher == 'Studienanfänger:innen (1. Hochschulsemester)'|
                    input.bulas_map_l_faecher == 'Absolvent:innen' |
                     input.bulas_map_l_faecher == 'internationale Absolvent:innen'",
                                      ns = ns,
                                      shinyWidgets::pickerInput(
                                        inputId = ns("bl_f_alle_faecher"),

                                        choices = studi_det_ui_faecher(spezif_i =c('internationale Studienanfänger:innen (1. Hochschulsemester)',
                                                                                   'Studierende',
                                                                                   'internationale Studierende',
                                                                                   'Studienanfänger:innen (1. Hochschulsemester)',
                                                                                   "internationale Absolvent:innen",
                                                                                   "Absolvent:innen")),

                                        selected = "Alle MINT-Fächer"
                                      )),
                     br(),
                     darstellung(id="dh_studium_fach_2b"),
                     br(),
                     br(),
                     shinyBS::bsPopover(id="ih_studium_fach_2neub", title="",
                                        content = paste0("Die Karte in der ersten Einstellung zeigt: Während Sachsen mit über 41 % MINT-Studierende den höchsten MINT-Anteil im Bundeslandvergleich hat, studieren im Nachbarland Thüringen nur 23 % aller Studierenden ein MINT-Fach."),
                                        trigger = "hover"),
                     tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_studium_fach_2neub")
    ),

    conditionalPanel(condition = "input.ansicht_studium_bulas_faecher == 'Zeitverlauf - Liniendiagramm'",
                     ns = ns,
                     p("Jahre:"),
                     shinyWidgets::sliderTextInput(
                       inputId = ns("bulas_verlauf_y_faecher"),
                       label = NULL,
                       choices = 2013:2023,
                       selected = c(2015, 2023)
                     ),

                     p("Studierendengruppen:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("bulas_verlauf_l_faecher"),
                       choices = c("Studierende",
                                   "Studienanfänger:innen (1. Hochschulsemester)",
                                   "Absolvent:innen",
                                   "internationale Studierende",
                                   "internationale Studienanfänger:innen (1. Hochschulsemester)",
                                   "internationale Absolvent:innen",
                                   "Studierende im Lehramt" = "Studierende (Lehramt)",
                                   "Absolvent:innen im Lehramt" ="Absolvent:innen (Lehramt)"),
                       selected = c("Studierende")
                       ,
                       multiple = F,
                       options =  list(
                         "max-options" = 2,
                         "max-options-text" = "<span style='color: red;'>Maximal 2 Gruppen auswählen</span>")
                     ),
                     #Conditional Panel, um für Lehramt nur sinnvollere Fächer auswählen zu lassen
                     p("Fächergruppe:"),
                     conditionalPanel(condition = "input.bulas_verlauf_l_faecher == 'Studierende (Lehramt)' |
                                      input.bulas_verlauf_l_faecher == 'Absolvent:innen (Lehramt)'",
                                      ns = ns,
                                      shinyWidgets::pickerInput(
                                        inputId = ns("bl_verlauf_lehr_faecher"),
                                        choices = studi_det_ui_faecher(spezif_i =c('Studierende (Lehramt)',
                                                                       'Absolvent:innen (Lehramt)')),

                                        selected = "Alle MINT-Fächer"
                                      )),

                     conditionalPanel(condition = "input.bulas_verlauf_l_faecher == 'internationale Studienanfänger:innen (1. Hochschulsemester)' |
                     input.bulas_verlauf_l_faecher == 'Studierende' |
                     input.bulas_verlauf_l_faecher == 'internationale Studierende' |
                     input.bulas_verlauf_l_faecher == 'Studienanfänger:innen (1. Hochschulsemester)'|
                     input.bulas_verlauf_l_faecher == 'Absolvent:innen' |
                     input.bulas_verlauf_l_faecher == 'internationale Absolvent:innen'",
                                      ns = ns,
                                      shinyWidgets::pickerInput(
                                        inputId = ns("bl_verlauf_alle_faecher"),

                                        choices = studi_det_ui_faecher(spezif_i =c('internationale Studienanfänger:innen (1. Hochschulsemester)',
                                                                                   'Studierende',
                                                                                   'internationale Studierende',
                                                                                   'Studienanfänger:innen (1. Hochschulsemester)',
                                                                                   "internationale Absolvent:innen",
                                                                                   "Absolvent:innen")),

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
                     p("Darstellungsart:"),
                     shinyWidgets::radioGroupButtons(
                       inputId = ns("bulas_verlauf_abs_rel_faecher"),
                       choices = c("In Prozent", "Anzahl"),
                       justified = TRUE,
                       checkIcon = list(yes = icon("ok",
                                                   lib = "glyphicon"))
                     ),
                     br(),
                     shinyBS::bsPopover(id="ih_studium_mint_5b", title="",
                                        content = paste0("Die erste Ansicht zeigt, dass in Baden-Württemberg im Vergleich zu Hamburg ein größerer Anteil an Studierenden MINT-Fächer studiert. In beiden Bundesländern bleibt der Anteil an MINT-Studierenden über die Jahre relativ konstant."),
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
                       choices = 2013:2023,
                       selected = 2023
                     ),
                     p("Studierendengruppen:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("bulas_balken_l_faecher"),
                       choices = c("Studierende",
                                   "Studienanfänger:innen (1. Hochschulsemester)",
                                   "Absolvent:innen",
                                   "internationale Studierende",
                                   "internationale Studienanfänger:innen (1. Hochschulsemester)",
                                   "internationale Absolvent:innen",
                                   "Studierende im Lehramt" = "Studierende (Lehramt)",
                                   "Absolvent:innen im Lehramt" ="Absolvent:innen (Lehramt)"),
                       selected = "Studierende"),

                     #Conditional Panel, um für Lehramt nur sinnvollere Fächer auswählen zu lassen
                     p("Fächergruppe:"),
                     conditionalPanel(condition = "input.bulas_balken_l_faecher == 'Studierende (Lehramt)' |
                                      input.bulas_balken_l_faecher == 'Absolvent:innen (Lehramt)'",
                                      ns = ns,
                                      shinyWidgets::pickerInput(
                                        inputId = ns("bl_balken_lehr_faecher"),
                                        choices = studi_det_ui_faecher(spezif_i =c('Studierende (Lehramt)',
                                                                                   'Absolvent:innen (Lehramt)')),

                                        selected = "Alle MINT-Fächer"
                                      )),

                     conditionalPanel(condition = "input.bulas_balken_l_faecher == 'internationale Studienanfänger:innen (1. Hochschulsemester)' |
                     input.bulas_balken_l_faecher == 'Studierende' |
                     input.bulas_balken_l_faecher == 'internationale Studierende' |
                     input.bulas_balken_l_faecher == 'Studienanfänger:innen (1. Hochschulsemester)'|
                     input.bulas_balken_l_faecher == 'Absolvent:innen' |
                     input.bulas_balken_l_faecher == 'internationale Absolvent:innen'",
                                      ns = ns,
                                      shinyWidgets::pickerInput(
                                        inputId = ns("bl_balken_alle_faecher"),

                                        choices = studi_det_ui_faecher(spezif_i =c('Internationale Studienanfänger:innen (1. Hochschulsemester)',
                                                                                   'Studierende',
                                                                                   'Internationale Studierende',
                                                                                   'Studienanfänger:innen (1. Hochschulsemester)',
                                                                                   "internationale Absolvent:innen",
                                                                                   "Absolvent:innen")),

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
                                        content = paste0("Die Übersicht zeigt, dass der Anteil von Studierenden in MINT an allen Studierenden zwischen den Bundesländern zwischen 23% (Thüringen) und 41% (Sachsen, Bayern) liegt."),
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
