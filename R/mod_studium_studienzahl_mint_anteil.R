#' studium_studienzahl_test UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_studium_studienzahl_anteil_ui <- function(id){
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
      inputId = ns("ansicht_studium_anteil"),
      label = NULL,
      choices = c("Einzelansicht - Kuchendiagramm", "Gruppenvergleich - Balkendiagramm"),
      selected = "Einzelansicht - Kuchendiagramm"
    ),
    p("Jahr:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("studium_anteil_y"),
      label = NULL,
      choices = 2013:2022,
      selected = 2022
    ),
    p("Region:"),
    shinyWidgets::pickerInput(
      inputId = ns("region_studium_anteil"),
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

    conditionalPanel(condition = "input.ansicht_studium_anteil ==
                     'Einzelansicht - Kuchendiagramm'",
                      ns = ns,

                     p("Studierendengruppen (max. 2):"),
                     shinyWidgets::pickerInput(
                       inputId = ns("studium_anteil_i"),
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
                       selected = c("Studierende"
                                    , "Studienanfänger:innen (1.Fachsemester)"
                       ),
                       multiple = TRUE,
                       options =  list(
                         "max-options" = 2,
                         "max-options-text" = "Maximal 2 Studierendengruppen auswählen")
                     ),
                     br(),
                     shinyBS::bsPopover(id="dh_studium_mint_1", title = "",
                                        content = paste0("Falls die Grafiken abgeschnitten dargestellt werden, bitte das gesamte Ansichtsfenster einmal verkleinern und dann wieder maximieren. Dann stellt sich das Seitenverhältnis des Desktops richtig ein."),
                                        trigger = "hover"),
                     tags$a(paste0("Probleme bei der Darstellung"), icon("question-circle"), id = "dh_studium_mint_1"),
                     br(),
                     br(),
                     shinyBS::bsPopover(id="ih_studium_mint_1", title="",
                                        content = paste0("In der ersten Einstellung ist zu sehen, dass in Deutschland 38 % der Studienanfänger:innen (1. FS) ein MINT-Fach wählen, bei den Studierenden ist dieser Anteil mit 37 % in 2021 etwas geringer, was bedeutet, dass die Abbruchsquote in MINT höher liegt als in anderen Fachbereichen."),
                                        trigger = "hover"),
                     tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_studium_mint_1")

    ),

    conditionalPanel(condition = "input.ansicht_studium_anteil ==
                     'Gruppenvergleich - Balkendiagramm'",
                     ns = ns,
                     p("Studierendengruppen (mehrere auswählbar):"),
                     shinyWidgets::pickerInput(
                       inputId = ns("studium_anteil_i_balken"),
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
                       multiple = TRUE,
                       options = list(`actions-box` = TRUE,
                                      `deselect-all-text` = "Alle abwählen",
                                      `select-all-text` = "Alle auswählen"),
                       # selected = c("Studienanfänger:innen (1.Fachsemester)",
                       #              "Studierende",
                       #              "Studierende (Lehramt, Universität)",
                       #              "Studierende (Universität)",
                       #              "Studierende (Fachhochschulen)"
                       # )
                       selected = c("Studienanfänger:innen (1.Fachsemester)",
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
                       )


                     ),
                     br(),
                     shinyBS::bsPopover(id="ih_studium_mint_6", title="",
                                        content = paste0("Über die Studierendengruppen hinweg liegt der Anteil an MINT-Studierenden in Deutschland 2021 zwischen 35 % - 39 %. Die einzige Ausnahme hierbei sind Lehramtstudierende: Weniger als ein Drittel der Lehramtstudierenden deutschlandweit belegen ein MINT-Fach als Hauptfach."),
                                        trigger = "hover"),
                     tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_studium_mint_6")

    )

    )

}

#moment
# p("Regionen:"),
# shinyWidgets::pickerInput(
#   inputId = ns("bulas_balken_regio_faecher"),
#   choices = c("Deutschland",
#               "Baden-Württemberg",
#               "Bayern",
#               "Berlin",
#               "Brandenburg",
#               "Bremen",
#               "Hamburg",
#               "Hessen",
#               "Mecklenburg-Vorpommern",
#               "Niedersachsen",
#               "Nordrhein-Westfalen",
#               "Rheinland-Pfalz",
#               "Saarland",
#               "Sachsen",
#               "Sachsen-Anhalt",
#               "Schleswig-Holstein",
#               "Thüringen"
#               ,
#               "Westdeutschland (o. Berlin)",
#               "Ostdeutschland (inkl. Berlin)"),
#   selected = c("Deutschland",
#                "Baden-Württemberg",
#                "Bayern",
#                "Berlin",
#                "Brandenburg",
#                "Bremen",
#                "Hamburg",
#                "Hessen",
#                "Mecklenburg-Vorpommern",
#                "Niedersachsen",
#                "Nordrhein-Westfalen",
#                "Rheinland-Pfalz",
#                "Saarland",
#                "Sachsen",
#                "Sachsen-Anhalt",
#                "Schleswig-Holstein",
#                "Thüringen"
#                ,
#                "Westdeutschland (o. Berlin)",
#                "Ostdeutschland (inkl. Berlin)"),
#   options = list(`actions-box` = TRUE,
#                  `deselect-all-text` = "Alle abwählen",
#                  `select-all-text` = "Alle auswählen"),
#   multiple = TRUE
# ),








#' studium_choice_gender Server Functions
#'
#' @noRd
mod_studium_studienzahl_anteil_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$ansicht_studium_anteil, {
      r$ansicht_studium_anteil<- input$ansicht_studium_anteil
    })

    observeEvent(input$studium_anteil_y, {
      r$studium_anteil_y <- input$studium_anteil_y
    })

    observeEvent(input$region_studium_anteil , {
      r$region_studium_anteil  <- input$region_studium_anteil
    })

    observeEvent(input$studium_anteil_i, {
      r$studium_anteil_i <- input$studium_anteil_i
    })

    observeEvent(input$studium_anteil_i_balken, {
      r$studium_anteil_i_balken <- input$studium_anteil_i_balken
    })

  })
}

## To be copied in the UI
# mod_studium_choice_gender_ui("studium_choice_gender_1")

## To be copied in the server
# mod_studium_choice_gender_server("studium_choice_gender_1")
