#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_studium_studienzahl_international_bundeslandvergleich_ui <- function(id){ ###DIESE SEITE IST NICHT INTERNATIONAL GRADE
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
      inputId = ns("ansicht_studium_international_bulas"),
      label = NULL,
      choices = c("Übersicht - Kartendiagramm", "Zeitverlauf - Liniendiagramm",
                  "Gruppenvergleich - Balkendiagramm"),
      selected = "Übersicht - Kartendiagramm"
    ),

    conditionalPanel(condition = "input.ansicht_studium_international_bulas == 'Übersicht - Kartendiagramm'",
                     ns = ns,

                     p("Jahr:"),
                     shinyWidgets::sliderTextInput(
                       inputId = ns("international_bulas_map_y"),
                       label = NULL,
                       choices = 2013:2023,
                       selected = 2023
                     ),

                     p("Studierendengruppen:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("international_bulas_map_l"),
                       choices = c("internationale Studierende",
                                   "internationale Studienanfänger:innen (1. Hochschulsemester)",
                                   "internationale Absolvent:innen"),
                       selected = c("Studierende")
                       ,
                       multiple = F,
                       options =  list(
                         "max-options" = 2,
                         "max-options-text" = "<span style='color: red;'>Maximal 2 Gruppen auswählen</span>")
                     ),
                     br(),
                     shinyBS::bsPopover(id="international_dh_studium_fach_2", title = "",
                                        content = paste0("Falls die Grafiken abgeschnitten dargestellt werden, bitte das gesamte Ansichtsfenster einmal verkleinern und dann wieder maximieren. Dann stellt sich das Seitenverhältnis des Desktops richtig ein."),
                                        placement = "top",
                                        trigger = "hover"),
                     tags$a(paste0("Probleme bei der Darstellung"), icon("question-circle"), id = "international_dh_studium_fach_2"),
                     br(),
                     br(),
                     shinyBS::bsPopover(id="international_ih_studium_fach_2neu", title="",
                                        content = paste0("Die Darstellung zeigt, wie hoch der MINT-Anteil unter internationalen Studierenden in MINT ist. In Sachsen studierenden bspw. besonders viele Internationals MINT (63%)."),
                                        trigger = "hover"),
                     tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="international_ih_studium_fach_2neu")
    ),

    conditionalPanel(condition = "input.ansicht_studium_international_bulas == 'Zeitverlauf - Liniendiagramm'",
                     ns = ns,
                     p("Jahre:"),
                     shinyWidgets::sliderTextInput(
                       inputId = ns("international_bulas_verlauf_y"),
                       label = NULL,
                       choices = 2013:2023,
                       selected = c(2017, 2023)
                     ),

                     p("Studierendengruppen:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("international_bulas_verlauf_l"),
                       choices = c("internationale Studierende",
                                   "internationale Studienanfänger:innen (1. Hochschulsemester)",
                                   "internationale Absolvent:innen"),
                       selected = c("Studierende")
                       ,
                       multiple = F,
                       options =  list(
                         "max-options" = 2,
                         "max-options-text" = "<span style='color: red;'>Maximal 2 Gruppen auswählen</span>")
                     ),
                     p("Regionen:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("international_bulas_verlauf_regio"),
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
                       inputId = ns("international_bulas_verlauf_abs_rel"),
                       choices = c("In Prozent", "Anzahl"),
                       justified = TRUE,
                       checkIcon = list(yes = icon("ok",
                                                   lib = "glyphicon"))
                     ),
                     br(),
                     shinyBS::bsPopover(id="international_ih_studium_mint_5", title="",
                                        content = paste0("Der Zeitverlauf lässt den MINT-Anteil unter internationalen Studierenden in einzelnen Bundesländern über die Zeit betrachten. Hier sieht man z. B., dass MINT bei Internationals in Baden-Würrtemberg beliebter ist als in Hamburg."),
                                        placement = "top",
                                        trigger = "hover"),
                     tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="international_ih_studium_mint_5")

    ),


    conditionalPanel(condition = "input.ansicht_studium_international_bulas == 'Gruppenvergleich - Balkendiagramm'",
                     ns = ns,

                     p("Jahr:"),
                     shinyWidgets::sliderTextInput(
                       inputId = ns("international_bulas_balken_date"),
                       label = NULL,
                       choices = 2013:2023,
                       selected = 2023
                     ),
                     p("Studierendengruppen:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("international_bulas_balken_l"),
                       choices = c("internationale Studierende",
                                   "internationale Studienanfänger:innen (1. Hochschulsemester)",
                                   "internationale Absolvent:innen"),
                       selected = "Studierende"),


                     br(),
                     shinyBS::bsPopover(id="international_ih_studium_fach_5", title="",
                                        content = paste0("In dieser Übersicht sieht man, dass der MINT-Anteil unter internationalen Studierenden in Sachsen und Mecklenburg-Vorpommern besonders hoch ist."),
                                        placement = "top",
                                        trigger = "hover"),
                     tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="international_ih_studium_fach_5")
    )
  )

}

#' studium_studienzahl_bundeslandvergleich Server Functions
#'
#' @noRd
mod_studium_studienzahl_international_bundeslandvergleich_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$ansicht_studium_international_bulas, {
      r$ansicht_studium_international_bulas <- input$ansicht_studium_international_bulas
    })

    observeEvent(input$international_bulas_map_y, {
      r$international_bulas_map_y <- input$international_bulas_map_y
    })

    observeEvent(input$international_bulas_map_l, {
      r$international_bulas_map_l <- input$international_bulas_map_l
    })

    observeEvent(input$international_bulas_verlauf_y, {
      r$international_bulas_verlauf_y <- input$international_bulas_verlauf_y
    })

    observeEvent(input$international_bulas_verlauf_l, {
      r$international_bulas_verlauf_l <- input$international_bulas_verlauf_l
    })

    observeEvent(input$international_bulas_verlauf_regio, {
      r$international_bulas_verlauf_regio <- input$international_bulas_verlauf_regio
    })

    observeEvent(input$international_bulas_verlauf_abs_rel, {
      r$international_bulas_verlauf_abs_rel <- input$international_bulas_verlauf_abs_rel
    })

    observeEvent(input$international_bulas_balken_date, {
      r$international_bulas_balken_date <- input$international_bulas_balken_date
    })

    observeEvent(input$international_bulas_balken_l, {
      r$international_bulas_balken_l <- input$international_bulas_balken_l
    })


  })
}
