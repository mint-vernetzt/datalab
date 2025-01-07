#' studium_studienzahl_bundeslandvergleich UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_studium_studienzahl_bundeslandvergleich_ui <- function(id){
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
      inputId = ns("ansicht_studium_bulas"),
      label = NULL,
      choices = c("Übersicht - Kartendiagramm", "Zeitverlauf - Liniendiagramm",
                  "Gruppenvergleich - Balkendiagramm"),
      selected = "Übersicht - Kartendiagramm"
    ),

    conditionalPanel(condition = "input.ansicht_studium_bulas == 'Übersicht - Kartendiagramm'",
                     ns = ns,

                     p("Jahr:"),
                     shinyWidgets::sliderTextInput(
                       inputId = ns("bulas_map_y"),
                       label = NULL,
                       choices = 2013:2023,
                       selected = 2023
                     ),

                     p("Studierendengruppen:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("bulas_map_l"),
                       choices = c("Studierende",
                                   "internationale Studierende",
                                   "Studierende (Lehramt)",
                                   "Studienanfänger:innen (1. Hochschulsemester)",
                                   "internationale Studienanfänger:innen (1. Hochschulsemester)",
                                   "Absolvent:innen",
                                   "internationale Absolvent:innen"),
                       selected = c("Studierende")
                       ,
                       multiple = F,
                       options =  list(
                         "max-options" = 2,
                         "max-options-text" = "<span style='color: red;'>Maximal 2 Gruppen auswählen</span>")
                     ),
                     br(),
                     darstellung(id="dh_studium_fach_h3"),
                     br(),
                     br(),
                     shinyBS::bsPopover(id="ih_studium_fach_2neu", title="",
                                        content = paste0("Die Karte in der ersten Einstellung zeigt: Während Sachsen mit über 41 % MINT-Studierende den höchsten MINT-Anteil im Bundeslandvergleich hat, studieren im Nachbarland Thüringen nur 23  % aller Studierenden ein MINT-Fach."),
                                        trigger = "hover"),
                     tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_studium_fach_2neu")
    ),

    conditionalPanel(condition = "input.ansicht_studium_bulas == 'Zeitverlauf - Liniendiagramm'",
                     ns = ns,
                     p("Jahre:"),
                     shinyWidgets::sliderTextInput(
                       inputId = ns("bulas_verlauf_y"),
                       label = NULL,
                       choices = 2013:2023,
                       selected = c(2017, 2023)
                     ),

                     p("Studierendengruppen:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("bulas_verlauf_l"),
                       choices = c("Studierende",
                                   "internationale Studierende",
                                   "Studierende (Lehramt)",
                                   "Studienanfänger:innen (1. Hochschulsemester)",
                                   "internationale Studienanfänger:innen (1. Hochschulsemester)",
                                   "Absolvent:innen",
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
                       inputId = ns("bulas_verlauf_regio"),
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
                       inputId = ns("bulas_verlauf_abs_rel"),
                       choices = c("In Prozent", "Anzahl"),
                       justified = TRUE,
                       checkIcon = list(yes = icon("ok",
                                                   lib = "glyphicon"))
                     ),
                     br(),
                     shinyBS::bsPopover(id="ih_studium_mint_5", title="",
                                        content = paste0("Die erste Ansicht zeigt, dass in Baden-Württemberg im Vergleich zu Hamburg ein größerer Anteil an Studierenden MINT-Fächer studiert. In beiden Bundesländern bleibt der Anteil an MINT-Studierenden über die Jahre relativ konstant."),
                                        placement = "top",
                                        trigger = "hover"),
                     tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_studium_mint_5")

    ),


    conditionalPanel(condition = "input.ansicht_studium_bulas == 'Gruppenvergleich - Balkendiagramm'",
                     ns = ns,

                     p("Jahr:"),
                     shinyWidgets::sliderTextInput(
                       inputId = ns("bulas_balken_date"),
                       label = NULL,
                       choices = 2013:2023,
                       selected = 2023
                     ),
                     p("Studierendengruppen:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("bulas_balken_l"),
                       choices = c("Studierende",
                                   "internationale Studierende",
                                   "Studierende (Lehramt)",
                                   "Studienanfänger:innen (1. Hochschulsemester)",
                                   "internationale Studienanfänger:innen (1. Hochschulsemester)",
                                   "Absolvent:innen",
                                   "internationale Absolvent:innen"),
                       selected = "Studierende"),


                     br(),
                     shinyBS::bsPopover(id="ih_studium_fach_5", title="",
                                        content = paste0("Die Übersicht zeigt, dass der Anteil von Studierenden in MINT an allen Studierenden zwischen den Bundesländern zwischen 24 % (Thüringen, Saarland) und 42 % (Sachsen) liegt."),
                                        placement = "top",
                                        trigger = "hover"),
                     tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_studium_fach_5")
    )
  )

}

#' studium_studienzahl_bundeslandvergleich Server Functions
#'
#' @noRd
mod_studium_studienzahl_bundeslandvergleich_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$ansicht_studium_bulas, {
      r$ansicht_studium_bulas <- input$ansicht_studium_bulas
    })

    observeEvent(input$bulas_map_y, {
      r$bulas_map_y <- input$bulas_map_y
    })

    observeEvent(input$bulas_map_l, {
      r$bulas_map_l <- input$bulas_map_l
    })

    observeEvent(input$bulas_verlauf_y, {
      r$bulas_verlauf_y <- input$bulas_verlauf_y
    })

    observeEvent(input$bulas_verlauf_l, {
      r$bulas_verlauf_l <- input$bulas_verlauf_l
    })

    observeEvent(input$bulas_verlauf_regio, {
      r$bulas_verlauf_regio <- input$bulas_verlauf_regio
    })

    observeEvent(input$bulas_verlauf_abs_rel, {
      r$bulas_verlauf_abs_rel <- input$bulas_verlauf_abs_rel
    })

    observeEvent(input$bulas_balken_date, {
      r$bulas_balken_date <- input$bulas_balken_date
    })

    observeEvent(input$bulas_balken_l, {
      r$bulas_balken_l <- input$bulas_balken_l
    })


  })
}

## To be copied in the UI
# mod_studium_studienzahl_bl_map_ui("studium_studienzahl_bl_map_1")

## To be copied in the server
# mod_studium_studienzahl_bl_map_server("studium_studienzahl_bl_map_1")
