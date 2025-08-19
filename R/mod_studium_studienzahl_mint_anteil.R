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
      choices = c("Gruppenvergleich - Balkendiagramm","Einzelansicht - Kuchendiagramm"),
      selected = "Gruppenvergleich - Balkendiagramm"
    ),
    p("Jahr:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("studium_anteil_y"),
      label = NULL,
      choices = 2013:2023,
      selected = 2023
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
                       choices = c("Studierende",
                                   "Studienanfänger:innen (1. Hochschulsemester)",
                                   "Absolvent:innen",
                                   "internationale Studierende",
                                   "internationale Studienanfänger:innen (1. Hochschulsemester)",
                                   "internationale Absolvent:innen",
                                   "Studierende im Lehramt" = "Studierende (Lehramt)",
                                   "Absolvent:innen im Lehramt" ="Absolvent:innen (Lehramt)"),
                       selected = c("Studierende"),
                       multiple = TRUE,
                       options =  list(
                         "max-options" = 2,
                         "max-options-text" = "<span style='color: red;'>Maximal 2 Studierendengruppen auswählen</span>")
                     ),
                     br(),

                     darstellung(id="dh_studium_mint_1"),
                     br(),
                     br(),
                     shinyBS::bsPopover(id="ih_studium_mint_1", title="",
                                        content = paste0("In der ersten Einstellung ist zu sehen, dass in Deutschland im Jahr 2023 rund 37 % der Studierenden ein MINT-Fach belegen. Blickt man dazu auf die Studierendengruppe der Absolvent:innen sieht man: Der MINT-Anteil hier ist mit 36 % etwas geringer, was auf Studienabbrüche in den MINT-Fächern hindeutet."),
                                        trigger = "hover"),
                     tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_studium_mint_1")

    ),

    conditionalPanel(condition = "input.ansicht_studium_anteil ==
                     'Gruppenvergleich - Balkendiagramm'",
                     ns = ns,
                     p("Studierendengruppen (mehrere auswählbar):"),
                     shinyWidgets::pickerInput(
                       inputId = ns("studium_anteil_i_balken"),
                       choices = c("Studierende",
                                   "Studienanfänger:innen (1. Hochschulsemester)",
                                   "Absolvent:innen",
                                   "internationale Studierende",
                                   "internationale Studienanfänger:innen (1. Hochschulsemester)",
                                   "internationale Absolvent:innen",
                                   "Studierende im Lehramt" = "Studierende (Lehramt)",
                                   "Absolvent:innen im Lehramt" ="Absolvent:innen (Lehramt)"),
                       multiple = TRUE,
                       options = list(`actions-box` = TRUE,
                                      `deselect-all-text` = "Alle abwählen",
                                      `select-all-text` = "Alle auswählen"),
                       selected = c("Studierende",
                                   "Studienanfänger:innen (1. Hochschulsemester)",
                                   "Absolvent:innen",
                                   "internationale Studierende",
                                   "internationale Studienanfänger:innen (1. Hochschulsemester)",
                                   "internationale Absolvent:innen",
                                   "Studierende im Lehramt" = "Studierende (Lehramt)",
                                   "Absolvent:innen im Lehramt" ="Absolvent:innen (Lehramt)"),
                     ),

                     p("Darstellungsart:"),
                     shinyWidgets::radioGroupButtons(
                       inputId = ns("abs_zahlen_arbeitsmarkt_einstieg_vergleich123"),
                       choices = c("In Prozent", "Anzahl"),
                       justified = TRUE,
                       checkIcon = list(yes = icon("ok",
                                                   lib = "glyphicon"))
                     ),
                     br(),
                     shinyBS::bsPopover(id="ih_studium_mint_6", title="",
                                        content = paste0("Der MINT-Anteil variiert zwischen den Studierendengruppen. MINT wird von internationalen Studierenden besonders oft belegt (47 % - 54 %). Dagegen belegt nur etwas weniger als ein viertel der Lehramstudierenden ein MINT-Fach als Hauptfach."),
                                        trigger = "hover"),
                     tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_studium_mint_6")

    )

    )

}




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

    observeEvent(input$abs_zahlen_arbeitsmarkt_einstieg_vergleich123, {
      r$abs_zahlen_arbeitsmarkt_einstieg_vergleich123 <- input$abs_zahlen_arbeitsmarkt_einstieg_vergleich123
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
