#' studium_studienzahl_einstieg_verlauf UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
# mod_studium_studienzahl_einstieg_verlauf_ui <- function(id){
#   ns <- NS(id)
#   tagList(
#
#     p("Jahre:"),
#     shinyWidgets::sliderTextInput(
#       inputId = ns("date_studienzahl_einstieg_verlauf"),
#       label = NULL,
#       choices = 2013:2022,
#       selected = c(2015,2022)
#     ),
#     p("Region:"),
#     shinyWidgets::pickerInput(
#       inputId = ns("region_studienzahl_einstieg_verlauf"),
#       choices = c("Deutschland",
#                   "Baden-Württemberg",
#                   "Bayern",
#                   "Berlin",
#                   "Brandenburg",
#                   "Bremen",
#                   "Hamburg",
#                   "Hessen",
#                   "Mecklenburg-Vorpommern",
#                   "Niedersachsen",
#                   "Nordrhein-Westfalen",
#                   "Rheinland-Pfalz",
#                   "Saarland",
#                   "Sachsen",
#                   "Sachsen-Anhalt",
#                   "Schleswig-Holstein",
#                   "Thüringen",
#                   "Westdeutschland (o. Berlin)",
#                   "Ostdeutschland (inkl. Berlin)"
#       ),
#       multiple = FALSE,
#       selected = c("Deutschland")
#     ),
#     p("Studierendengruppen (alle auswählbar): "),
#     shinyWidgets::pickerInput(
#       inputId = ns("studienzahl_einstieg_verlauf_indi"),
#       choices = c("Studienanfänger:innen (1.Fachsemester)",
#                   "Studienanfänger:innen (1.Hochschulsemester)",
#                   "Studienanfänger:innen (Fachhochschulen, 1.Fachsemester)",
#                   "Studienanfänger:innen (Fachhochschulen, 1.Hochschulsemester)",
#                   "Studienanfänger:innen (Lehramt, Universität, 1.Fachsemester)",
#                   "Studienanfänger:innen (Lehramt, Universität, 1.Hochschulsemester)",
#                   "Studienanfänger:innen (Universität, 1.Fachsemester)",
#                   "Studienanfänger:innen (Universität, 1.Hochschulsemester)",
#                   "Studierende",
#                   "Studierende (Fachhochschulen)",
#                   "Studierende (Lehramt, Universität)",
#                   "Studierende (Universität)"
#       ),
#       selected = c("Studierende"
#                    , "Studienanfänger:innen (1.Fachsemester)"
#       ),
#       multiple = TRUE,
#       # options =  list(
#       #   "max-options-text" = "Maximal 3 Indikatoren auswählen")
#       options = list(`actions-box` = TRUE,
#                      `deselect-all-text` = "Alle abwählen",
#                      `select-all-text` = "Alle auswählen")
#     ),
#     p("Darstellungsart:"),
#     shinyWidgets::radioGroupButtons(
#       inputId = ns("abs_zahlen_einstieg_verlauf_indi"),
#       choices = c("In Prozent", "Anzahl"),
#       justified = TRUE,
#       checkIcon = list(yes = icon("ok",
#                                   lib = "glyphicon"))
#     ),
#     br(),
#     shinyBS::bsPopover(id="ih_studium_mint_3", title="",
#                        content = paste0("Der Zeitverlauf zeigt, dass der Anteil von MINT-Studierenden an allen Studierenden sowie die absolute Anzahl der Studierenden in MINT bis zuletzt über die Jahre relativ konstant bleibt. (Großes Augenmerk muss hier auf die Y-Skala gelegt werden. Die visuellen Sprünge sind kleiner als sie aussehen.)"),
#                        trigger = "hover"),
#     tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_studium_mint_3")
#     )
#
#
# }


mod_studium_studienzahl_einstieg_verlauf_ui <- function(id){
  ns <- NS(id)
  tagList(
    # Hier füge ich das CSS für die Buttons "Alle auswählen" und "Alle abwählen" hinzu
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

    p("Jahre:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_studienzahl_einstieg_verlauf"),
      label = NULL,
      choices = 2013:2023,
      selected = c(2016,2023)
    ),

    p("Region:"),
    shinyWidgets::pickerInput(
      inputId = ns("region_studienzahl_einstieg_verlauf"),
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

    p("Studierendengruppen (alle auswählbar): "),
    shinyWidgets::pickerInput(
      inputId = ns("studienzahl_einstieg_indi_verlauf"),
      choices = c("Studierende",
                  "internationale Studierende",
                  "Studierende (Lehramt)",
                  "Studienanfänger:innen (1. Hochschulsemester)",
                  "internationale Studienanfänger:innen (1. Hochschulsemester)",
                  "Absolvent:innen",
                  "internationale Absolvent:innen"),
      selected = c("Studierende"
                   , "Studienanfänger:innen (1. Hochschulsemester)"
      ),
      multiple = TRUE,
      options = list(`actions-box` = TRUE,  # Die Auswahlbox für "Alle auswählen" und "Alle abwählen"
                     `deselect-all-text` = "Alle abwählen",  # Text für "Alle abwählen"
                     `select-all-text` = "Alle auswählen"   # Text für "Alle auswählen"
      )
    ),

    p("Darstellungsart:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("abs_zahlen_einstieg_verlauf_indi"),
      choices = c("In Prozent", "Anzahl"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok", lib = "glyphicon"))
    ),

    br(),

    shinyBS::bsPopover(id="ih_studium_mint_3", title="",
                       content = paste0("Der Zeitverlauf in erster Einstellung zeigt, dass der Anteil von MINT-Studierenden an allen Studierenden zwischen 2016-2023 leicht gesunken ist. Großes Augenmerk muss hier auf die Prozente-Skala links gelegt werden. Die visuellen Sprünge in den Zeitverläufen sind ggf. kleiner als sie aussehen."),
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_studium_mint_3")
  )
}





#' studium_studienzahl_einstieg_verlauf Server Functions
#'
#' @noRd
mod_studium_studienzahl_einstieg_verlauf_server <- function(id, r){
  moduleServer( id, function(input, output, session){


    observeEvent(input$date_studienzahl_einstieg_verlauf, {
      r$date_studienzahl_einstieg_verlauf <- input$date_studienzahl_einstieg_verlauf
    })

    observeEvent(input$region_studienzahl_einstieg_verlauf, {
      r$region_studienzahl_einstieg_verlauf <- input$region_studienzahl_einstieg_verlauf
    })

    observeEvent(input$studienzahl_einstieg_indi_verlauf, {
      r$studienzahl_einstieg_indi_verlauf <- input$studienzahl_einstieg_indi_verlauf
    })

    observeEvent(input$abs_zahlen_einstieg_verlauf_indi, {
      r$abs_zahlen_einstieg_verlauf_indi <- input$abs_zahlen_einstieg_verlauf_indi
    })


    observeEvent(input$abs_zahlen1, {
      r$abs_zahlen1 <- input$abs_zahlen1
    })


    observeEvent(input$abs_zahlen2, {
      r$abs_zahlen2 <- input$abs_zahlen2
    })

  })
}

## To be copied in the UI
# mod_studium_studienzahl_einstieg_verlauf_ui("studium_studienzahl_einstieg_verlauf_1")

## To be copied in the server
# mod_studium_studienzahl_einstieg_verlauf_server("studium_studienzahl_einstieg_verlauf_1")
