#' studium_choice_gender UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_studium_choice_gender_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Darstellungsart:"),
    shiny::radioButtons(
      inputId = ns("ansicht_studi_gen_wahl"),
      label = NULL,
      choices = c("Einzelansicht - Kuchendiagramm", "Zeitverlauf - Liniendiagramm"),
      selected = "Einzelansicht - Kuchendiagramm"
    ),

    conditionalPanel(condition = "input.ansicht_studi_gen_wahl == 'Einzelansicht - Kuchendiagramm'",
                     ns = ns,
    p("Jahr:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("choice_y"),
      label = NULL,
      choices = 2013:2023,
      selected = 2023
    ),
    p("Studierendengruppe:"),
    shinyWidgets::pickerInput(
      inputId = ns("choice_l"),
      choices = c(
        "Studierende",
         "Studierende (Lehramt)",
         "Studienanfänger:innen (1. Hochschulsemester)",
         "Absolvent:innen"
      ),
      selected = c("Studierende")
      ,
      multiple = F,
      options =  list(
        "max-options" = 2,
        "max-options-text" = "<span style='color: red;'>Maximal 2 Studierendengruppen auswählen</span>")
    ),
    p("Region:"),
    shinyWidgets::pickerInput(
      inputId = ns("region_studi_gen"),
      choices = c("Deutschland",
                  "Westdeutschland (o. Berlin)",
                  "Ostdeutschland (inkl. Berlin)",
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
      ),
      multiple = F,
      selected = "Deutschland"
    ),
    p("Kurswahl der Männer als Vergleich anzeigen?", style = "color: #b16fab;"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("gegenwert_studi_gen"),
      choices = c("Ja", "Nein"),
      selected = "Ja",
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
    ),

    br(),
    shinyBS::bsPopover(id="ih_studium_mint_7", title="",
                       content = paste0("Von allen Frauen, die studieren, studieren 23,4 % ein MINT-Fach. Unter männlichen Studierenden ist MINT und insbesondere Ingenieurwissenschaften verbreiteter: Etwas über die Hälfte studieren MINT."),
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_studium_mint_7")

    ),

    conditionalPanel(condition = "input.ansicht_studi_gen_wahl == 'Zeitverlauf - Liniendiagramm'",
                     ns = ns,
                     p("Jahre:"),
                     shinyWidgets::sliderTextInput(
                       inputId = ns("choice_V_y"),
                       label = NULL,
                       choices = 2013:2023,
                       selected = c(2017,2023)
                     ),
                     p("Studierendengruppen:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("choice_l_v"),
                       choices = c(
                         "Studierende",
                         "Studierende (Lehramt)",
                         "Studienanfänger:innen (1. Hochschulsemester)",
                         "Absolvent:innen"
                       ),
                       selected = c("Studierende", "Studienanfänger:innen (1. Hochschulsemester)")
                       ,
                       multiple = T,
                       options =  list(
                         "max-options-text" = "Maximal 2 Studierendengruppen auswählen")
                       # options = list(`actions-box` = TRUE,
                       #                `deselect-all-text` = "Alle abwählen",
                       #                `select-all-text` = "Alle auswählen")
                     ),
                     p("Fächergruppe:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("choice_v_f"),

                       choices = c("Alle MINT-Fächer","Mathematik, Naturwissenschaften", "Ingenieurwissenschaften (inkl. Informatik)"),

                       selected = "Alle MINT-Fächer"
                     ),
                     p("Region:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("choice_states"),
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
                     p("Darstellungsart:"),
                     shinyWidgets::radioGroupButtons(
                       inputId = ns("abs_zahlen_l_v"),
                       choices = c("In Prozent", "Anzahl"),
                       justified = TRUE,
                       checkIcon = list(yes = icon("ok",
                                                   lib = "glyphicon"))
                     ),
                     br(),
                     shinyBS::bsPopover(id="ih_studium_mint_8", title="",
                                        content = paste0("Die erste Einstellung zeigt u.A., dass der Anteil an Frauen, die sich für ein MINT-Studium entscheiden sowohl unter den Studienanfänger:innen und Studierenden ähnlich verläuft."),
                                        placement = "top",
                                        trigger = "hover"),
                     tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_studium_mint_8")
    )
  )
}

#' studium_choice_gender Server Functions
#'
#' @noRd
mod_studium_choice_gender_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$ansicht_studi_gen_wahl, {
      r$ansicht_studi_gen_wahl <- input$ansicht_studi_gen_wahl
    })

    observeEvent(input$choice_y, {
      r$choice_y <- input$choice_y
    })

    observeEvent(input$choice_l, {
      r$choice_l <- input$choice_l
    })

    observeEvent(input$region_studi_gen, {
      r$region_studi_gen <- input$region_studi_gen
    })

    observeEvent(input$gegenwert_studi_gen, {
      r$gegenwert_studi_gen <- input$gegenwert_studi_gen
    })


    observeEvent(input$choice_states, {
      r$choice_states <- input$choice_states
    })

    observeEvent(input$choice_v_f, {
      r$choice_v_f <- input$choice_v_f
    })

    observeEvent(input$abs_zahlen_l_v, {
      r$abs_zahlen_l_v <- input$abs_zahlen_l_v
    })

    observeEvent(input$choice_l_v, {
      r$choice_l_v <- input$choice_l_v
    })

    observeEvent(input$choice_V_y, {
      r$choice_V_y <- input$choice_V_y
    })

    observeEvent(input$subject_verlauf_bl_subject_gender, {
      r$subject_verlauf_bl_subject_gender <- input$subject_verlauf_bl_subject_gender
    })

    observeEvent(input$states_verlauf_bl_subject_gender, {
      r$states_verlauf_bl_subject_gender <- input$states_verlauf_bl_subject_gender
    })



  })
}

## To be copied in the UI
# mod_studium_choice_gender_ui("studium_choice_gender_1")

## To be copied in the server
# mod_studium_choice_gender_server("studium_choice_gender_1")
