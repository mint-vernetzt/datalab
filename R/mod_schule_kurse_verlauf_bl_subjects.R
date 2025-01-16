#' schule_kurse_verlauf_bl_subjects UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_schule_kurse_verlauf_bl_subjects_ui <- function(id){
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

    p("Jahre:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_kurse_verlauf_subject_bl"),
      label = NULL,
      choices = 2013:2023,
      selected = c(2017, 2023)
    ),
    p("Kursart:"),
    shinyWidgets::pickerInput(
      inputId = ns("topic_selected_subject_bl"),
      choices = c("Grundkurse", "Leistungskurse"),
      selected = "Grundkurse"
    ),

    p("Region:"),
    shinyWidgets::pickerInput(
      inputId = ns("states_kurse_verlauf_subject_bl"),
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
      selected = "Deutschland"
    ),

    p("Fächer/Fächergruppen:"),
    shinyWidgets::pickerInput(
      inputId = ns("subject_selected_bl_sub"),
      choices = c("MINT-Fächer (gesamt)","Mathematik", "Informatik", "Physik", "Chemie",
                  "Biologie", "andere naturwiss.-technische Fächer", "Deutsch", "Fremdsprachen",
                  "Gesellschaftswissenschaften",
                  "Musik/Kunst", "Religion/Ethik", "Sport"),
      selected = c("Mathematik", "Informatik", "Physik", "Chemie",
                   "Biologie"),
      options = list(`actions-box` = TRUE,
                     `deselect-all-text` = "Alle abwählen",
                     `select-all-text` = "Alle auswählen"),
      multiple = TRUE
    ),
    p("Darstellungsart:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("abs_zahlen_kurse_verlauf_subject_bl"),
      choices = c("In Prozent", "Anzahl"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
    ),
    br(),
    shinyBS::bsPopover(id="ih_schule_fach_3", title="",
                       content = paste0("In dieser Grafik ist bei den voreingestellten Kategorien ist z. B. zu sehen, dass im Jahr 2023 die Mathematik mit 8 % den größten Anteil an den Grundkursbelegungen hat."),
                       placement = "top",
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_schule_fach_3"),

    br(),
    br(),
    darstellung(id="popover_darstellung2"),
    br()
  )

}

#' schule_kurse_verlauf_bl_subjects Server Functions
#'
#' @noRd
mod_schule_kurse_verlauf_bl_subjects_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$abs_zahlen_kurse_verlauf_subject_bl, {
      r$abs_zahlen_kurse_verlauf_subject_bl <- input$abs_zahlen_kurse_verlauf_subject_bl
    })

    observeEvent(input$subject_selected_bl_sub, {
      r$subject_selected_bl_sub <- input$subject_selected_bl_sub
    })

    observeEvent(input$date_kurse_verlauf_subject_bl, {
      r$date_kurse_verlauf_subject_bl <- input$date_kurse_verlauf_subject_bl
    })

    observeEvent(input$states_kurse_verlauf_subject_bl, {
      r$states_kurse_verlauf_subject_bl <- input$states_kurse_verlauf_subject_bl
    })

    observeEvent(input$topic_selected_subject_bl, {
      r$topic_selected_subject_bl <- input$topic_selected_subject_bl
    })

  })
}

## To be copied in the UI
# mod_schule_kurse_verlauf_bl_subjects_ui("schule_kurse_verlauf_bl_subjects_1")

## To be copied in the server
# mod_schule_kurse_verlauf_bl_subjects_server("schule_kurse_verlauf_bl_subjects_1")
