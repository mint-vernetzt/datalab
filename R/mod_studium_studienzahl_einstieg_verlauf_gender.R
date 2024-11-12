#' studium_studienzahl_einstieg_verlauf_gender UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_studium_studienzahl_einstieg_verlauf_gender_ui <- function(id){
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
      inputId = ns("genz_date"),
      label = NULL,
      choices = 2013:2023,
      selected = c(2015, 2023)
    ),
    # p("Nur Lehramt anzeigen:"),
    # tags$div(
    #   shinyWidgets::materialSwitch(inputId = ns("nurLehramt_studierende_einstieg_verlauf_gender"), label = "Nein", inline = TRUE),
    #   tags$span("Ja")
    # ),
    p("Studierendengruppen:"),
    shinyWidgets::pickerInput(
      inputId = ns("genzl"),
      choices = c(
        "Studierende",
        "Studierende (Lehramt)",
        "Studienanfänger:innen (1. Hochschulsemester)",
        "Absolvent:innen"
      ),
      selected = c("Studierende",
                   "Absolvent:innen"
      ),
      multiple = TRUE,
      options = list(`actions-box` = TRUE,
                     `deselect-all-text` = "Alle abwählen",
                     `select-all-text` = "Alle auswählen")
    ),
    p("Region:"),
    shinyWidgets::pickerInput(
      inputId = ns("gen_z_region"),
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

    p("Fach/Fächergruppe:"),
    shinyWidgets::pickerInput(
      inputId = ns("gen_z_faecher"),
      choices = studi_det_ui_faecher(),
      selected = "Alle MINT-Fächer",
      multiple = FALSE),

    # p("Auswahl der Hochschulform:"),
    # conditionalPanel(condition = "input.nurLehramt_studierende_einstieg_verlauf_gender == false",
    #                  ns = ns,
    #                  shinyWidgets::pickerInput(
    #                    inputId = ns("hochschulform_studierende_einstieg_verlauf_gender_1"),
    #                    choices = c("Alle Hochschulen"="insgesamt", "Universität" = "Uni", "Fachhochschule" = "FH")
    #                  )),
    # conditionalPanel(condition = "input.nurLehramt_studierende_einstieg_verlauf_gender != false",
    #                  ns = ns,
    #                  shinyWidgets::pickerInput(
    #                    inputId = ns("hochschulform_studierende_einstieg_verlauf_gender_2"),
    #                    choices = "Uni"
    #                  ))

    p("Darstellungsart:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("abs_zahlen"),
      choices = c("In Prozent", "Anzahl"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
    ),

    br(),
    shinyBS::bsPopover(id="ih_studium_frauen_2", title="",
                       content = paste0("Die erste Einstellung der interaktiven Grafik zeigt, dass sowohl unter Studienanfänger:innen als auch Studierenden der Frauenanteil über die Jahre gewachsen ist. Waren 2015 deutschlandweit nur 29 % der Studierenden in MINT Frauen, sind es 2021 32 %."),
                       placement = "top",
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_studium_frauen_2")
  )

}

#' studium_studienzahl_einstieg_verlauf_gender Server Functions
#'
#' @noRd
mod_studium_studienzahl_einstieg_verlauf_gender_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$genzl, {
      r$genzl <- input$genzl
    })

    observeEvent(input$genz_date, {
      r$genz_date <- input$genz_date
    })

    observeEvent(input$abs_zahlen, {
      r$abs_zahlen <- input$abs_zahlen
    })

    observeEvent(input$gen_z_faecher, {
      r$gen_z_faecher <- input$gen_z_faecher
    })

    observeEvent(input$gen_z_region, {
      r$gen_z_region <- input$gen_z_region
    })

    # observeEvent(input$hochschulform_studierende_einstieg_verlauf_gender_1, {
    #   r$hochschulform_studierende_einstieg_verlauf_gender_1 <- input$hochschulform_studierende_einstieg_verlauf_gender_1
    # })
    #
    #
    # observeEvent(input$hochschulform_studierende_einstieg_verlauf_gender_2, {
    #   r$hochschulform_studierende_einstieg_verlauf_gender_2 <- input$hochschulform_studierende_einstieg_verlauf_gender_2
    # })

  })
}

## To be copied in the UI
# mod_studium_studienzahl_einstieg_verlauf_gender_ui("studium_studienzahl_einstieg_verlauf_gender_1")

## To be copied in the server
# mod_studium_studienzahl_einstieg_verlauf_gender_server("studium_studienzahl_einstieg_verlauf_gender_1")
