#' studium_studienzahl_verlauf_bl_subject UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_studium_studienzahl_verlauf_bl_subject_ui <- function(id){
  ns <- NS(id)
  tagList(

    p("Jahre:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_verlauf_subject_bl"),
      label = NULL,
      choices = 2013:2022,
      selected = c(2016, 2022)
    ),
    # p("Nur Lehramt anzeigen:"),
    # tags$div(
    #   shinyWidgets::materialSwitch(inputId = ns("nurLehramt_studierende_verlauf_bl_subject"), label = "Nein", inline = TRUE),
    #   tags$span("Ja"),
    #   p("Auswahl der Hochschulform:"),
    #   conditionalPanel(condition = "input.nurLehramt_studierende_verlauf_bl_subject == false",
    #                    ns = ns,
    #                    shinyWidgets::pickerInput(
    #                      inputId = ns("hochschulform_studierende_verlauf_1"),
    #                      choices = c("Alle Hochschulen"="insgesamt", "Universität" = "Uni", "Fachhochschule" = "FH")
    #                    )),
    #   conditionalPanel(condition = "input.nurLehramt_studierende_verlauf_bl_subject != false",
    #                    ns = ns,
    #                    shinyWidgets::pickerInput(
    #                      inputId = ns("hochschulform_studierende_verlauf_2"),
    #                      choices = "Uni"
    #                    ))
    # ),
    p("Studierendengruppen:"),
    shinyWidgets::pickerInput(
      inputId = ns("verl_l"),
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
      selected = c("Studierende")
      ,
      multiple = F,
      options =  list(
        "max-options" = 2,
        "max-options-text" = "<span style='color: red;'>Maximal 2 Studierendengruppen auswählen</span>")
    ),
    # p("Auswahl der Fächer:"),
    # shinyWidgets::pickerInput(
    #   inputId = ns("subject_selected_bl"),
    #   choices = c("MINT-Fächer (gesamt)" = "MINT", "Mathematik/Naturwissenschaften" = "Mathematik/Naturwissenschaften",
    #               "Ingenieurwissenschaften" = "Ingenieurwissenschaften"),
    #   selected = c("MINT", "Ingenieurwissenschaften"),
    #   multiple = TRUE
    # ),
    p("Region:"),
    shinyWidgets::pickerInput(
      inputId = ns("states_verlauf_subject_bl"),
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
      selected = "Berlin"

    ),

    p("Darstellungsart:"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("abs_zahlen_verlauf_subject_bl"),
      choices = c("In Prozent", "Anzahl"),
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
    ),
    br(),
    shinyBS::bsPopover(id="ih_studium_mint_4", title="",
                       content = paste0("Die Grafik der ersten Einstellung zeigt, dass sich der Anteil an MINT-Studierenden in Berlin seit 2015 minimal (um 2 Prozentpunkte) verringert hat. Eine Betrachtung der Anzahl an MINT-Studierenden zeigt dagegen, dass es über die Jahre in Berlin stetig mehr Studierende in MINT gibt. Das weist darauf hin, dass die Studierendenzahlen in Berlin insgesamt steigen."),
                       placement = "top",
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_studium_mint_4")
  )
}

#' studium_studienzahl_verlauf_bl_subject Server Functions
#'
#' @noRd
mod_studium_studienzahl_verlauf_bl_subject_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$date_verlauf_subject_bl, {
      r$date_verlauf_subject_bl <- input$date_verlauf_subject_bl
    })

    observeEvent(input$verl_l, {
      r$verl_l <- input$verl_l
    })

    observeEvent(input$abs_zahlen_verlauf_subject_bl, {
      r$abs_zahlen_verlauf_subject_bl <- input$abs_zahlen_verlauf_subject_bl
    })
#
#     observeEvent(input$subject_selected_bl, {
#       r$subject_selected_bl <- input$subject_selected_bl
#     })

    observeEvent(input$states_verlauf_subject_bl, {
      r$states_verlauf_subject_bl <- input$states_verlauf_subject_bl
    })

    # observeEvent(input$nurLehramt_studierende_verlauf_bl_subject, {
    #   r$nurLehramt_studierende_verlauf_bl_subject <- input$nurLehramt_studierende_verlauf_bl_subject
    # })
    #
    # observeEvent(input$hochschulform_studierende_verlauf_1, {
    #   r$hochschulform_studierende_verlauf_1 <- input$hochschulform_studierende_verlauf_1
    # })
    #
    # observeEvent(input$hochschulform_studierende_verlauf_2, {
    #   r$hochschulform_studierende_verlauf_2 <- input$hochschulform_studierende_verlauf_2
    # })


  })
}

## To be copied in the UI
# mod_studium_studienzahl_verlauf_bl_subject_ui("studium_studienzahl_verlauf_bl_subject_1")

## To be copied in the server
# mod_studium_studienzahl_verlauf_bl_subject_server("studium_studienzahl_verlauf_bl_subject_1")
