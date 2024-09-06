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
    p("Jahr:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("choice_y"),
      label = NULL,
      choices = 2013:2022,
      selected = 2022
    ),
    p("Indikator:"),
    shinyWidgets::pickerInput(
      inputId = ns("choice_l"),
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
        "max-options-text" = "Maximal 3 Indikatoren auswählen")
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
    p("Kurswahl der Jungen als Vergleich anzeigen?", style = "color: #b16fab;"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("gegenwert_studi_gen"),
      choices = c("Ja", "Nein"),
      selected = "Nein",
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
    ),

    br(),
    shinyBS::bsPopover(id="ih_studium_mint_7", title="",
                       content = paste0("Von allen Frauen, die studieren, studieren nur 24 % ein MINT-Fach im Hauptfach. Unter männlichen Studierenden ist MINT und insbesondere Ingenieurwissenschaften verbreiteter: Etwas über die Hälfte aller studierender Männer studieren in MINT."),
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_studium_mint_7")
    # ,
    # p("Nur Lehramt anzeigen:"),
    # tags$div(
    #   shinyWidgets::materialSwitch(inputId = ns("nurLehramt_studium_choice_gender"), label = "Nein", inline = TRUE),
    #   tags$span("Ja"),
    #   p("Auswahl Hochschulform:"),
    #   conditionalPanel(condition = "input.nurLehramt_studium_choice_gender == false",
    #                    ns = ns,
    #                    shinyWidgets::pickerInput(
    #                      inputId = ns("hochschulform_studium_choice_gender1"),
    #                      choices = c("Alle Hochschulen"="insgesamt", "Universität" = "Uni", "Fachhochschule" = "FH")
    #                    )),
    #   conditionalPanel(condition = "input.nurLehramt_studium_choice_gender != false",
    #                    ns = ns,
    #                    shinyWidgets::pickerInput(
    #                      inputId = ns("hochschulform_studium_choice_gender2"),
    #                      choices = "Uni"
    #                    ))
    # ),
    # p("Status der Student:innen:"),
    # shinyWidgets::radioGroupButtons(
    #   inputId = ns("level_studium_choice_gender"),
    #   choices = c("Studienanfänger:innen"="Studienanfänger:innen", "Studierende"),
    #   direction = "vertical",
    #   justified = TRUE,
    #   checkIcon = list(yes = icon("ok",
    #                               lib = "glyphicon"))
    # )

  )
}

#' studium_choice_gender Server Functions
#'
#' @noRd
mod_studium_choice_gender_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

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


    # observeEvent(input$hochschulform_studium_choice_gender1, {
    #   r$hochschulform_studium_choice_gender1 <- input$hochschulform_studium_choice_gender1
    # })
    #
    # observeEvent(input$hochschulform_studium_choice_gender2, {
    #   r$hochschulform_studium_choice_gender2 <- input$hochschulform_studium_choice_gender2
    # })
    #
    # observeEvent(input$level_studium_choice_gender, {
    #   r$level_studium_choice_gender <- input$level_studium_choice_gender
    # })


  })
}

## To be copied in the UI
# mod_studium_choice_gender_ui("studium_choice_gender_1")

## To be copied in the server
# mod_studium_choice_gender_server("studium_choice_gender_1")
