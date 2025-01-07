#' studium_studienzahl_mint_fach_ui_1 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_studium_studienzahl_mint_fach_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Darstellungsart:"),
    shiny::radioButtons(
      inputId = ns("ansicht_mint_fach"),
      label = NULL,
      choices = c("Einzelansicht - Kuchendiagramm", "Gruppenvergleich - Balkendiagramm"),
      selected = "Einzelansicht - Kuchendiagramm"
    ),

    p("Jahr:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("jahr_mint_fach"),
      label = NULL,
      choices = 2013:2023,
      selected = 2023
    ),

    p("Region:"),
    shinyWidgets::pickerInput(
      inputId = ns("region_mint_fach"),
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
    p("Fächer-Ebene:"),
    shinyWidgets::pickerInput(
      inputId = ns("ebene_mint_fach"),
      choices = c("MINT-Fachbereiche", "MINT-Fächergruppen"),
      selected = "MINT-Fachbereiche"
    ),
    conditionalPanel(condition = "input.ansicht_mint_fach ==
                     'Einzelansicht - Kuchendiagramm'",
                     ns = ns,

      p("Studierendengruppen:"),
      shinyWidgets::pickerInput(
        inputId = ns("gruppe_mint_fach_pies"),
        choices = c("Studierende",
                    "Studierende (Lehramt)",
                    "internationale Studierende",
                    "Studienanfänger:innen (1. Hochschulsemester)",
                    "internationale Studienanfänger:innen (1. Hochschulsemester)",
                    "Absolvent:innen",
                    "internationale Absolvent:innen"),
        selected = c("Studierende"),
        multiple = FALSE
      ),
      br(),
      shinyBS::bsPopover(id="dh_studium_mint_fach_1", title = "",
                                content = paste0("Falls die Grafiken abgeschnitten dargestellt werden, bitte das gesamte Ansichtsfenster einmal verkleinern und dann wieder maximieren. Dann stellt sich das Seitenverhältnis des Desktops richtig ein."),
                                trigger = "hover", placement = "top"),
      tags$a(paste0("Probleme bei der Darstellung"), icon("question-circle"), id = "dh_studium_mint_fach_1"),
      br(),
      br(),
      shinyBS::bsPopover(id="ih_studium_mint_fach_2", title="",
                         content = paste0("Die erste Darstellung zeigt: Zusammen gerechnet knapp 37 % der Studierenden belegen ein MINT-Fach. Der Großteil davon (26 %) studiert eine Ingenieurwissenschaft, wozu auch ein Informatikstudium zählt."),
                         trigger = "hover", placement = "top"),
      tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_studium_mint_fach_2")
    ),

    conditionalPanel(condition = "input.ansicht_mint_fach ==
                     'Gruppenvergleich - Balkendiagramm'",
                     ns = ns,
                     p("Studierendengruppen:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("gruppe_mint_fach_balken"),

                       choices =c("Studierende",
                                  "Studierende (Lehramt)",
                                  "internationale Studierende",
                                  "Studienanfänger:innen (1. Hochschulsemester)",
                                  "internationale Studienanfänger:innen (1. Hochschulsemester)",
                                  "Absolvent:innen",
                                  "internationale Absolvent:innen"),
                       selected = c("Studierende"),
                       multiple = FALSE
                     ),
                     br(),
                     shinyBS::bsPopover(id="ih_studium_mint_fach_3", title="",
                                        content = paste0("Die Darstellung zeigt, wie groß der Anteil Studierender in einzelnen MINT-Fächern an allen Studierenden ist. In der ersten Einstellung sieht man beispielsweise, 2023 studieren in Deutschland 128.000 Personen (25,5 %) eine Ingenieurwissenschaft."),
                                        placement = "top",
                                        trigger = "hover"),
                     tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_studium_mint_fach_3")
    )
  )

}

#' mod_studium_studienzahl_mint_fach_server Server Functions
#'
#' @noRd
mod_studium_studienzahl_mint_fach_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$ansicht_mint_fach, {
      r$ansicht_mint_fach <- input$ansicht_mint_fach
    })

    observeEvent(input$jahr_mint_fach, {
      r$jahr_mint_fach <- input$jahr_mint_fach
    })

    observeEvent(input$region_mint_fach, {
      r$region_mint_fach <- input$region_mint_fach
    })

    observeEvent(input$ebene_mint_fach, {
      r$ebene_mint_fach <- input$ebene_mint_fach
    })

    observeEvent(input$gruppe_mint_fach_pies, {
      r$gruppe_mint_fach_pies <- input$gruppe_mint_fach_pies
    })

    observeEvent(input$gruppe_mint_fach_balken, {
      r$gruppe_mint_fach_balken <- input$gruppe_mint_fach_balken
    })


  })
}

## To be copied in the UI
# mod_studium_studienzahl_choice_1_ui("studium_studienzahl_choice_1_1")

## To be copied in the server
# mod_studium_studienzahl_choice_1_server("studium_studienzahl_choice_1_1")
