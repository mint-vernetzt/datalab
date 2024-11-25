#' home_start_einstieg_gender UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_home_start_einstieg_gender_ui <- function(id){
  ns <- NS(id)
  tagList(
    p("Darstellungsart:"),
    shiny::radioButtons(
      inputId = ns("ansicht_start_comparison_mint_gender"),
      label = NULL,
      choices = c("Einzelansicht - Kuchendiagramm", "Gruppenvergleich - Balkendiagramm"),
      selected = "Einzelansicht - Kuchendiagramm"
    ),

    p("Jahr:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_start_comparison_mint_gender"),
      label = NULL,
      choices = 2013:2023,
      selected = 2023
    ),

    p("Region:"),
    shinyWidgets::pickerInput(
      inputId = ns("regio_start_comparison_gender"),
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

    conditionalPanel("input.ansicht_start_comparison_mint_gender == 'Einzelansicht - Kuchendiagramm'",
                     ns = ns,
             p("Bereiche (max. 2):"),
             shinyWidgets::pickerInput(
               inputId = ns("indikator_start_einstieg_1_gender"),
               choices = c("Schüler:innen Leistungskurse" = "Leistungskurse",
                           "Studierende",
                           "Auszubildende", "Beschäftigte"),
               selected = c("Beschäftigte", "Studierende"),
               multiple = TRUE,
               options =  list(
                 "max-options" = 2,
                 "max-options-text" = "<span style='color: red;'>Bitte nur maximal 2 Bereiche auswählen</span>"
               )
             )
             ),

    br(),
    p("Nicht-MINT als Vergleich anzeigen?", style = "color: #b16fab;"),
    shinyWidgets::radioGroupButtons(
      inputId = ns("gegenwert_start_comparison_gender"),
      choices = c("Ja", "Nein"),
      selected = "Nein",
      justified = TRUE,
      checkIcon = list(yes = icon("ok",
                                  lib = "glyphicon"))
      ),

      conditionalPanel("input.ansicht_start_comparison_mint_gender == 'Einzelansicht - Kuchendiagramm'",
                       ns = ns,
            br(),
            shinyBS::bsPopover(id="dh_alle_frauen_1", title = "",
                               content = paste0("Falls die Grafiken abgeschnitten dargestellt werden, bitte das gesamte Ansichtsfenster einmal verkleinern und dann wieder maximieren. Dann stellt sich das Seitenverhältnis des Desktops richtig ein."),
                               trigger = "hover"),
            tags$a(paste0("Probleme bei der Darstellung"), icon("question-circle"), id = "dh_alle_frauen_1"),
            br(),
            br(),
            shinyBS::bsPopover(id="ih_alle_frauen_1", title="",
                               content = paste0("Betrachtet man hier beispielsweise die MINT-Beschäftigten, sieht man, dass deutschlandweit im Jahr 2021 17 % der MINT-Beschäftigten Frauen sind. Betrachtet man dagegen alle Berufsgruppen außer MINT zusammen, machen Frauen sogar die Mehrheit der Beschäftigten aus (55 %)."),
                               trigger = "hover"),
            tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_alle_frauen_1")

  ),

  conditionalPanel("input.ansicht_start_comparison_mint_gender == 'Gruppenvergleich - Balkendiagramm'",
             br(),
             shinyBS::bsPopover(id="ih_alle_frauen_3", title="",
                                content = paste0("Der Frauenanteil nimmt entlang der Bildungskette ab. Der Anteil von Mädchen, die in der Schule einen MINT-Leistungskurs belegen, liegt noch bei fast der Hälfte (48 %). In MINT-Studiengängen ist der Frauenanteil mit ca. einem Drittel bereits geringer. In Ausbildung und Beruf sind nur noch 13 % bzw. 17 % der Beschäftigte weiblich."),
                                trigger = "hover"),
             tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_alle_frauen_3")
  )
  )

}

#' home_start_einstieg_gender Server Functions
#'
#' @noRd
mod_home_start_einstieg_gender_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    observeEvent(input$ansicht_start_comparison_mint_gender, {
      r$ansicht_start_comparison_mint_gender <- input$ansicht_start_comparison_mint_gender
    })

    observeEvent(input$regio_start_comparison_gender, {
      r$regio_start_comparison_gender <- input$regio_start_comparison_gender
    })

    observeEvent(input$date_start_comparison_mint_gender, {
      r$date_start_comparison_mint_gender <- input$date_start_comparison_mint_gender
    })

    observeEvent(input$indikator_start_einstieg_1_gender, {
      r$indikator_start_einstieg_1_gender <- input$indikator_start_einstieg_1_gender
    })

    observeEvent(input$gegenwert_start_comparison_gender, {
      r$gegenwert_start_comparison_gender <- input$gegenwert_start_comparison_gender
    })

  })
}

## To be copied in the UI
# mod_home_start_einstieg_gender_ui("home_start_einstieg_gender_1")

## To be copied in the server
# mod_home_start_einstieg_gender_server("home_start_einstieg_gender_1")
