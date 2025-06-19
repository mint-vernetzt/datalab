#' studium_studienzahl_mintfrauen_1 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_studium_studienzahl_mintfrauen_ui <- function(id){
  ns <- NS(id)
  tagList(
    # p("Darstellungsart:"),
    # shiny::radioButtons(
    #   inputId = ns("ansicht_mint_fach_frauen"),
    #   label = NULL,
    #   choices = c("Gruppenvergleich - Balkendiagramm","Einzelansicht - Kuchendiagramm"),
    #   selected = "Gruppenvergleich - Balkendiagramm"
    # ),

    p("Jahr:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("jahr_mint_fach_frauen"),
      label = NULL,
      choices = 2013:2023,
      selected = 2023
    ),

    p("Region:"),
    shinyWidgets::pickerInput(
      inputId = ns("region_mint_fach_frauen"),
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
    # p("Fächerebene:"),
    # shinyWidgets::pickerInput(
    #   inputId = ns("ebene_mint_fach_frauen"),
    #   choices = c("MINT-Fachbereiche", "MINT-Fächergruppen"),
    #   selected = "MINT-Fachbereiche"
    # ),
    p("Bereich:"),
    shinyWidgets::pickerInput(
      inputId =  ns("ebene_mint_fach_frauen"),
      choices = c("MINT", "Nicht MINT"),
      selected = "MINT"
    ),

                     p("Studierendengruppen:"),
                     shinyWidgets::pickerInput(
                       inputId = ns("gruppe_mint_fach_balken_frauen"),

                       choices =c("weibliche Studierende",
                                  "weibliche Studierende (Lehramt)",
                                  "weibliche Studienanfänger:innen (1. Hochschulsemester)",
                                  "weibliche Absolvent:innen"),
                       selected = c("weibliche Studierende"),
                       multiple = FALSE
                     ),
                     br(),
                     shinyBS::bsPopover(id="ih_studium_mint_fach_3_frauen", title="",
                                        content = paste0("Die Darstellung zeigt, wie groß der Anteil Studierender in einzelnen MINT-Fächern an allen Studierenden ist. In der ersten Einstellung sieht man beispielsweise, 2023 studieren in Deutschland 128.000 Personen (25,5 %) eine Ingenieurwissenschaft."),
                                        placement = "top",
                                        trigger = "hover"),
                     tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_studium_mint_fach_3_frauen")
    )

}

#' mod_studium_studienzahl_mint_fach_server Server Functions
#'
#' @noRd
mod_studium_studienzahl_mintfrauen_server <- function(id, r){
  moduleServer( id, function(input, output, session){

    # observeEvent(input$ansicht_mint_fach_frauen, {
    #   r$ansicht_mint_fach_frauen <- input$ansicht_mint_fach_frauen
    # })

    observeEvent(input$jahr_mint_fach_frauen, {
      r$jahr_mint_fach_frauen <- input$jahr_mint_fach_frauen
    })

    observeEvent(input$region_mint_fach_frauen, {
      r$region_mint_fach_frauen <- input$region_mint_fach_frauen
    })

    observeEvent(input$ebene_mint_fach_frauen, {
      r$ebene_mint_fach_frauen <- input$ebene_mint_fach_frauen
    })

    # observeEvent(input$gruppe_mint_fach_pies_frauen, {
    #   r$gruppe_mint_fach_pies_frauen <- input$gruppe_mint_fach_pies_frauen
    # })

    observeEvent(input$gruppe_mint_fach_balken_frauen, {
      r$gruppe_mint_fach_balken_frauen <- input$gruppe_mint_fach_balken_frauen
    })


  })
}


