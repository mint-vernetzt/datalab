#' studium_studienzahl_bl_map_gender UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_beruf_arbeitsmarkt_balken_entgelt_ui <- function(id){
  ns <- NS(id)
  tagList(

    p("Jahr:"),
    shinyWidgets::sliderTextInput(
      inputId = ns("date_balken_entgelt"),
      label = NULL,
      choices = 2013:2024,
      selected = 2024
    ),
    p("Region:"),
    shinyWidgets::pickerInput(
      inputId = ns("states_balken_entgelt"),
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
                  "Ostdeutschland (inkl. Berlin)"),
      selected = "Deutschland"
    ),

    p("Studierendengruppen:"),
    shinyWidgets::pickerInput(
      inputId = ns("status_balken_entgelt"),
      choices = c("Gesamt", "Fachkraft", "Spezialist", "Experte"
      ),
      selected = "Gesamt"
    ),
    p("Darstellungsart:"),
    shinyWidgets::pickerInput(
      inputId = ns("abs_zahlen_balken_entgelt"),
      choices = c("MINT-Berufe", "Insgesamt", "Mathematik, Naturwissenschaften", "Informatik", "Technik", "Landtechnik", "Produktionstechnik", "Bau-und Gebäudetechnik", "Keine MINT-Berufe", "Verkehrs-, Sicherheits- und Veranstaltungstechnik", "Gesundheitstechnik"),
      selected = c("MINT-Berufe", "Insgesamt", "Keine MINT-Berufe"),
      multiple = TRUE,
      options =  list(
        "max-options" = 4,
        "max-options-text" = "<span style='color: red;'>Maximal 4 Indikatoren auswählen</span>")

    ),
    br(),
    shinyBS::bsPopover(id="ih_studium_international_2_entgelt", title="",
                       content = paste0("D"),
                       placement = "top",
                       trigger = "hover"),
    tags$a(paste0("Interpretationshilfe zur Grafik"), icon("info-circle"), id="ih_studium_international_2_entgelt")
  )
}

#' studium_studienzahl_bl_map_gender Server Functions
#'
#' @noRd
mod_beruf_arbeitsmarkt_balken_entgelt_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns







    observeEvent(input$date_balken_entgelt, {
      r$date_balken_entgelt <- input$date_balken_entgelt
    })

    observeEvent(input$states_balken_entgelt, {
      r$states_balken_entgelt <- input$states_balken_entgelt
    })

    observeEvent(input$status_balken_entgelt, {
      r$status_balken_entgelt <- input$status_balken_entgelt
    })

    observeEvent(input$abs_zahlen_balken_entgelt, {
      r$abs_zahlen_balken_entgelt <- input$abs_zahlen_balken_entgelt
    })



  })
}

## To be copied in the UI
# mod_studium_studienzahl_bl_map_gender_ui("studium_studienzahl_bl_map_gender_1")

## To be copied in the server
# mod_studium_studienzahl_bl_map_gender_server("studium_studienzahl_bl_map_gender_1")
